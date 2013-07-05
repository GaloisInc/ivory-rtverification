{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.RTVerification.Operators
  ( historically
  , previous
  , since
  , properties
  , Getter
  , HistLen
  ) where

import Ivory.Language
import MonadLib (StateT, StateM, Id, get, set, runM)
import MonadLib.Derive (Iso (..),derive_get,derive_set)

-- | A getter takes an integer t' and returns the value at time t-t'.
type Getter a = Def ('[Sint32] :-> a)

-- | HistLen returns the length of the recorded history for a variable
type HistLen = Def ('[] :-> Sint32)

-- | A prediate takes a list of values and returns a Boolean.
type Predicate a = [a] -> IBool

-- checkers are a list of functions returning a Boolean and a list of memory
-- areas used by these functions. `nextId` is used to generate unique names
-- for memory areas.
data Checkers = Checkers
  { globals   :: [MemArea (Stored IBool)]
  , functions :: [Body IBool]
  , nextId    :: Int
  }

newtype CheckersState a = CheckersState
  { unSt :: StateT Checkers Id a
  } deriving (Functor, Monad)

instance StateM CheckersState Checkers  where
  get = derive_get (Iso CheckersState unSt)
  set = derive_set (Iso CheckersState unSt)

-- | Takes a list of 'Getter's and a predicate and returns a checker function
-- that checks that this has been true in all observed states.
historically :: (IvoryVar a) => [Getter a] -> Predicate a -> CheckersState ()
historically getters predicate = do
  propsState <- get
  let prevResId = "historically_prev" ++ (show . nextId) propsState
  let prev_res = area prevResId (Just (ival true)) :: MemArea (Stored IBool)
  let test = body $ do
        currVals <- mapM (flip call 0) getters
        let prev = addrOf prev_res
        prev' <- deref prev
        -- if the previous test failed, return false right away;
        -- otherwise, store the result of this check and return
        -- its result
        flip (ifte_ prev') (ret false) $ do
          let res = predicate currVals
          store prev res
          ret prev'
  set (propsState
    { functions = test: functions propsState
    , globals   = prev_res: globals propsState
    , nextId    = succ $ nextId propsState
    })

-- | Takes a list of 'Getter's and a predicate and generates a checker function
-- that checks that the predicate was true at time t-1.
previous :: (IvoryVar a)
  => [Getter a] -> [Def ('[] :-> Sint32)] -> Predicate a -> CheckersState ()
previous getters histlens predicate = do
  let test = body $ do
       -- check that we actually have a previous state (i.e. that histories are
       -- of length > 1). If this is the case, return the result of the
       -- predicates, if not; return true.
       histLens <- mapM call histlens
       ifte_ (foldr (\a b -> a >? 1 .&& b) true histLens)
         (ret . predicate =<< mapM (flip call 1) getters)
         (ret true)
  fs <- get
  set (fs { functions = test: functions fs })

-- | Takes a list of 'Getter's and two predicates and generates a checker
-- function that checks that 'pred2' has been true since 'pred1' first became
-- true. The generated function will return true if 'pred1' has never been
-- true.
since :: (IvoryVar a) =>
  [Getter a] -> Predicate a -> Predicate a -> CheckersState ()
since getters pred1 pred2 = do
  propsState <- get
  let anteStatusId = "since_pred1_status" ++ (show . nextId) propsState
  let ante_res = area anteStatusId (Just (ival false)) :: MemArea (Stored IBool)
  let test = body $ do
        currVals <- mapM (flip call 0) getters
        let ante = addrOf ante_res
        ante' <- deref ante
        -- if the first predicate is false, we can return true right away.
        -- Since it has never been true (prior to this state). If it's true, we
        -- need to record that fact and from here on return the result of the
        -- second predicate.
        ifte_ ante'
          (ret $ pred2 currVals)
          (store ante (pred1 currVals) >> ret (ante' ? (pred2 currVals,false)))
  set (propsState
    { functions = test: functions propsState
    , globals   = ante_res: globals propsState
    , nextId    = succ $ nextId propsState
    })

-- | Generates a pair of a list of function definitions and a list of
-- 'Ivory.Language.MemArea's from the given properties:
--
-- > properties $ do
-- >   historically [get_var0] (\[x] -> x /=? 0)
-- >   historically [get_var1] (\[x] -> x >=? 100)
properties ::
  CheckersState a -> ([Def ('[] :-> IBool)],[MemArea (Stored IBool)])
properties props = (funs, areas)
  where
  propNames   = map (\n -> "prop" ++ show n) [1 :: Int ..]
  definitions = snd $ runM (unSt props) $ Checkers [] [] 0
  funs        = zipWith proc propNames (functions definitions)
  areas       = globals definitions

