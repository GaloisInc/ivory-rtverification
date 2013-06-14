{-# Language DataKinds #-}
{-# Language TypeOperators #-}

module Ivory.RTVerification.GenChecker
  ( generateChecker
  , createModule
  ) where

import Ivory.Language

-- | Takes a list of C functions taking no arguments and returning a Boolean
-- and returns true if all the functions return true.
generateChecker :: [Def ('[] :-> IBool)] -> Def ('[] :-> IBool)
generateChecker fs = proc "check_properties" $ body $ do
  -- collect the results of calling each property check ...
  results <- mapM call fs
  -- ... then return true only if all checks returned true
  ret $ foldl1 (.&&) results

-- | Generates an Ivory.Language.Module from the given list of function
-- definitions from a call to 'Ivory.RTVerification.Operators.properties':
--
-- > createModule $ properties $ do
-- >     ...
createModule :: ([Def ('[] :-> IBool)],[MemArea (Stored IBool)]) -> Module
createModule checks  = package "runtime-checker" $ do
  public  $ incl . generateChecker . fst $ checks
  private $ do
    inclHeader "instrumented.h"
    mapM_ incl . fst $ checks
    mapM_ defMemArea . snd $ checks
