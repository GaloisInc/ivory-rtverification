{-# Language QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- Generates C instrumentation code for the list 'variables' of (name, type)
   tuples. The files 'out/instrumented.{c,h}' are generated. -}

module Ivory.RTVerification.GenSettersGetters
  ( gettersAndHists
  , writeCFilesForVariables
  ) where

import Prelude hiding (exp)
import Control.Monad (forM_)
import System.FilePath.Posix ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import System.IO

import Text.PrettyPrint.Mainland hiding ((</>), char, string)

import Language.C.Quote.GCC
import Language.C.Syntax hiding (Type, Exp)
import qualified Language.C.Syntax as S

import Language.Haskell.TH hiding (Type, ppr)
import qualified Language.Haskell.TH as T

import Ivory.Language hiding (proc)
import Ivory.RTVerification.ParseVars
import Ivory.RTVerification.Types

--------------------------------------------------------------------------------

outFile :: String
outFile = "instrumented"

--------------------------------------------------------------------------------
-- Template Haskell to generate setters and getters for variables.

gettersAndHists :: Q [Dec]
gettersAndHists = do
  tyLst <- runIO getVarList
  mapM getterAndHist (toIdxs tyLst) >>= return . concat

getterAndHist :: (String,Type) -> Q [Dec]
getterAndHist (var,ty) = do
  let getVar = "get_" ++ var
  let hist   = "hist_len_" ++ var
  getterNm  <- newName getVar
  histNm    <- newName hist
  -- Make the type signatures
  getType   <- [t| Def ('[Sint32] :-> $(ivoryType ty)) |]
  histType  <- [t| Def ('[] :-> Sint32) |]

  -- Make the definitions
  getCall   <- [e| importProc getVar outFile |]
  histCall  <- [e| importProc hist   outFile |]

  return [ SigD getterNm getType
         , FunD getterNm [clauseExp getCall]
         , SigD histNm histType
         , FunD histNm   [clauseExp histCall]
         ]

clauseExp :: Exp -> Clause
clauseExp exp = Clause [] ( NormalB exp ) []

ivoryType :: Type -> Q T.Type
ivoryType ty = case ty of
  Int        -> [t| Sint32  |]
  LongInt    -> [t| Sint64  |]
  Double     -> [t| IDouble |]
  Float      -> [t| IFloat  |]
  Char       -> [t| IChar   |]

--------------------------------------------------------------------------------
-- getter, setter, etc. functions

histStruct :: String -> S.Type -> Int -> Definition
histStruct varName varType histLen =
  let arr  = [cty|$ty:varType[$uint:histLen]|]
      fs   = [ [csdecl| $ty:arr hist;|]
             , [csdecl| int idx;|]
             , [csdecl| int len;|]]
   in [cedecl|struct { $sdecls:fs } $id:varName; |]

getter :: String -> S.Type -> Int -> Func
getter varName varType histLen= [cfun|
  $ty:varType $id:("get_" ++ varName)(int t)
  {
    return $id:varName.hist[($id:varName.idx-t-1)%$uint:histLen];
  }
  |]

setter :: String -> S.Type -> Int -> Func
setter varName varType histLen = [cfun|
  void $id:("append_to_hist_" ++ varName)($ty:varType new_val)
  {
    $id:varName.hist[$id:varName.idx] = new_val;
    $id:varName.idx = ($id:varName.idx+1)%$uint:histLen;
    if ($id:varName.len < $uint:histLen) {
      ++$id:varName.len;
    }
  }
  |]

getHistLen :: String -> Func
getHistLen varName = [cfun|
  int $id:("hist_len_" ++ varName)(void)
  {
    return $id:varName.len;
  }
  |]

settergetters :: [(String, b)] -> [InitGroup]
settergetters vars = [ [cdecl|void* (*get_fns[])(int) = {$inits:gids};|]
                     , [cdecl|void  (*set_fns[])(void*) = {$inits:sids};|] ]
  where
  gids = map (stringToInitializer . ("get_"++) . fst) vars
  sids = map (stringToInitializer . ("append_to_hist_"++) . fst) vars

publicSetter :: Func
publicSetter = [cfun|
  void append_to_history(int var_id, void* value)
  {
    set_fns[var_id](value);
  }
  |]

publicGetter :: Func
publicGetter = [cfun|
  void *read_from_history(int var_id, int t)
  {
    return get_fns[var_id](t);
  }
  |]

toType :: Type -> S.Type
toType s = case s of
                Int        -> [cty| int     |]
                LongInt    -> [cty| long int|]
                Double     -> [cty| double  |]
                Float      -> [cty| float   |]
                Char       -> [cty| char    |]

stringToInitializer :: String -> Initializer
stringToInitializer s = [cinit|$id:s|]

toIdxs :: [Type] -> [(String, Type)]
toIdxs = zip (map (("id" ++) . show) [0::Int,1..])

--------------------------------------------------------------------------------

-- | Generate instrumented.c and instrumented.h
writeCFilesForVariables :: FilePath -> IO ()
writeCFilesForVariables outDir = do
  tyList <- getVarList
  let variables = toIdxs tyList
  createDirectoryIfMissing True outDir
  -- write a C file with functions for setting/getting temporal state for the
  -- instrumented variables
  withFile (outDir </> outFile <.> "c") WriteMode $ \cFile -> do
    let prt c = hPrint cFile (ppr c)
    forM_ variables $ \(varName,varType) -> do
      prt $ histStruct varName (toType varType) 100
      prt $ getter varName     (toType varType) 100
      prt $ setter varName     (toType varType) 100
      prt $ getHistLen varName

    -- create arrays of function pointers to the getters/setters and populate
    -- them
    mapM_ (hPutStrLn cFile . (++";") . show . ppr) (settergetters variables)

    -- create the two public functions: append_to_history and read_from_history
    prt publicSetter
    prt publicGetter

  -- write a header file with the exposed functions
  withFile (outDir </> outFile <.> "h") WriteMode $ \cFile -> do
    let prt c = hPrint cFile (ppr c)
    prt $ [cedecl|$esc:("#ifndef __INSTRUMENTED_H_INCLUDED__")|]
    prt $ [cedecl|$esc:("#define __INSTRUMENTED_H_INCLUDED__")|]
    prt $ [cedecl|void *read_from_history(int var_id);|]
    prt $ [cedecl|void append_to_history(int var_id, void* value);|]
    forM_ variables $ \(varName,varType) -> do
      prt $ [cedecl|$ty:(toType varType) $id:("get_" ++ varName)(int);|]
      prt $ [cedecl|void $id:("append_to_hist_" ++ varName)
                         ($ty:(toType varType));|]
      prt $ [cedecl|int $id:("hist_len_" ++ varName)(void);|]
    prt $ [cedecl|$esc:("#endif")|]

--------------------------------------------------------------------------------
