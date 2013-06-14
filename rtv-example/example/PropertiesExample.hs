{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.RTVerification.GenSettersGetters
import Ivory.RTVerification.Operators
import Ivory.RTVerification.GenChecker

-- get_v, hist_len_v is defined for variables in variables
gettersAndHists

checksMod :: Module
checksMod = createModule $ properties $ do
  -- Historically: has the property always held?
  historically [get_id0,get_id1] (\[a,b] -> a <=? b)

  -- Other example kinds of properties:

{-
  -- Previously: did the property hold in the previous state?
  previous
    [get_id0,get_id1]
    [hist_len_id1,hist_len_id1]
    (\[a,b] -> a  >? b)
  -- Since: since the first proposition held, has the second held?
  since
    [get_id0,get_id1]
    (\[a,_]  -> a >? 100)
    (\[_,b] -> b ==? 1)
-}

main :: IO ()
main = do
  writeCFilesForVariables "out"
  runCompiler [checksMod] initialOpts { includeDir = "out", srcDir = "out" }

{- Generate the C files out/instrumented{c,h} with setters and getters for and
  int var0 and and a double var1. The following functions will be generated:

      void* read_from_history(int var_id);
      void append_to_history(int var_id, void* value);
      int get_id0(int);
      void append_to_hist_0(int);
      int hist_len_id0(void);
      float get_id1(int);
      void append_to_hist_1(float);
      int hist_len_id1(void);

Also generate the property specifications.
-}
