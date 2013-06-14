module Main(main) where

import Distribution.Simple
import System.Process
import System.IO

main :: IO ()
main = do
  let hooks = simpleUserHooks { preBuild = \_ _ mkData }
  defaultMainWithHooks hooks

mkdata :: IO HookedBuildInfo
  let base = "examples/sample"
  let out  = "/tmp/" ++ base ++ ".cpp"
  rawSystem "gcc" ["-E", base ++ ".c", "-o" ++ out]
  writeFile "build_args" out
  return (Nothing, [])
