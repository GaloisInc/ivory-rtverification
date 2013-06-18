-- | Parses C files (after pre-proccesing them) for use in generating
-- setters/getters in Template Haskell.  The user should give on the command
-- line arguments of the form
--
-- compiler incls srcs
--
-- where compiler is the pre-processor (expected to end in "gcc"), includes
-- (expected to begin with "-I'), and sources (expected to end with ".c").  They
-- can be given in any order.

module Ivory.RTVerification.ParseVars
  ( getVarList
  , getVarListV
  ) where

import Data.List
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

import Ivory.RTVerification.Types

import qualified Parsimony as P
import qualified Parsimony.Char as P

--------------------------------------------------------------------------------

type Err a = Either String a

type Parsers a = P.Parser String a

--------------------------------------------------------------------------------

-- Parser definitions

skipWhiteSpace :: Parsers ()
skipWhiteSpace = P.skipMany (P.char ' ')

toType :: String -> Type
toType ty =
  case ty of
    "int"      -> Int
    "int32_t"  -> Int
    "long int" -> LongInt
    "float"    -> Float
    "double"   -> Double
    "char"     -> Char
    _          -> error $ "Type " ++ ty ++ " not implemented in parser."

typeParser :: Parsers Type
typeParser = do
  skipWhiteSpace

  tyStr <- P.choice [ tyP "int32_t"
                    , tyP "int"
                    , tyP "long int"
                    , tyP "float"
                    , tyP "double"
                    , tyP "char"
                    ]
  P.string tyStr
  return (toType tyStr)
  where tyP tStr = P.lookAhead (P.string tStr) >> return tStr

-- parse __attribute__((instrument(
numParser :: Parsers Int
numParser = do
  skipWhiteSpace
  strWS "__attribute__"
  paren
  paren
  strWS "instrument"
  paren
  num <- P.many1 P.digit
  return (read num)

  where paren = P.char '(' >> skipWhiteSpace
        strWS str = P.string str >> skipWhiteSpace

-- Drop rest of line
dropRestParser :: Parsers ()
dropRestParser = P.skip P.anyToken

-- Parse variable
varParser :: Parsers String
varParser = do
  skipWhiteSpace
  -- Overly general for C vars
  var <- P.manyTill P.anyChar (P.char ' ')
  return var

-- The full parser.
declParser :: Parsers VarId
declParser = do
  t <- typeParser
  v <- varParser
  i <- numParser
  dropRestParser
  return (VarId t (toInteger i) v)

parse :: String -> Either P.ParseError VarId
parse = P.parse declParser

--------------------------------------------------------------------------------

-- Are indexes unique and are they greater or equal to 0?
validateVarLst :: [VarId] -> Err [VarId]
validateVarLst vs' =
  let vs = sortBy (\x y -> idx x `compare` idx y) vs' in
  if and $ snd $ mapAccumL go (-1) (map idx vs)
    then Right vs
    else Left "Repeated or bad index for vars"
  where go acc v = (v, acc < v)

--------------------------------------------------------------------------------

-- | Return the path to the file containing instrumented declarations.
--
-- If the environment variable 'RTV_DECLS' is set, its value is
-- returned.  Otherwise, return the file 'instrumented-decls' in
-- the current directory.
dataFileName :: IO FilePath
dataFileName = do
  file <- lookupEnv "RTV_DECLS"
  case file of
    Just s  -> return s
    Nothing -> fmap (</> "instrumented-decls") getCurrentDirectory

getDataFile :: IO [String]
getDataFile = do
  fp <- dataFileName
  b  <- doesFileExist fp
  if b
    then return . lines =<< readFile fp
    else error (err fp)
  where
  err file =
       "Error: expected a file at\n  "
    ++ file
    ++ "\ncontaining the instrumented declarations.\n"
    ++ "  Use 'find-instrumented.sh' included in the rtv-lib package "
    ++ "to generate this."

--------------------------------------------------------------------------------

parseAndVerify :: [String] -> Either String [VarId]
parseAndVerify decls =
  case mapM parse decls of
    Left err  -> Left (show err)
    Right res -> validateVarLst res

--------------------------------------------------------------------------------

-- | Parse the variables from all C sources in scope using find-instrumented.sh.
-- Generates an assignment e.g., "variables = [Int, Char]"
getVarList :: IO [Type]
getVarList = getVarListV False

-- | Optional verbose output
getVarListV :: Bool -> IO [Type]
getVarListV verbose = do
  decls <- getDataFile
  puts $ "---------------------------"
  puts $ "FOUND INSTRUMENTED VARS:"
  mapM_ puts decls
  puts $ "---------------------------"
  case parseAndVerify decls of
    Left err  -> error $ "Parse failed: " ++ err
    Right vs' -> return (map vType vs')
  where
  puts = if verbose then putStrLn else const (return ())

--------------------------------------------------------------------------------
