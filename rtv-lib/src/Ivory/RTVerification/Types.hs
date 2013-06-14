module Ivory.RTVerification.Types where

-- | Types we support runtime monitoring over.
data Type = Int
          | LongInt
          | Double
          | Float
          | Char
  deriving (Eq, Show, Read)

data VarId = VarId
  { vType  :: Type
  , idx    :: Integer
  , varStr :: String
  } deriving (Show, Read)
