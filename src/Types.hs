module Types (Address(..), toString) where

import Data.List (intercalate)

data Address = Address Int Int Int Int
  deriving Show

toString :: Address -> String
toString (Address a b c d) = intercalate "." $ map show [a, b, c, d]
