{-# LANGUAGE OverloadedStrings #-}

module Rules (Address(..), findAbusiveAddress, findBoringAddress, toString) where

import Prelude hiding                   (take)
import Control.Applicative              ((<|>))
import Data.List                        (intercalate)
import Data.Attoparsec.ByteString       (Parser, manyTill, parseOnly, string, take)
import Data.Attoparsec.ByteString.Char8 (anyChar, char, decimal, skipSpace)
import Data.ByteString                  (ByteString)

data Address = Address Int Int Int Int
  deriving Show

parseAddress :: Parser Address
parseAddress = do
  a <- decimal
  char '.'
  b <- decimal
  char '.'
  c <- decimal
  char '.'
  d <- decimal
  return $ Address a b c d

toString :: Address -> String
toString (Address a b c d) = intercalate "." $ map show [a, b, c, d]

rootBrute     = string "Failed password for root from " *> parseAddress
invalidUser   = string "Invalid user " *> manyTill anyChar (string "from ") *> parseAddress
noIdent       = string "Did not receive identification string from " *> parseAddress
preDisconnect = string "Received disconnect from " *> parseAddress <* manyTill anyChar (string "preauth")

findAbusiveAddress :: ByteString -> Either String Address
findAbusiveAddress = parseOnly $ skipHeader *> tryRules
  where
    pid = char '[' *> decimal *> char ']'
    skipHeader = manyTill anyChar (string "sshd") *> pid *> string ": "
    tryRules = rootBrute <|> invalidUser <|> noIdent <|> preDisconnect

findBoringAddress :: ByteString -> Either String Address
findBoringAddress = parseOnly $ zeroValues *> skipExcess *> ws *> parseAddress
  where
    ws = skipSpace
    star = ws *> char '*'
    hyphens = ws *> string "--"
    drop' = ws *> string "DROP"
    all' = ws *> string "all"
    zeroValues = ws *> char '0' *> ws *> char '0'
    skipExcess = drop' *> all' *> hyphens *> star *> star
