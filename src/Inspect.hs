{-# LANGUAGE OverloadedStrings #-}

module Inspect (findAbusiveAddress) where

import Prelude hiding                   (take)
import Control.Applicative              ((<|>))
import Data.Attoparsec.ByteString       (Parser, manyTill, parseOnly, string, take)
import Data.Attoparsec.ByteString.Char8 (anyChar, char, decimal)
import Data.ByteString                  (ByteString)

import Types                            (Address(..))

findAbusiveAddress :: Int -> ByteString -> Either String Address
findAbusiveAddress n = parseOnly $ take n *> tryRules
  where
    tryRules = rootBrute <|> invalidUser <|> noIdent <|> preDisconnect

rootBrute     = string "Failed password for root from " *> parseAddress
invalidUser   = string "Invalid user " *> manyTill anyChar (string "from ") *> parseAddress
noIdent       = string "Did not receive identification string from " *> parseAddress
preDisconnect = string "Received disconnect from " *> parseAddress <* manyTill anyChar (string "preauth")

parseAddress = do
  a <- decimal
  char '.'
  b <- decimal
  char '.'
  c <- decimal
  char '.'
  d <- decimal
  return $ Address a b c d
