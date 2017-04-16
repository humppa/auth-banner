{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe            (catMaybes)
import Data.ByteString as B  (ByteString, null, putStr, split)
import Data.ByteString.Char8 (unpack)
import System.Environment    (getArgs)
import System.Process        (createProcess, proc, std_out, StdStream (CreatePipe))
import System.Exit           (exitFailure, exitSuccess)
import System.IO             (hGetContents, hPutStrLn, stderr)

import Sqlite                (initDB, seenOrInsert)
import Tailf                 (lineStream)

offendRules =
  [ (4, ["Invalid", "user", "from"])
  , (5, ["Failed", "password", "for", "root", "from"])
  ]

main :: IO ()
main = do
  initDB
  [filename, pos] <- getArgs
  lineStream filename (read pos) (10^6) lineProcessor

lineProcessor :: Either String ByteString -> IO ()
lineProcessor (Left err) = do
  hPutStrLn stderr err
  exitSuccess
lineProcessor (Right line) = do
  let words = preprocess line
  let addresses = catMaybes $ map (checkForOffence words) offendRules
  banAddr addresses
    where
      cutPrefix = drop 5
      removeEmpty = filter $ not . B.null
      splitToWords = split 0x20
      needle = ["message", "repeated", "times:"]
      dropRepeats = \x -> if ascend needle x then [] else x
      preprocess = dropRepeats . cutPrefix . removeEmpty . splitToWords

checkForOffence :: [ByteString] -> (Int, [ByteString]) -> Maybe ByteString
checkForOffence words (i, rule)
  | ruleMatches = Just $ words !! i
  | otherwise   = Nothing
    where
      ruleMatches = ascend rule words

banAddr :: [ByteString] -> IO ()
banAddr [] = return ()
banAddr (bytestring:_) = do
  let addr = unpack bytestring
  let args = ["-A", "BLACKLIST", "-s", addr, "-j", "DROP"]
  changes <- seenOrInsert addr
  case changes of
    0 -> do
      putStrLn $ "- skip  " ++ addr
    1 -> do
      putStrLn $ "* ban   " ++ addr
      createProcess (proc "/sbin/iptables" args){ std_out = CreatePipe }
      return ()
    otherwise -> exitFailure

ascend :: [ByteString] -> [ByteString] -> Bool
ascend [] _ = True
ascend _ [] = False
ascend n@(x:xs) h@(y:ys)
  | x == y    = ascend xs ys
  | otherwise = ascend  n ys
