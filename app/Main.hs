{-# LANGUAGE OverloadedStrings #-}

{- TODO -
 - Configuration file
 - Lift manual optimization
 - At init, if there are addresses in database:
 -  Flush BLACKLIST table
 -  Add addresses from database
 - Periodically clear old rules from database and tables
 - DSL for offence rules:
 -  A name
 -  Line match rules
 -  Addr parse rules
 - Rename
 -}

module Main where

import Data.ByteString       (ByteString)
import System.Environment    (getArgs)
import System.Process        (createProcess, proc, std_out, StdStream(CreatePipe))
import System.Exit           (exitFailure, exitSuccess)
import System.IO             (hPutStrLn, stderr)

import Inspect               (findAbusiveAddress)
import Sqlite                (initDB, existsOrInsert)
import Tailf                 (lineStream)
import Types                 (Address(..), toString)

main :: IO ()
main = do
  putStrLn "Auth-Banner version 2017-04-21"
  initDB
  [filename, cut, pos] <- getArgs
  lineStream filename (read pos) (10^6) (lineProcessor $ read cut)

lineProcessor :: Int -> Either String ByteString -> IO ()
lineProcessor n (Left err) = do
  -- err is always nothing but "File does not exist"
  hPutStrLn stderr err
  exitFailure
lineProcessor n (Right line) = do
  banAddr $ findAbusiveAddress n line

banAddr :: Either String Address -> IO ()
banAddr (Left _) = return ()
banAddr (Right a) = do
  let addr = toString a
  let args = ["-A", "BLACKLIST", "-s", addr, "-j", "DROP"]
  changes <- existsOrInsert addr
  case changes of
    0 -> do
      putStrLn $ "- " ++ addr
    1 -> do
      putStrLn $ "+ " ++ addr
      createProcess (proc "/sbin/iptables" args){ std_out = CreatePipe }
      return ()
    otherwise -> exitFailure
