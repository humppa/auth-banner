{-# LANGUAGE OverloadedStrings #-}

module Sqlite (initDB, seenOrInsert) where

import Database.SQLite.Simple (Only(..), changes, close,
                               execute, execute_, open)

sqliteFilePath = "/var/cache/auth-banner.sqlite"

sqlCreateTable = "CREATE TABLE IF NOT EXISTS ban ( \
                 \ ip TEXT PRIMARY KEY, \
                 \ ts DATETIME DEFAULT CURRENT_TIMESTAMP \
                 \ )"

sqlInsertOrIgnore = "INSERT OR IGNORE INTO ban (ip) VALUES (?)"

initDB :: IO ()
initDB = do
  conn <- open sqliteFilePath
  execute_ conn sqlCreateTable
  close conn

seenOrInsert :: String -> IO Int
seenOrInsert addr = do
  conn <- open sqliteFilePath
  execute conn sqlInsertOrIgnore (Only addr)
  n <- changes conn
  close conn
  return n
