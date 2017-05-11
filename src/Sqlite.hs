{-# LANGUAGE OverloadedStrings #-}

module Sqlite (initDatabase, existsOrInsert, blindDelete) where

import Database.SQLite.Simple (Only(..), changes, close, execute, execute_, open, query_)

import Config (sqliteFilePath)

sqlCreateTable = "CREATE TABLE IF NOT EXISTS ban ( \
                 \ ip TEXT PRIMARY KEY, \
                 \ ts DATETIME DEFAULT CURRENT_TIMESTAMP \
                 \ )"

sqlInsertOrIgnore = "INSERT OR IGNORE INTO ban (ip) VALUES (?)"

sqlDeleteStraight = "DELETE FROM ban WHERE ip = ?"

initDatabase :: IO ()
initDatabase = do
  conn <- open sqliteFilePath
  execute_ conn sqlCreateTable
  close conn

existsOrInsert :: String -> IO Int
existsOrInsert addr = do
  conn <- open sqliteFilePath
  execute conn sqlInsertOrIgnore (Only addr)
  n <- changes conn
  close conn
  return n

blindDelete :: String -> IO ()
blindDelete addr = do
  conn <- open sqliteFilePath
  execute conn sqlDeleteStraight (Only addr)
  close conn
