{-# LANGUAGE OverloadedStrings #-}

module Tailf (tailf) where

import Prelude hiding        (init)
import Control.Concurrent    (threadDelay)
import Control.Exception     (tryJust)
import Control.Monad         (guard)
import Data.ByteString       (ByteString, hGetSome, splitWith)
import Data.Monoid           ((<>))
import System.IO             (IOMode(ReadMode), SeekMode(AbsoluteSeek),
                              hClose, hSeek, hTell, withFile)
import System.IO.Error       (isDoesNotExistError)
import System.Linux.Inotify  (Inotify, Event(..), Mask(..),
                              addWatch, getEvent, init,
                              in_DELETE_SELF, in_MODIFY, in_MOVE_SELF)

type MSec = Int
type FileOffset = Integer

tailf :: FilePath -> FileOffset -> MSec -> (ByteString -> IO ()) -> IO ()
tailf path offset delay callback = do
  inotify <- init
  watch <- tryJust (guard . isDoesNotExistError) $
    addWatch inotify path (in_MODIFY <> in_MOVE_SELF <> in_DELETE_SELF)
  case watch of
    (Left  _) -> do
      threadDelay delay
      tailf path 0 delay callback
    (Right _) -> do
      eventReader inotify path offset callback
      tailf path offset delay callback

eventReader :: Inotify -> FilePath -> Integer -> (ByteString -> IO ()) -> IO ()
eventReader inotify path offset callback = do
  event <- getEvent inotify
  case event of
    (Event _ (Mask 2) _ _) -> do
      n <- readf path offset callback
      eventReader inotify path n callback
    _ -> return ()

readf :: FilePath -> Integer -> (ByteString -> IO ()) -> IO Integer
readf path offset callback = withFile path ReadMode $ \handle -> do
  hSeek handle AbsoluteSeek offset
  content <- hGetSome handle (2^30)
  newOffset <- hTell handle
  mapM_ callback $ splitWith (== 0x0A) content
  hClose handle
  return newOffset
