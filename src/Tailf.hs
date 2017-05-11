{-# LANGUAGE OverloadedStrings #-}

module Tailf (tailf) where

import Control.Concurrent      (threadDelay)
import Control.Exception       (tryJust)
import Control.Monad           (guard)
import System.IO               (IOMode(ReadMode), SeekMode(AbsoluteSeek),
                                hSeek, openFile)
import System.IO.Error         (isDoesNotExistError)
import System.Posix.Files      (fileSize, getFileStatus)

import qualified Data.ByteString as B

type FilePosition = Integer
type MicroSeconds = Int

-- |Tail a file, sending complete lines to the passed-in IO function.
-- If the file disappears, tailf will return, and the function will
-- be called one final time with a Left.
tailf
  :: FilePath     -- ^ File to be followed.
  -> FilePosition -- ^ File reading offset (usually 0).
  -> MicroSeconds -- ^ Polling delay for file reads.
  -> (Either String B.ByteString -> IO ()) -- ^ Line callback.
  -> IO ()
tailf path offset delay callback = do
  maybeStat <- tryJust (guard . isDoesNotExistError) $ getFileStatus path
  go offset
  where
    go offset = do
      threadDelay delay
      maybeStat <- tryJust (guard . isDoesNotExistError) $ getFileStatus path
      case maybeStat of
       Left e -> callback $ Left "File does not exist"
       Right stat -> do
         let newSize = fromIntegral $ fileSize stat :: Integer
         if newSize > offset
         then do
           handle <- openFile path ReadMode
           hSeek handle AbsoluteSeek offset
           newContents <- B.hGetContents handle
           let lines = B.splitWith (== 0x0A) newContents
           let startNext = newSize - (toInteger $ B.length $ last lines)
           mapM_ (callback . Right) $ init lines
           go startNext
         else do
           go offset
