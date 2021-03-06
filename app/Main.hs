{-# LANGUAGE OverloadedStrings #-}

{- TODO
 - Support configuration of match rules
 - Configuration for iptables stuff
 - Rename (e.g. botblock)
 -}

module Main where

import Prelude hiding        (lines)
import Control.Monad         (forM_)
import Data.ByteString       (ByteString, hGetContents)
import Data.ByteString.Char8 (lines)
import Data.Either           (rights)
import Data.Semigroup        ((<>))
import Options.Applicative --(*)
import System.Exit           (exitFailure, exitSuccess)
import System.IO             (BufferMode(..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.Process  (getProcessID)
import System.Posix.Signals  (Handler(..), installHandler, sigHUP)
import System.Process        (createProcess, proc, std_out, StdStream(CreatePipe))

import Config                (copyText, descText, headerText, ipCmd, offsetHelp)
import Rules                 (Address(..), findAbusiveAddress, findNotTooAbusiveAddress, toString)
import Sqlite                (initDatabase, existsOrInsert, blindDelete)
import Tailf                 (tailf)

data Args = Args { filename :: String, offset :: Integer }

args :: Parser Args
args = Args
  <$> argument str (metavar "FILE")
  <*> option auto (long "offset" <> metavar "N" <> value 0 <> help offsetHelp)

main :: IO ()
main = execParser magic >>= authbanner
  where
    magic = info (args <**> helper) $
      fullDesc <> progDesc descText <> header (headerText Nothing)

authbanner :: Args -> IO ()
authbanner (Args filename offset) = do
  pid <- show <$> getProcessID
  hSetBuffering stdout LineBuffering
  putStrLn $ headerText (Just pid)
  putStrLn copyText
  putStrLn "Initializing database"
  initDatabase
  putStrLn "Installing SIGHUP handler"
  installHandler sigHUP pruneTable Nothing
  putStrLn "Following log file"
  tailf filename offset (banOrElse . findAbusiveAddress)

pruneTable :: Handler
pruneTable = Catch $ do
  let args = ["-L", "BLACKLIST", "-n", "-v"]
  (_, Just h, _, _) <- createProcess (proc ipCmd args){ std_out = CreatePipe }
  output <- hGetContents h
  let addresses = rights $ map findNotTooAbusiveAddress (lines output)  -- :: [Address]
  forM_ (toString <$> addresses) $ \addr -> do
    blindDelete addr
    liftBan addr

banOrElse :: Either String Address -> IO ()
banOrElse (Left _) = return ()
banOrElse (Right a) = do
  let addr = toString a
  let args = ["-A", "BLACKLIST", "-s", addr, "-j", "DROP"]
  changes <- existsOrInsert addr
  case changes of
    0 -> do
      return ()
    1 -> do
      createProcess (proc ipCmd args){ std_out = CreatePipe }
      putStrLn $ "Banning " ++ addr
    otherwise -> exitFailure

liftBan :: String -> IO ()
liftBan addr = do
  let args = ["-D", "BLACKLIST", "-s", addr, "-j", "DROP"]
  createProcess (proc ipCmd args){ std_out = CreatePipe }
  putStrLn $ "Unbanning " ++ addr
