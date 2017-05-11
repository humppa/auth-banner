{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Maybe

version = "0.8.0"

-- ipCmd = "/bin/echo"
-- sqliteFilePath = "/home/tuomas/auth-banner.sqlite"

ipCmd = "/sbin/iptables"
sqliteFilePath = "/var/cache/auth-banner.sqlite"

headerText Nothing    = "Auth-Banner v" ++ version
headerText (Just pid) = "Auth-Banner v" ++ version ++ " (pid " ++ pid ++ ")"

copyText = "Copyright (c) 2017 Tuomas Starck"

offsetHelp = "Skip first N bytes of the file"

descText = "Auth-Banner follows system authentication log and blocks\
            \ any IP addresses which are deemed abusive"
