
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polytype.Examples.Bash where

import Polytype

import Data.Text (Text)

-- | Example of interacting with bash running in PTY
-- with prompt effect.
bashExample :: IO String
bashExample =
    runM
  . runLogShow
  . untag @"test"
  . runPty "bash" ["--norc", "--noprofile"]
  . convertTeletypeStrings
  . runLogShow
  . teletypeLog
  . convertTeletypeStrings @String @Text
  . teletypeLogTagged @"test"
  . runReadlineHaskeline
  $ do
      waitString "$"
      writeLine "export"
      waitString "$"
      writeLine "export PS1='+[bashproxy e:$? ${BASH_SOURCE}:${LINENO}:${FUNCNAME[0]:+${FUNCNAME[0]}}:${BASH_SUBSHELL}] '"
      waitString "bashproxy"
      writeLine "echo $(( 2 + 2 ))"
      waitString "4"
      writeLine "false"
      waitString "e:1"
      cmd <- prompt ">"
      maybe (pure ()) writeLine cmd
      _ <- readTTY @String
      readTTY @String
