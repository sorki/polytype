{-|
Description : Teletype interpreters for interacting with POSIX serial terminals
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Polytype.Serial (
    runTeletypeAsSerial
  , unsafeRunTeletypeAsSerial
  , defaultSerialSettings
  ) where

import Polysemy
import Polysemy.Resource
import Polysemy.Input.Streaming

import Polytype.Teletype

import Data.Default (Default(def))
import System.Hardware.Serialport (SerialPortSettings(..), CommSpeed(..), FlowControl(..), Parity(..), StopBits(..))

import qualified Streaming.Prelude
import qualified System.IO
import qualified System.Hardware.Serialport

-- | Run `Teletype String` via `System.Hardware.Serialport`
runTeletypeAsSerial :: (Member (Embed IO) r, Member Resource r)
                    => FilePath
                    -> SerialPortSettings
                    -> Sem (Teletype String ': r) b
                    -> Sem r b
runTeletypeAsSerial dev opts rest = bracket
  (embed $ System.Hardware.Serialport.hOpenSerial dev opts)
  (embed . System.IO.hClose)
  (\h ->
      runOutputStream (Streaming.Prelude.toHandle h)
    . runInputViaStream (Streaming.Prelude.fromHandle h)
    . runTeletypeAsInputOutput $ rest)

-- | Like `runTeletypeAsSerial` without `Resource`
unsafeRunTeletypeAsSerial :: Member (Embed IO) r
                          => FilePath
                          -> SerialPortSettings
                          -> Sem (Teletype String : r) b
                          -> Sem r b
unsafeRunTeletypeAsSerial dev opts rest = do
  h <- embed $ System.Hardware.Serialport.hOpenSerial dev opts
  ret <- runOutputStream (Streaming.Prelude.toHandle h)
    . runInputViaStream (Streaming.Prelude.fromHandle h)
    . runTeletypeAsInputOutput
    $ rest
  embed $ System.IO.hClose h
  pure ret

-- | Opinionated `SerialPortSettings` defaults
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings = SerialPortSettings
  CS115200 8 One NoParity NoFlowControl 1

instance Default SerialPortSettings where
  def = defaultSerialSettings
