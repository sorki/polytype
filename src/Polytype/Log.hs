{-|
Description : Teletype logging helpers
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Polytype.Log (
    Log(..)
  , logs
  , Stamp(..)
  , stampLogs
  , TelMsg(..)
  , teletypeLog
  , teletypeLogTagged
  , reinterpretLogAsOutput
  , runLogShow
  , runLogStrings
  , runLogForward
  , runLogForward'
  ) where

import Prelude hiding (log)

import Polysemy
import Polysemy.Output
import Polysemy.Tagged

import Polytype.Teletype

import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Colog.Core (LogAction, cmapM)
import Colog.Core.IO (logStringStdout)
import Colog.Polysemy (Log(Log), log, runLogAction)

import qualified Data.Text
import qualified Data.Time

logs :: Member (Log msg) r => msg -> Sem r ()
logs = log

data TelMsg stringy = Read stringy | Write stringy
  deriving (Show, Eq, Ord)

data Stamp a = Stamp
    { stampData :: a
    , stampTime :: UTCTime
    } deriving (Show, Eq, Ord)

stampLogs
    :: LogAction IO (Stamp (TelMsg stringy)) -> LogAction IO (TelMsg stringy)
stampLogs = cmapM toStamp
  where
    toStamp :: TelMsg stringy -> IO (Stamp (TelMsg stringy))
    toStamp msg = do
        time <- Data.Time.getCurrentTime
        pure $ Stamp msg time

teletypeLog
    :: forall stringy r a
     . Monoid stringy
    => Sem (Teletype stringy ': r) a
    -> Sem (Log (TelMsg stringy) ': Teletype stringy ':r) a
teletypeLog = reinterpret2 $ \case
    ReadTTY -> do
        x <- readTTY
        log (Read x)
        return x
    WriteTTY msg -> log (Write msg) >> writeTTY msg

teletypeLogTagged
    :: forall t stringy r a
     . (Monoid stringy, Member (Tagged t (Log (TelMsg stringy))) r)
    => Sem (Teletype stringy ': r) a
    -> Sem (Teletype stringy ': r) a
teletypeLogTagged = tag @t @(Log (TelMsg stringy)) . reinterpret2 \case
    ReadTTY -> do
        x <- readTTY
        log (Read x)
        return x
    WriteTTY msg -> log (Write msg) >> writeTTY msg

-- | Forward teletype output to standard output
-- XXX convertible
runLogForward :: Member (Embed IO) r
              => Sem (Log (TelMsg String) ': r) a
              -> Sem r a
runLogForward = runLogAction @IO (contramap ex logStringStdout)
  where
    ex (Read  r) = r
    ex (Write _) = ""

runLogForward' :: Member (Embed IO) r
               => Sem (Log (TelMsg Text) ': r) a
               -> Sem r a
runLogForward' = runLogAction @IO (contramap ex logStringStdout)
  where
    ex (Read  r) = Data.Text.unpack r
    ex (Write _) = ""

runLogShow
    :: (Member (Embed IO) r, Show stringy)
    => Sem (Log (TelMsg stringy) ': r) a
    -> Sem r a
runLogShow = runLogAction @IO (stampLogs $ contramap show logStringStdout)

runLogStrings :: (Member (Embed IO) r) => Sem (Log String ': r) a -> Sem r a
runLogStrings = runLogAction @IO logStringStdout

-- | Reinterpret `Log` as `Output`
reinterpretLogAsOutput ::
       Sem (Log msg ': r) a
    -> Sem (Output msg ': r) a
reinterpretLogAsOutput = reinterpret $ \case
   Log msg -> output msg
