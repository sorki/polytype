{-|
Description : Asciinema compatibility and asciicast v2 implementation
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Polytype.Asciinema where

import Data.Aeson (ToJSON(..), Value(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Map (Map)
import GHC.Generics (Generic)

import qualified Data.Aeson.Text
import qualified Data.Map
import qualified Data.Scientific
import qualified Data.Text.Lazy
import qualified Data.Text.IO
import qualified Data.Time
import qualified Data.Time.Clock.POSIX
import qualified Data.Vector
import qualified System.IO

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Resource

import Polytype.Env
import Polytype.Log
import Polytype.Pty (PtyOpts(..))
import Polytype.Util (mapMOutput)

data AsciicastVersion = Version1 | Version2
  deriving (Show, Eq, Ord)

instance ToJSON AsciicastVersion where
  toJSON Version1 = Number 1
  toJSON Version2 = Number 2

data AsciicastMeta = AsciicastMeta {
    version   :: AsciicastVersion  -- ^ Version of the asciicast file format
  , width     :: Int               -- ^ Terminal width used during recording
  , height    :: Int               -- ^ Terminal height used during recording
  , timestamp :: Int               -- ^ UNIX timestamp marking the start of the session
  , title     :: Text              -- ^ Title of the session
  , env       :: Map String String
  } deriving (Generic, ToJSON, Show, Eq, Ord)

data AsciicastRecord = AsciicastRecord {
    timeDiffSeconds :: Float -- ^ How long it took since the start of the session
  , isInput         :: Bool  -- ^ Input from user or output from program
  , contents        :: Text
  } deriving (Eq, Show, Ord)

instance ToJSON AsciicastRecord where
  toJSON AsciicastRecord{..} =
    Array $ Data.Vector.fromList [
      (Number $ Data.Scientific.fromFloatDigits timeDiffSeconds)
    , (String $ if isInput then "i" else "o")
    , (String contents)
    ]

data AsciinemaOpts = AsciinemaOpts {
    aoTitle      :: Text      -- ^ Title of the session
  , aoOutputFile :: FilePath  -- ^ Output file path
  , aoPtyOpts    :: PtyOpts   -- ^ `PtyOpts` used for recording -
                              --    width, height and environment variables are stored in metadata header
  } deriving (Eq, Show, Ord)

-- | Interpret `Log (TelMsg Text)` as file output using @asciicast@ v2 format
--
-- `Input AsciinemaOpts` is used to configure output file path and
-- meta data for Asciinema header.
runLogAsciinema :: Members '[Embed IO, Resource, Input AsciinemaOpts] r
                => Sem (Log (TelMsg Text) ': r) a
                -> Sem r a
runLogAsciinema =
    runOutAsciinema
  . mapMOutput @(TelMsg Text) @(Stamp (TelMsg Text))
      (\o -> embed Data.Time.getCurrentTime >>= \t -> pure $ Stamp o t)
  . reinterpretLogAsOutput

-- | Interpreter for `Output` containing time-stamped `Teletype` `TelMsg`s
-- which are written to file using `asAsciicast`.
runOutAsciinema :: Members '[Embed IO, Resource, Input AsciinemaOpts] r
                => Sem (Output (Stamp (TelMsg Text)) ': r) a
                -> Sem r a
runOutAsciinema foo =  do
  AsciinemaOpts{..} <- input @AsciinemaOpts
  let PtyOpts{..} = aoPtyOpts

  bracket
    (embed do
      (h, t) <- (,)
        <$> System.IO.openFile aoOutputFile System.IO.WriteMode
        <*> Data.Time.getCurrentTime

      posixTime <- Data.Time.Clock.POSIX.getPOSIXTime
      env <- computeEnv ptyEnv

      -- output header
      Data.Text.IO.hPutStrLn h
        $ Data.Text.Lazy.toStrict
        $ Data.Aeson.Text.encodeToLazyText
        $ AsciicastMeta
            Version2
            ptyWidth
            ptyHeight
            (round posixTime)
            aoTitle
            (Data.Map.fromList env)

      pure (h, t)
      )
    (embed . System.IO.hClose . fst)
    \(h, t) -> interpret (\case
        Output o -> embed $ Data.Text.IO.hPutStrLn h (asAsciicast t o)
      ) foo

diffUTCTimeFloatSeconds :: UTCTime -> UTCTime -> Float
diffUTCTimeFloatSeconds t start =
    realToFrac
  $ Data.Time.diffUTCTime t start

-- | Marshall `Stamp (TelMsg Text)` to `AsciicastRecord`
-- into JSON output `Text`.
--
-- Requires `UTCTime` of the start of the recorded session.
--
-- Uses asciicast v2 format: https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md
--
asAsciicast :: UTCTime -> Stamp (TelMsg Text) -> Text
asAsciicast start (Stamp o t) =
    Data.Text.Lazy.toStrict
  . Data.Aeson.Text.encodeToLazyText
  $ AsciicastRecord
      (diffUTCTimeFloatSeconds t start)
      (isInput o)
      (unMsg o)
  where
    isInput (Write _) = True
    isInput _        = False
    unMsg (Read m) = m
    unMsg (Write m) = m
