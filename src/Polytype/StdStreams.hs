{-|
Description : Standard process streams effect
-}

module Polytype.StdStreams (
    StdStreams(..)
  , writeStdin
  , readStdout
  , readStderr
  , teletypeAsStdStreams
  , stdStreamsAsTeletype
  , stdStreamsAsInputOutput
  , stdStreamsIO
  , ProcException(..)
  , streamStdStreams
  ) where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.Tagged

import Polysemy.Input.Streaming

import Data.Kind (Type)

import System.IO (Handle)
import qualified System.IO

import Streaming (Stream, Of)

import Polytype.Teletype (Teletype)
import qualified Polytype.Teletype

data StdStreams (stringy :: Type) m a where
  WriteStdin :: stringy -> StdStreams stringy m ()
  ReadStdout :: StdStreams stringy m stringy
  ReadStderr :: StdStreams stringy m stringy

makeSem_ ''StdStreams

writeStdin :: forall stringy r .  Member (StdStreams stringy) r
           => stringy        -- ^ Message to write
           -> Sem r ()       -- ^ Effectful computation with no result

readStdout :: forall stringy r .  Member (StdStreams stringy) r
           => Sem r stringy

readStderr :: forall stringy r .  Member (StdStreams stringy) r
           => Sem r stringy

instance Show (StdStreams String m x) where
  show (ReadStdout)   = "read out"
  show (ReadStderr)   = "read err"
  show (WriteStdin z) = "write in: [" ++ z ++ "]"

-- | Reinterpret `Teletype stringy` as `StdStreams stringy`
-- utilizing only stdout but not stderr.
teletypeAsStdStreams :: forall stringy r a .
                           Sem (Teletype stringy ': r) a
                        -> Sem (StdStreams stringy ': r) a
teletypeAsStdStreams = reinterpret $ \case
  Polytype.Teletype.ReadTTY    -> readStdout -- <|> readStderr -- possible with NonDet
  Polytype.Teletype.WriteTTY s -> writeStdin s

-- | Inverse of `teletypeAsStdStreams`
stdStreamsAsTeletype :: forall stringy r a .
                           Sem (StdStreams stringy ': r) a
                        -> Sem (Teletype stringy ': r) a
stdStreamsAsTeletype = reinterpret $ \case
  WriteStdin s -> Polytype.Teletype.writeTTY s
  ReadStdout   -> Polytype.Teletype.readTTY
  ReadStderr   -> Polytype.Teletype.readTTY

-- | Reinterpret `StdStreams` as `Tagged` `Input`s and `Output`.
stdStreamsAsInputOutput :: forall stringy hOut hErr r a . Monoid stringy =>
                              Sem (StdStreams stringy ': r) a
                           -> Sem (   Tagged hOut (Input (Maybe stringy))
                                   ': Tagged hErr (Input (Maybe stringy))
                                   ': Output stringy ': r
                                  )
                              a
stdStreamsAsInputOutput = reinterpret3 $ \case
      WriteStdin msg -> output msg
      ReadStdout -> maybe mempty id <$> tagged @hOut input
      ReadStderr -> maybe mempty id <$> tagged input

-- | Run `StdStreams` using `Streaming`
streamStdStreams :: Monad m
                 => Stream (Of String)
                      (Sem (Tagged "err" (Input (Maybe String)) : Output String : r)) ()
                 -> Stream (Of String)
                      (Sem (Output String : r)) ()
                 -> (Stream (Of String)
                      m () -> Sem r ())
                 -> Sem (StdStreams String : r) a
                 -> Sem r a
streamStdStreams sOut sErr sIn =
    runOutputStream sIn
  . runInputViaStream sErr
  . untag @"err"
  . runInputViaStream sOut
  . untag @"out"
  . stdStreamsAsInputOutput @String @"out" @"err"

data ProcException = StdoutNotAvailable | StderrNotAvailable
  deriving (Show, Eq, Ord)

-- | Run `StdStreams` via handles
-- treating `ReadStdout` and `ReadStderr` as readlines from stdin
-- and `WriteStdin`s as writes to stdout
stdStreamsIO :: Members '[Embed IO, Error ProcException] r
             => (Maybe Handle, Maybe Handle, Maybe Handle)
             -> Sem (StdStreams String ': r) a
             -> Sem r a
stdStreamsIO (stdin, stdout, _stderr) = interpret $ \case
  WriteStdin str -> embed $ maybe (pure ()) (flip System.IO.hPutStr str) stdout
  ReadStdout -> maybe (throw StdoutNotAvailable) (embed . System.IO.hGetLine) stdin
  ReadStderr -> maybe (throw StderrNotAvailable) (embed . System.IO.hGetLine) stdin
