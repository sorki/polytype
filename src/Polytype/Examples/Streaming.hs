module Polytype.Examples.Streaming where

import Polytype

import qualified System.IO

-- | Contrived example using `Teletype` for interaction
-- with current processes standard streams using `stdStreamsIO`.
streamingTeletypeExample :: IO (Either ProcException String)
streamingTeletypeExample =
    runFinal
  . embedToFinal @IO
  . errorToIOFinal @ProcException
  . traceToIO
  . stdStreamsIO (Just System.IO.stdin, Just System.IO.stdout, Just System.IO.stderr)
  . teletypeAsStdStreams @String
  $ do
    z <- readTTY @String
    writeTTY z
    return z
