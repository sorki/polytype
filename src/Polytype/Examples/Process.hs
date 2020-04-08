module Polytype.Examples.Process where

import System.Exit (ExitCode)

import Polytype

import qualified Streaming.Prelude

-- | Example of `Process` effect, for safe variant
-- using `Polysemy.bracket` and `withProcess`
-- see `Polytype.Examples.SafeProcess`.
processExample :: IO (Maybe (String, ExitCode))
processExample =
    runFinal
  . runTimeoutToIO @Seconds
  . runProcessIOFinal
  $ timeout @Seconds 2 $ do
    (i, o, e, h) <- createProcess "echo" ["polytype"]

    x <-
        runOutputStream   (Streaming.Prelude.toHandle i)
      . runInputViaStream (Streaming.Prelude.fromHandle e)
      . untag @"err"
      . runInputViaStream (Streaming.Prelude.fromHandle o)
      . untag @"out"
      . stdStreamsAsInputOutput @String @"out" @"err"
      . teletypeAsStdStreams
      $ readTTY

    ex <- waitProcess h
    return (x, ex)
