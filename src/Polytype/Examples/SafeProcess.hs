module Polytype.Examples.SafeProcess where

import Polytype

import qualified Streaming.Prelude

-- | Example of running system process safely.
safeProcess :: IO (Maybe String)
safeProcess =
    runFinal
  . resourceToIOFinal
  . runTimeoutToIO @Seconds
  . runProcessIOFinal
  . runProcessOverSSH "localhost"
  . runDelayIO @Seconds
  $ timeout @Seconds 2
  $ withProcess "sleep" ["3"] $ \(i, o, e, _p) -> do
       runOutputStream   (Streaming.Prelude.toHandle i)
     . runInputViaStream (Streaming.Prelude.fromHandle e)
     . untag @"err"
     . runInputViaStream (Streaming.Prelude.fromHandle o)
     . untag @"out"
     . stdStreamsAsInputOutput @String @"out" @"err"
     . teletypeAsStdStreams
     . runLogShow
     . teletypeLog
     $ readTTY
