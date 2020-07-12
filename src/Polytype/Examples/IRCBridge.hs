{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polytype.Examples.IRCBridge where

import Polytype

-- | Actual test for ircbridge package utilizing `ircbridge-zre-cat`
-- and `ircbridge-zre-pretty` executables to test loopback of
-- IRC forwarding.
--
-- Requires two forwarders running so one can see the messages
-- of the other one.
testIrcbridgeZre :: IO ()
testIrcbridgeZre =
    runFinal
  . asyncToIOFinal
  . embedToFinal @IO
  . runLogStrings
  . runDelayIO @Seconds
  . runTimeoutToIO @Seconds
  $ do
    logs "Starting test"
    res <- repeats 10 $ retryCount 1 $ timeout 5 $ do
      logs "Start"
      logs "Pty"
      a <- async
          $ runPty "ircbridge-zre-pretty" []
          . convertTeletypeStrings
          . runLogShow
          . teletypeLog -- enable for debugging raw input / output
          . runLogStrings
          . runDelayAsync @Seconds
          $ do
              logs "waiting for HelloWorld"
              waitString "HelloWorld"
              logs "Got HelloWorld"
              return True

      delay 1
      logs "Proc"
      e <- runProc "ircbridge-zre-cat" ["--chan", "#bottest", "HelloWorld"]
      logs $ "ExitCode:" ++ (show e)
      logs "Await"
      b <- await a
      logs (show b)
      logs "Done"
      return True
    logs "Test done"
    logs (show res)
  where
    runProc exe args = runProcessIO $ do
      (_i, _o, _e, h) <- createProcess exe args
      waitProcess h
