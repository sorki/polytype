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
  $ repeats_ 10 $ do
    logs "Start"
    logs "Pty"
    a <- async
        $ runPty "ircbridge-zre-pretty" []
        . convertTeletypeStrings
        . runLogStrings
        . runDelayAsync @Seconds
        $ do
            logs "waiting for HelloWorld"
            waitString "HelloWorld"

    delay 1
    logs "Proc"
    e <- runProc "ircbridge-zre-cat" ["--chan", "#bottest", "HelloWorld"]
    logs $ "ExitCode:" ++ (show e)
    logs "Await"
    b <- await a
    logs (show b)
    logs "Done"
  where
    runProc exe args = runProcessIO $ do
      (_i, _o, _e, h) <- createProcess exe args
      waitProcess h
