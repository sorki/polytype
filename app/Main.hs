{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Polytype

--import Multitype.Types

import Control.Monad
import Control.Monad.Loops

import Data.List

main = do
  --safe
  --testHtopParse
  --test_ircbridge_zre
  --test_pty
  --runTest
  return ()
{--
  --io <- mkConc
  io <- mkStdio
  --shim <- mkProcCwd ".." "gdb" [""]

  --fmap print $ runTest $ lal io
  --x <- runTest $ lal io

  t <- mkTCP "duck.lan" 3333
  x <- runTest $ lal t
  print x
  --runTest $ do
  --  waitString t "uu"
  --  return ()
  --fmap print $ runTest $ devnodeCycle zre

lal :: Target -> MT ()
lal t = do
  trace "lal!"
  subtest "Aa!" $ waitString t "aa"
  --subtest "F" $ testFail Halted
  waitString t "uuu"
  testFail Halted
  --getprompt t

getprompt t = subtest "getting prompt" $ do
  pinger <- fork $ forever $ do
    writeLine t ""
    delay 1

  iterateUntil ("root" `isInfixOf`) (readLine t) <* cancel pinger



 -- old shite
writeCmd t cmd = subtest ("calling command " ++ cmd) $ writeLine t cmd
cmd = writeCmd
ops t = do
  trace "nixops starts"
  waitString t "activation finished successfuly"

devnodeCycle t = do
  trace "cycling node"
  getprompt t
  writeCmd t "reboot"
  waitString t "and down we go"
  waitString t "reboot: Restarting system"
  waitLine t "ERROR: Type:2; Severity:80; Class:1; Subclass:1; Operation: 1001"
  waitString t "iPXE"
  waitString t "vpsAdminOS Stage 1"
  waitString t "vpsAdminOS Stage 2"
  waitString t "setting up /etc..."
  waitString t "devnode1 login"
  writeLine t "root"
  waitString t "Password"
  delay 1
  writeLine t "testarea"
  waitString t "root"
  writeLine t "uname -a"
  waitString t "Linux"
  delay 5
  testNet t
  testInstall t

testInstall t = sub "fake install" $ do
  cmd t "mkdir /install"
  cmd t "os-generate-config --root /install"
  waitString t "hardware-configuration.nix"
  waitString t "configuration.nix"
  cmd t "os-install"
  waitString t "setting root password..."
  waitString t "New password:"
  writeLine t "lal"
  waitString t "Retype new password:"
  writeLine t "lal"
  waitString t "installation finished!"

testNet t = do
  sub "network" $ do
    sub "eth0 present" $ do
      writeLine t "ip a"
      waitString t "eth0"
      testSuccess

    sub "osrtr0 present" $ do
      writeLine t "ip a"
      waitString t "osrtr0"
      testSuccess

--dnode :: (Monad m) => Target -> TestT b m String
dnode t = do
  trace "dnode starts"

  x <- getprompt t
  trace x
  --fork $ forever $ writeLine t "uptime" >> delay 1

  sub "counter" $ do
    writeLine t "counter=0"
    f <- fork $ do
      forM_ [0..1] $ \n -> do
        trace $ "Counter should be " ++ show n
        writeLine t "ip a"
        writeLine t "ip r"
        writeLine t "counter=$(( counter + 1 ))"
        writeLine t "echo $counter"
        delay 0
      testSuccess
    wait f

  subtest "init" $ do
    --untilM_ (writeLine t "uname -a") (fmap (== Right Ok) (waitString t "Linux"))
    testSuccess


  delay 1

  sub "osctl" $ do
    sub "can ct ls" $ do
      writeLine t "ct ls"
      waitString t "POOL"
      waitString t "webserver"
      testSuccess

  trace "BLAH"
  delay 1
  trace "LAL"

  delay 1
  trace "end"
  ok

  -}

{-
spam = do
    trace "w00t"
    delay 1
    spam
-}

{-
--demo :: (Monad m) => Target -> TestT m Result
demo io = do
  --fork $ spam
  --fork $ forM_ [1..6] $ pure $ trace "wtf"

  trace "demo starts"
  writeLine io "What's your name?"
  forM_ [1..6] $ pure $ delay 1 >> trace "wtf"
  name <- readLine io
  trace $ "got name: " ++ name
  writeLine io "Write 'yes' to quit"
  waitLine io "yes"
  trace "demo done"
  testOk

-}
