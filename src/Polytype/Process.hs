{-|
Description : Process control effect
-}

module Polytype.Process (
    Process(..)
  , createProcess
  , waitProcess
  , terminateProcess

  , withProcess

  , runProcessIO
  , runProcessIOFinal
  , runProcessOverSSH
  ) where

import Polysemy
import Polysemy.Resource

import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (ProcessHandle)

import qualified System.Process

data Process m a where
  CreateProcess
    :: String
    -> [String]
    -> Process m ( Handle
                 , Handle
                 , Handle
                 , ProcessHandle)
  WaitProcess
    :: ProcessHandle
    -> Process m ExitCode

  TerminateProcess
    :: ProcessHandle
    -> Process m ()

makeSem ''Process

-- | Interpret process in terms of `IO`
runProcessIO :: Member (Embed IO) r
             => Sem (Process ': r) a
             -> Sem r a
runProcessIO = interpret $ \case
  CreateProcess exe args -> embed
    $ (\(Just i, Just o, Just e, ph) -> (i, o, e, ph))
    <$> System.Process.createProcess (System.Process.proc exe args)
          { System.Process.std_in  = System.Process.CreatePipe
          , System.Process.std_out = System.Process.CreatePipe
          , System.Process.std_err = System.Process.CreatePipe
          }

  WaitProcess handle -> embed $ System.Process.waitForProcess handle
  TerminateProcess handle -> embed $ System.Process.terminateProcess handle

-- | Interpret process in term of `Final IO`
runProcessIOFinal :: Member (Final IO) r
                  => Sem (Process ': Embed IO ': r) a
                  -> Sem r a
runProcessIOFinal = embedToFinal @IO . runProcessIO

-- | Reinterpret process in term of process running over ssh
runProcessOverSSH :: Member (Embed IO) r
                  => String -- host?
                  -> Sem (Process ': r) a
                  -> Sem (Process ': r) a
runProcessOverSSH host = reinterpret $ \case
  CreateProcess exe args -> createProcess "ssh" (host:exe:args)
  WaitProcess handle -> waitProcess handle
  TerminateProcess handle -> terminateProcess handle

-- | Spawn a process and run another `Sem r a` computation
-- interacting with it.
--
-- Safely terminates the process in case of failure and completion
-- of the nested computation.
withProcess :: Members '[Resource, Process] r
            => String
            -> [String]
            -> ((Handle, Handle, Handle, ProcessHandle) -> Sem r a)
            -> Sem r a
withProcess exe args rest = do
  bracket
    (createProcess exe args)
    (\(_, _, _, h) -> terminateProcess h)
    (\p@(_, _, _, h) -> do
      x <- rest p
      terminateProcess h
      pure x
      )
