{-|
Description : Teletype interpreters for interacting with programs spawned with TTY
-}

module Polytype.Pty where

import Polysemy
import Polysemy.Input

import Polytype.Env
import Polytype.Teletype

import Data.ByteString (ByteString)
import Data.Default (Default(def))
import System.Process (ProcessHandle)
import System.Posix.Pty (Pty)

import qualified System.Posix.Pty

-- | Reinterpret `Teletype ByteString` in terms
-- of `Input (Pty, ProcessHandle)`.
runTeletypePtyOpts :: Member (Embed IO) r
                   => Sem (Teletype ByteString ': r) a
                   -> Sem (Input (Pty, ProcessHandle) ': r) a
runTeletypePtyOpts = reinterpret \case
  ReadTTY -> do
    (pty, _handle) <- input @(Pty, ProcessHandle)
    embed $ System.Posix.Pty.readPty pty
  WriteTTY msg -> do
    (pty, _handle) <- input @(Pty, ProcessHandle)
    embed $ System.Posix.Pty.writePty pty msg

data PtyOpts = PtyOpts {
    ptyEnv        :: Env      -- Environment variables to add and pass-thru from system
  , ptySearchPath :: Bool     -- Search $PATH environment variable
  , ptyExecutable :: FilePath -- Executable name or path
  , ptyArgs       :: [String] -- Arguments for the executable
  , ptyWidth      :: Int      -- Width of the spawned PTY in characters
  , ptyHeight     :: Int      -- Height of the spawned PTY
  } deriving (Eq, Show, Ord)

instance Default PtyOpts where
  def = PtyOpts {
    ptyEnv = mkEnv
      [
        ("POLYTYPE", "true")
      , ("TERM", "rxvt-unicode-256color")
      ]
      ["PATH", "SHELL", "TERMINFO", "TERMINFO_DIRS"]
  , ptySearchPath = True
  , ptyExecutable = mempty
  , ptyArgs = mempty
  , ptyWidth = 80
  , ptyHeight = 40
  }

-- Create `PtyOpts` with executable name or path ad its arguments
-- using defaults for the rest of `PtyOpts`.
ptyOptsExeArgs :: FilePath -> [String] -> PtyOpts
ptyOptsExeArgs exe args = def {
    ptyExecutable = exe
  , ptyArgs = args
  }

-- | `System.Posix.Pty.spawnWithPty` variant accepting `PtyOpts`
spawnWithPtyOpts :: PtyOpts -> IO (Pty, ProcessHandle)
spawnWithPtyOpts PtyOpts{..} = do
  env <- computeEnv ptyEnv
  System.Posix.Pty.spawnWithPty
    (Just env)
    ptySearchPath
    ptyExecutable
    ptyArgs
    (ptyWidth, ptyHeight)

-- | Run `Teletype ByteString` using `System.Posix.Pty.spawnWithPty`.
--
-- Requires `PtyOpts` `Input` as its configuration.
runPtyOpts :: (Members '[Embed IO, Input PtyOpts] r)
           => Sem (Teletype ByteString ':  r) a
           -> Sem r a
runPtyOpts rest = do
  opts <- input @PtyOpts
  i <- embed $ spawnWithPtyOpts opts
  runInputConst i $ runTeletypePtyOpts $ rest

-- | Run `Teletype ByteString` using `System.Posix.Pty.spawnWithPty`.
--
-- Requires `PtyOpts` `Input` effect handled next.
runPty :: (Member (Embed IO) r)
       => FilePath
       -> [String]
       -> Sem (Teletype ByteString ': Input PtyOpts ': r) b
       -> Sem r b
runPty exe args =
    runInputConst @PtyOpts (ptyOptsExeArgs exe args)
  . runPtyOpts
