{-|
Description : Higher level compositions
-}

module Polytype.Combinators (
    runTeletypeAsciinema
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource

import Polytype.Ansi
import Polytype.Asciinema
import Polytype.Log
import Polytype.Pty
import Polytype.Teletype

-- | Run `Teletype` with Asciinema asciicast output logger and
-- output forwarding to stdout.
runTeletypeAsciinema :: Members '[Embed IO, Resource] r
                     => Text
                     -> FilePath
                     -> PtyOpts
                     -> Sem (Teletype String : Input PtyOpts : Input AsciinemaOpts : r) a
                     -> Sem r (Either String a)
runTeletypeAsciinema title outputFile ptyOpts =
    runInputConst @AsciinemaOpts (AsciinemaOpts title outputFile ptyOpts)
  . runInputConst @PtyOpts ptyOpts
  -- here we are done handling teletype and need to handle "ambient" effects
  . runPtyOpts
  . convertTeletypeStrings @Text @ByteString
  . runLogAsciinema
  . teletypeLog -- for asciinema logger
  . runLogForward'
  . teletypeLog -- for forwarding to stdOut
  . runError @String
  . teletypeStripAnsi
  . convertTeletypeStrings @String @Text


