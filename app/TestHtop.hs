module Main where

import Options.Applicative

import Polytype
import Polytype.Examples.HtopAsciinema

main = execParser opts >>= htopAsciinemaExample
  where
    opts = info (argument str (metavar "FILE") <**> helper)
      ( fullDesc
       <> progDesc "Record htop demo using asciicast output")

