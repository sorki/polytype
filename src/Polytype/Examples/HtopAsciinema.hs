{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polytype.Examples.HtopAsciinema where

import Polytype

-- | Record a demo of htop interaction
-- using asciicast logger.
htopAsciinemaExample :: FilePath -> IO ()
htopAsciinemaExample outputFile =
    void
  . runFinal
  . embedToFinal @IO
  . resourceToIO
  . runTeletypeAsciinema "htop demo" outputFile (ptyOptsExeArgs "htop" [])
--  . runLogShow
--  . teletypeLog -- enable for debugging raw input / output
  . asyncToIOFinal
  . runDelayAsync @MilliSeconds
  . runDelayAsync @Seconds
  . runDelayAsync @Minutes
  $ do
      waitString "Load average"
      waitString "Help"
      writeTTY "\ESC[11~" -- F1
      waitString "incremental"
      writeTTY "\ESC[11~"
      waitString "Uptime"

      delay @Seconds 2

      void $ async $ forever $ readTTY >> pure ()

      void $ async $ do
        --writeTTY "\ESC[13~"
        display "Welcome to htop demo!"
        display "Recorded with polytype"
        display "Press h or F1 for help"
        writeTTY "h"
        delay @Seconds 5
        writeTTY "h"
        delay @Seconds 2

        display "Search feature - F3"
        writeTTY "\ESC[13~"
        writeLineSlowly "htop"
        delay @Seconds 5

        display "Filtering feature - F4"
        writeTTY "\ESC[14~"
        writeLineSlowly "polytype"
        delay @Seconds 5

        display ""

        writeTTY "\ESC[14~"
        writeLineSlowly "htop"
        delay @Seconds 5

        display "Sorted - F5"
        writeTTY "\ESC[15~"
        delay @Seconds 5

        display "Toggle program path - p"
        writeTTY "p"
        delay @Seconds 5

        display "That's all for now!"
        display "Lets toggle program path for a bit"
        forever $ do
          writeTTY "p"
          delay @Seconds 5

      delay @Minutes 2
      display "Thanks for watching"
      writeTTY "q"
      readTTY
  where
    writeSlowly [] = delay @Seconds 1
    writeSlowly (x:xs) = do
      writeTTY [x]
      delay @MilliSeconds 100
      writeSlowly xs

    writeLineSlowly msg = writeSlowly msg >> writeLine ""

    display :: Members '[Teletype String, Delay Seconds, Delay MilliSeconds] r
            => String
            -> Sem r ()
    display msg = do
      writeTTY "\ESC[14~"
      writeTTY "\ESC~"
      writeTTY "\ESC[14~"
      writeSlowly msg
      writeTTY "\ESC~"
