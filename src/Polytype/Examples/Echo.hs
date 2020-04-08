module Polytype.Examples.Echo where

import Polytype

-- | Simple echo
echoExample :: IO ()
echoExample =
    runM
  . teletypeToIO
  . runLogShow
  . teletypeLog
  $ echo @String
