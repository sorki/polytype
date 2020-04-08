module StdStreamsSpec where

import SpecHelper

spec :: Spec
spec = return ()

testIdentity :: Sem (Teletype stringy : r) a
             -> Sem (Teletype stringy : r) a
testIdentity x =
    stdStreamsAsTeletype
  . teletypeAsStdStreams
  $ x


