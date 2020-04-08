module PtySpec where

import Data.ByteString (ByteString)

import SpecHelper

spec :: Spec
spec = return ()

-- not implemented yet as it requires some program to interact with

test :: IO (ByteString, ByteString, ByteString)
test =
    runM
  . runInputConst @PtyOpts def
  . runPtyOpts
  $ (,,) <$> readTTY <*> readTTY <*> readTTY
