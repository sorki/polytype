
test :: IO (ByteString, ByteString, ByteString)
test =
    runM
  . runInputConst @PtyOpts def
  . runPtyOpts
  $ (,,) <$> readTTY <*> readTTY <*> readTTY
