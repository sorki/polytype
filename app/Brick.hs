{-# LANGUAGE OverloadedStrings #-}

module Brick where

import Lens.Micro ((^.))
import Control.Monad

import Brick.Types
import Brick.Main
import Brick.Widgets.Core

--src/Brick/BChan.hs:23:51: error:
--newBChan size = atomically $ BChan <$> newTBQueue size

 runTestBrick t = runTerminal $ do
  st@MultiState{..} <- atomically $ defState
  (withReg $ \r -> do
    runStateT (iterT runIO t) st
    atomically $ closeTMQueue traceQueue
    finishConsoleRegion r ("Test done" :: String)) `A.concurrently` (withConsoleRegion Linear $ \r -> do
      setConsoleRegion r ("" :: String)
      setConsoleRegion r (take 10  $ repeat '\n')
      dumpTrace traceQueue r
      finishConsoleRegion r ("Trace done" :: String)
      ) `A.concurrently` (withConsoleRegion Linear $ \r -> forever $ do
      setConsoleRegion r $ do
        w <- consoleWidth
        h <- consoleHeight
        return $ T.pack $ unwords [ "size:", show w, "x", show h]
      threadDelay 1000000
      )
  return ()
  where
    lastN :: Int -> [a] -> [a]
    lastN n xs = foldl (const . drop 1) xs (drop n xs)

    dumpTrace q r = do
      buf <- atomically $ readTMQueue q
      case buf of
        Nothing -> return ()
        Just msg -> do
          appendConsoleRegion r (msg ++ "\n")
          tuneDisplay r $ pure . (T.unlines .  (lastN 9)  . T.lines)
          dumpTrace q r
 
