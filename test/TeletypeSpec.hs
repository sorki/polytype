module TeletypeSpec where

import SpecHelper

spec :: Spec
spec = return ()

-- run
-- . runTeletypePure ["te", ""]
-- . runError @String
-- . teletypeParse (many1 $ string "test")  $ readTTY
-- ([],Left "not enough input")
--
--
-- λ: run
-- . runTeletypePure ["te", "st"]
-- . runError @String
-- . teletypeParse (many $ string "test") (Data.Text.concat)
-- $ writeTTY ["a"] >> readTTY
-- (["a"],Right ["test"])

-- >>> run $ runState [] $ runTeletypePure ["ab 1\n123", "cd 2"] $ bufferBy lines $ flip mapM [0..10] $ \x -> readTTY
-- ([],([],["ab 1","123","cd 2","","","","","","","",""]))
-- >>> run $ runState [] $ runTeletypePure ["ab 1\n123", "cd 2"] $ bufferBy (concatMap words . lines) $ flip mapM [0..10] $ \x -> readTTY
-- ([],([],["ab","1","123","cd","2","","","","","",""]))
-- bufferBy

