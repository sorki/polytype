{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module TeletypeSpec where

import SpecHelper
import qualified Data.Attoparsec.Text
import qualified Data.Text

spec :: Spec
spec = do
  it "runs purely" $ do
    flip shouldBe (["hello", "world"], ())
      $ run
      . runTeletypePure []
      $ writeTTY "hello" >> writeTTY "world"

  it "runs purely buffered by lines" $ do
    flip shouldBe ["ab 1", "123", "cd 2", "", "", "", "", "", "", "", ""]
      $ snd . snd
      . run
      . runState []
      . runTeletypePure ["ab 1\n123", "cd 2"]
      . bufferBy lines
      $ flip mapM [0..10] $ \x -> readTTY

  it "runs purely buffered by words and lines" $ do
    flip shouldBe ["ab", "1", "123", "cd", "2", "", "", "", "", "", ""]
      $ snd . snd
      . run
      . runState []
      . runTeletypePure ["ab 1\n123", "cd 2"]
      . bufferBy (concatMap words . lines)
      $ flip mapM [0..10] $ \x -> readTTY

  it "runs purely with parser" $ do
    flip shouldBe (["a"], Right ["test"])
      $ run
      . runTeletypePure ["te", "st"]
      . runError @String
      . teletypeParseWith
          Data.Attoparsec.Text.parse
          (Data.Attoparsec.Text.many1 $ Data.Attoparsec.Text.string "test")
          Data.Text.concat
      $ writeTTY ["a"] >> readTTY

  it "runs purely with parser failure" $ do
    flip shouldBe ([], Left "not enough input")
      $ run
      . runTeletypePure ["te", ""]
      . runError @String
      . teletypeParseWith
          Data.Attoparsec.Text.parse
          (Data.Attoparsec.Text.many1 $ Data.Attoparsec.Text.string "test")
          Data.Text.concat
      $ readTTY
