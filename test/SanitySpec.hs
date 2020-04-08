{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SanitySpec where

import SpecHelper

{--
spec :: Spec
spec = do
  it "passes" $ do
    runTest (return ()) `shouldReturn` (Right ())

  it "passes with testSuccess" $ do
    runTest testOk `shouldReturn` (Right Ok)

  it "fails" $ do
    runTest (testFail TimedOut) `shouldReturn` (Left TimedOut)

  it "fails subtest" $ do
    runTest (subtest "fail" $ testFail TimedOut) `shouldReturn` (Left TimedOut)

  it "fails subtest followed by success" $ do
    runTest (subtest "fail" (testFail TimedOut) >> testOk) `shouldReturn` (Left TimedOut)

  it "forks" $ do
    runTest (fork (delayms 1) >>= void . wait) `shouldReturn` (Right ())

  it "fork ignores failures" $ do
    -- questionable
    runTest (fork (testFail TimedOut) >>= wait >> testOk) `shouldReturn` (Right Ok)

main :: IO ()
main = hspec spec
--}
