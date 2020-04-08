{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TargetSpec where

import SpecHelper

{--
spec :: Spec
spec = do
  it "handles dummy" $ do
    runTest (mkDummy >>= flip waitLine "dummy") `shouldReturn` (Right ())

main :: IO ()
main = hspec spec
--}
