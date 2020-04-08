module Polytype.Teletype.Text (
    waitText
  , waitText'
  , writeTextLine
  ) where

import Data.Text (Text)
import qualified Data.Text

import Polysemy
import Polytype.Teletype

-- | Wait for a `Text` to appear on `Teletype`
waitText :: Member (Teletype Text) r
         => Text
         -> Sem r ()
waitText s = go
  where
    go = do
      x <- readTTY
      if (s `Data.Text.isInfixOf` x) then return () else go

-- | Variant of `waitText` returning matched `Text`
waitText' :: Member (Teletype Text) r
         => Text
         -> Sem r Text
waitText' s = go
  where
    go = do
      x <- readTTY
      if (s `Data.Text.isInfixOf` x) then return x else go

-- | Write `Text` terminated by newline to `Teletype`
writeTextLine :: Member (Teletype Text) r
          => Text
          -> Sem r ()
writeTextLine x = writeTTY (x <> Data.Text.pack "\n")
