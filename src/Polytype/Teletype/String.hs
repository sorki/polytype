module Polytype.Teletype.String (
    waitString
  , waitString'
  , writeLine
  ) where

import Polysemy
import Polytype.Teletype

import qualified Data.List

-- | Wait for a `String` to appear on `Teletype`
waitString :: Member (Teletype String) r
           => String
           -> Sem r ()
waitString s = go
  where
    go = do
      x <- readTTY
      if (s `Data.List.isInfixOf` x) then return () else go

-- | Variant of `waitString` returning matched `String`
waitString' :: Member (Teletype String) r
            => String
            -> Sem r String
waitString' s = go
  where
    go = do
      x <- readTTY
      if (s `Data.List.isInfixOf` x) then return x else go

-- | Write `String` terminated by newline to `Teletype`
writeLine :: Member (Teletype String) r
          => String
          -> Sem r ()
writeLine x = writeTTY (x ++ "\n")
