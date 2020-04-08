{-|
Description : Experiments, mostly incomplete
-}
module Polytype.Experiments where

import Polysemy
import Control.Monad.Fix

import Polytype.Teletype

ask ::  Sem (Teletype String ': r) ()
ask = writeTTY @String "asking" >> readTTY @String >>= writeTTY @String

respond ::  Sem (Teletype String ': r) ()
respond = readTTY @String >> writeTTY @String "responding"

-- | Attempt to loop two `Teletype`s (`ask` & `respond`).
runTeletypeLoop :: Monoid stringy
                => Sem '[Teletype stringy] a
                -> Sem '[Teletype stringy] b
                -> ([stringy], [stringy])
runTeletypeLoop second first = fix $ \f ->
  ( fst . run $ runTeletypePure [] $ second -- this should get snd f as input but it loops
  , fst . run $ runTeletypePure (fst f) $ first)
