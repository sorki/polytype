{-|
Description : Race effect

Slightly adjusted code from
https://github.com/polysemy-research/polysemy/issues/252
by @KingoftheHomeless
* (currently unused)
-}

module Polytype.Race where


import Polysemy
import Polysemy.Input
import Polysemy.Final
import Polysemy.Tagged

import qualified Control.Concurrent.Async

data Race m a where
  Race  :: m a -> m b -> Race m (Either a b)

makeSem ''Race

raceToIOFinal
  :: Member (Final IO) r
  => Sem (Race ': r) a
  -> Sem r a
raceToIOFinal = interpretFinal @IO $ \case
  Race left right -> do
    left' <- runS left
    right' <- runS right
    pure
      $ either
        (fmap Left)
        (fmap Right)
        <$> Control.Concurrent.Async.race left' right'

raceInput
  :: forall i r
   . Member (Final IO) r
  => InterpreterFor (Input i) r
  -> InterpreterFor (Input i) (Tagged "intr1" (Input i) ': r)
  -> InterpreterFor (Input i) r
raceInput intr1 intr2 =
     intr1 . untag @"intr1"
   . intr2 . untag @"intr2"
   . (reinterpret2 $ \Input -> withStrategicToFinal $ do
        input1 <- runS (tag @"intr1" @(Input i) input)
        input2 <- runS (tag @"intr2" @(Input i) input)
        pure $ either id id <$> Control.Concurrent.Async.race input1 input2
     )
