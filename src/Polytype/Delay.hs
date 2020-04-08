{-|
Description : Polymorphic Delay effect
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Polytype.Delay (
    Delay(..)
  , delay
  , runDelayAsync
  , runDelayIO
  ) where

import Data.Kind (Type)
import qualified Control.Concurrent

import Polysemy
import Polysemy.Async

import Polytype.Types.Time (ToMicros(..))

data Delay (delayUnit :: Type) m a where
  Delay :: Int -> Delay delayUnit m ()

makeSem_ ''Delay

delay :: forall delayUnit r . Member (Delay delayUnit) r
      => Int
      -> Sem r ()

-- Has to run via Final interpret as `asyncToIO` is not
-- safe when using nested `await`
runDelayAsync :: forall delayUnit r a
               . ( Members '[Embed IO, Async] r
                 , ToMicros delayUnit)
           => Sem (Delay delayUnit ': r) a
           -> Sem r a
runDelayAsync = interpret $ \case
  Delay dt -> do
    a <- async $ embed $ Control.Concurrent.threadDelay $ scaleMicros @delayUnit dt
    _ <- await a
    pure ()

runDelayIO :: forall delayUnit r a . (Member (Embed IO) r, ToMicros delayUnit)
           => Sem (Delay delayUnit ': Async ': r) a
           -> Sem r a
runDelayIO = asyncToIO . runDelayAsync

-- this is broken and blocks even when wrapped in async
{--
runDelayBroken :: forall delayUnit r a
                . ( Member (Embed IO) r
                  , ToMicros delayUnit)
               => Sem (Delay delayUnit ': r) a
               -> Sem r a
runDelayBroken = interpret $ \case
  Delay dt -> embed $ Control.Concurrent.threadDelay $ scaleMicros @delayUnit dt
--}
