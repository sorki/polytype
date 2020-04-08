{-|
Description :  Polymorphic Timeout effect

Thanks to @bolt12
 * via https://github.com/polysemy-research/polysemy/issues/342
 * adjusted to accept `timeUnit`
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Polytype.Timeout where


import Polysemy
import Polysemy.Final

import Data.Kind (Type)

import qualified System.Timeout

import Polytype.Types.Time (ToMicros(..))

data Timeout (timeUnit :: Type) m a where
  Timeout :: Int -> m a -> Timeout timeUnit m (Maybe a)

makeSem_ ''Timeout

timeout :: forall timeUnit r a . Member (Timeout timeUnit) r
        => Int
        -> Sem r a
        -> Sem r (Maybe a)

-- | Run `Timeout` in terms of `Final IO`.
runTimeoutToIO :: forall unit r a . (Member (Final IO) r, ToMicros unit)
               => Sem (Timeout unit ': r) a
               -> Sem r a
runTimeoutToIO = interpretFinal @IO \case
  Timeout i ma -> do
    n <- pureS Nothing
    ma' <- System.Timeout.timeout (scaleMicros @unit i) <$> runS ma
    pure $ ma' >>= maybe n (pure . fmap Just)
