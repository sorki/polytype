{-|
Description : Polytype utilities
-}

module Polytype.Util where

import Polysemy
import Polysemy.Output

import Polytype.Teletype

import qualified Control.Monad

-- | Map monadic `Sem` function over `Output` turning it into another output.
mapMOutput :: forall o1 o2 r a . ()
           => (o1 -> Sem r o2)
           -> Sem (Output o1 ': r) a
           -> Sem (Output o2 ': r) a
mapMOutput f = reinterpret \case
  Output o -> raise (f o) >>= output

-- | Repeat monadic action N times returning results as a list.
repeats :: (Monad m) => Integer -> (m b) -> m [b]
repeats n a = Control.Monad.forM [0..n] (pure a)

-- | Repeat monadic action N times returning discarding results.
repeats_ :: (Monad m) => Integer -> (m b) -> m ()
repeats_ n a = Control.Monad.forM_ [0..n] (pure a)

-- | Read multiple messages from `Teletype`.
readMany :: Int -> Sem (Teletype stringy ': r) [stringy]
readMany x = mapM (pure readTTY) [0..x]

-- | Retry an action until it succeeds.
retry :: Monad m => m (Maybe b) -> m b
retry a = do
  r <- a
  case r of
    Nothing -> retry a
    Just v -> return v

-- | Retry an action until it succeeds or we run out of attempts.
retryCount :: Monad m => Int -> m (Maybe b) -> m (Maybe b)
retryCount 0 _ = return Nothing
retryCount c a = do
  r <- a
  case r of
    Nothing -> retryCount (c - 1) a
    v -> return v
