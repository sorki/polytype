{-|
Description : Debug effect for tracing other effects

From https://github.com/polysemy-research/polysemy/issues/287
by @isovector.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Polytype.Debug (
    debugTraceEffect
  , debugTraceEffectWith
  ) where

import Polysemy.Trace
import Polysemy.Internal
import Polysemy.Internal.Union

-- | Trace calls to some effect, requires `Show` instance for the effect data type.
debugTraceEffect
    :: forall e r a
     . ( Members '[e, Trace] r
       , forall m x. Show (e m x)
       )
    => Sem r a
    -> Sem r a
debugTraceEffect (Sem m) = Sem $ \k -> m $ \u ->
  case prj @e u of
    Just (Weaving e _ _ _ _) -> do
      usingSem k $ trace $ show e
      k u
    Nothing -> k $ hoist (debugTraceEffect @e) u

-- | Version of `debugTraceEffect` accepting rendering function directly
debugTraceEffectWith
    :: forall e r a
     . Members '[e, Trace] r
    => (forall m x . e m x -> String)
    -> Sem r a
    -> Sem r a
debugTraceEffectWith showFn (Sem m) = Sem $ \k -> m $ \u ->
  case prj @e u of
    Just (Weaving e _ _ _ _) -> do
      usingSem k $ trace $ showFn e
      k u
    Nothing -> k $ hoist (debugTraceEffectWith @e showFn) u
