{-|
Description : Time unit types
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Polytype.Types.Time (
    MicroSeconds
  , MilliSeconds
  , Seconds
  , Minutes
  , Hours
  , Days
  , ToMicros(..)
  ) where

data MicroSeconds
data MilliSeconds
data Seconds
data Minutes
data Hours
data Days

class ToMicros a where
  scaleMicros :: (Int -> Int)

instance ToMicros MicroSeconds where
  scaleMicros = (*1)

instance ToMicros MilliSeconds where
  scaleMicros = (*1_000)

instance ToMicros Seconds where
  scaleMicros = (*1_000_000)

instance ToMicros Minutes where
  scaleMicros = (*(1_000_000 * 60))

instance ToMicros Hours where
  scaleMicros = (*(1_000_000 * 60 * 60))

instance ToMicros Days where
  scaleMicros = (*(1_000_000 * 60 * 60 * 7))
