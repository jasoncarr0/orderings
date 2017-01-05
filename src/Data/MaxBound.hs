{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.MaxBound (
    MaxBound,
    maxBound
) where

import Prelude hiding (Bounded, minBound)
import qualified Prelude as Pr (Bounded, minBound)

-- | MaxBound defines a maximum bounded element to the set
-- Whenever there is also an instance for some ordering, it should be true
-- that no element is less than maxBound :: a
class MaxBound a where
    maxBound :: a

instance Pr.Bounded a => MaxBound a where
    maxBound = Pr.maxBound


