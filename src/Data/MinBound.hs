{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.MinBound (
    MinBound,
    minBound
) where

import Prelude hiding (Bounded, minBound)
import qualified Prelude as Pr (Bounded, minBound)

-- | MinBound defines a minimum bounded element to the set
-- Whenever there is also an instance for some ordering, it should be true
-- That no element of a is less than minBound :: a
class MinBound a where 
    minBound :: a

instance Pr.Bounded a => MinBound a where
    minBound = Pr.minBound


instance MinBound [a] where
    minBound = []

enumFromMin :: (Enum a, MinBound a) => [a]
enumFromMin = enumFrom minBound

