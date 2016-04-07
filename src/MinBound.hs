{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module MinBound (
    MinBound,
    minBound
) where

import Prelude hiding (Bounded, minBound)
import qualified Prelude as Pr (Bounded, minBound)

class MinBound a where 
    minBound :: a

instance Pr.Bounded a => MinBound a where
    minBound = Pr.minBound

instance MinBound [] where
	minBound = []

enumFromMin :: (Enum a, MinBound a) => [a]
enumFromMin = enumFrom minBound

