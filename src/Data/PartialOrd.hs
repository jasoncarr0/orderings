{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Data.PartialOrd (
    PartialOrd (..),
    PartialOrdering (..)
    
) where

import Data.Data
import GHC.Generics
import Prelude hiding (Ord, compare, Ordering (LT, EQ, GT), (<), (>), (<=), (>=))
import qualified Prelude as P (Ord, compare, Ordering (LT, EQ, GT))

-- | PartialOrdering defines a set of possible relations between two values.
-- For partial orders, these values may be any of less than (LT), greater than
-- (GT), equal (EQ), or incomparable (IC), that is neither the first value nor
-- the second is less than or equal
data PartialOrdering = LT | EQ | IC | GT 
    deriving (Bounded, Enum, Eq, Data, Read, Show, Generic)

embedOrdering :: P.Ordering -> PartialOrdering
embedOrdering P.LT = LT
embedOrdering P.EQ = EQ
embedOrdering P.GT = GT

class PartialOrd a where
    poCompare :: a -> a -> PartialOrdering

instance {-# OVERLAPPABLE #-} P.Ord a => PartialOrd a where
    poCompare a b = embedOrdering $ P.compare a b

