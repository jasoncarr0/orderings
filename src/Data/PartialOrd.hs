{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Data.PartialOrd (
    PartialOrd,
    PartialOrdering (..)
) where

import Data.Data
import GHC.Generics
import Prelude hiding (Ord, compare, Ordering (LT, EQ, GT))
import qualified Prelude as Pr (Ord, compare, Ordering (LT, EQ, GT))

-- | PartialOrdering defines a set of possible relations between two values.
-- For partial orders, these values may be any of less than (LT), greater than
-- (GT), equal (EQ), or incomparable (IC), that is neither the first value nor
-- the second is less than or equal
data PartialOrdering = LT | EQ | IC | GT 
    deriving (Bounded, Enum, Eq, Data, Read, Show, Generic)

embedOrdering :: Pr.Ordering -> PartialOrdering
embedOrdering Pr.LT = LT
embedOrdering Pr.EQ = EQ
embedOrdering Pr.GT = GT


class PartialOrd a where
    poCompare :: a -> a -> PartialOrdering

instance {-# OVERLAPPABLE #-} Pr.Ord a => PartialOrd a where
    poCompare a b = embedOrdering $ Pr.compare a b
 
instance PartialOrd PartialOrdering where
    poCompare LT LT = EQ
    poCompare LT _ = LT
    poCompare _ LT = GT
    poCompare GT GT = EQ
    poCompare _ _ = IC

