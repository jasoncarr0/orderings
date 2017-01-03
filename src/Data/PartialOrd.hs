{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Data.PartialOrd (
    PartialOrd,
    PartialOrdering (..)
) where

import Data.Data
import GHC.Generics
import Prelude hiding (Ord, compare, Ordering)
import qualified Prelude as Pr (Ord, compare, Ordering (LT, EQ, GT))

-- | PartialOrdering defines a set of possible relations between two values.
-- For partial orders, these values may be any of less than (LT), greater than
-- (GT), equal (EQ), or incomparable (IC), that is neither the first value nor
-- the second is less than or equal
data PartialOrdering = LT | EQ | IC | GT 
    deriving (Bounded, Enum, Eq, Data, Read, Show, Generic)

embedOrdering :: Pr.Ordering -> PartialOrdering
embedOrdering Pr.LT = Data.PartialOrd.LT
embedOrdering Pr.EQ = Data.PartialOrd.EQ
embedOrdering Pr.GT = Data.PartialOrd.GT


class PartialOrd a where
    pCompare :: a -> a -> PartialOrdering

instance {-# OVERLAPPABLE #-} Pr.Ord a => PartialOrd a where
    pCompare a b = embedOrdering $ Pr.compare a b
 
instance PartialOrd PartialOrdering where
    pCompare Data.PartialOrd.LT Data.PartialOrd.LT = Data.PartialOrd.EQ
    pCompare Data.PartialOrd.LT _ = Data.PartialOrd.LT
    pCompare _ Data.PartialOrd.LT = Data.PartialOrd.GT
    pCompare Data.PartialOrd.GT Data.PartialOrd.GT = Data.PartialOrd.EQ
    pCompare _ _ = IC

