{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Data.PreOrd (
    PreOrd (..),
    PreOrdering (..)
) where

import Data.Data
import GHC.Generics
import Prelude hiding (Ord, compare, Ordering (LT, EQ, GT))
import qualified Prelude as Pr (Ord, compare, Ordering (LT, EQ, GT))
import qualified Data.PartialOrd as PO

-- | PreOrdering defines a set of possible preorder relations between two values
-- For preorders, these terms are less than (LT), greater than (GT), less than
-- and greater than (LG), equal (EQ), and incomparable (IC). 
-- Equality strictly is not part of the preorder, but is a useful check for users

data PreOrdering = LT | EQ | LG | IC | GT
    deriving (Bounded, Enum, Eq, Data, Read, Show, Generic)

embedPartial :: PO.PartialOrdering -> PreOrdering
embedPartial PO.LT = LT
embedPartial PO.EQ = EQ
embedPartial PO.IC = IC
embedPartial PO.GT = GT

class PreOrd a where
    prCompare :: a -> a -> PreOrdering

instance {-# OVERLAPPABLE #-} PO.PartialOrd a => PreOrd a where
    prCompare a b = embedPartial $ PO.poCompare a b
