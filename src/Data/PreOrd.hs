{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Data.PreOrd (
    PreOrd (..),
    PreOrdering (..),
    (<),
    (>),
    (<=),
    (>=),
    (<~),
    (>~),
    (~~),
) where

import Data.Data
import GHC.Generics
--import Prelude hiding (Ord, compare, Ordering (LT, EQ, GT), (<), (>), (<=), (>=))
import Prelude (Bool, Show, Eq, Read, Bounded, Enum, (==), (||), ($))
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

-- I have no idea what the issue is with this, but adding it turns PreOrd into ghc-prims Ord
instance {-# OVERLAPPABLE #-} PO.PartialOrd a => PreOrd a where
    prCompare a b = embedPartial $ PO.poCompare a b
-- Bogus instance to fix typechecker bug
instance PreOrd () where
    prCompare () () = EQ

(<) :: PreOrd a => a -> a -> Bool
a < b = prCompare a b == LT

(>) :: PreOrd a => a -> a -> Bool
a > b = prCompare a b == GT

(<=) :: PreOrd a => a -> a -> Bool
a <= b = let comp = prCompare a b in (comp == LT) || (comp == EQ) 

(>=) :: PreOrd a => a -> a -> Bool
a >= b = let comp = prCompare a b in (comp == GT) || (comp == EQ) 

(<~) :: PreOrd a => a -> a -> Bool
a <~ b = let comp = prCompare a b in (comp == LT) || (comp == EQ) || (comp == LG) 

(>~) :: PreOrd a => a -> a -> Bool
a >~ b = let comp = prCompare a b in (comp == GT) || (comp == EQ) || (comp == LG) 

(~~) :: PreOrd a => a -> a -> Bool
a ~~ b = prCompare a b == LG
