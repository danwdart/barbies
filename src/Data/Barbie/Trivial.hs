{-# LANGUAGE PolyKinds #-}
module Data.Barbie.Trivial
  ( Void
  , Unit (..)
  )

where

import Data.Barbie.Internal.Constraints(ConstraintsB(..))
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Product(ProductB(..))
import Data.Barbie.Internal.ProductC(ProductBC(..))
import Data.Barbie.Internal.Traversable(TraversableB(..))

import Data.Data (Data(..))
import Data.Kind (Type)
import Data.Semigroup (Semigroup(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Prelude hiding (Semigroup(..))

---------------------------------------------------
-- Trivial Barbies
---------------------------------------------------

-- | Uninhabited barbie type.
data Void (f :: k -> Type)
  deriving (Generic, Typeable)

instance Eq   (Void f) where
  (==) v = case v of

instance Ord  (Void f) where
  compare v = case v of

instance Show (Void f) where
  showsPrec _ v = case v of

instance Semigroup (Void f) where
  (<>) v = case v of


instance FunctorB Void
instance TraversableB Void
instance ConstraintsB Void


-- | A barbie type without structure.
data Unit (f :: k -> Type)
  = Unit
  deriving
    ( Data, Generic, Typeable
    , Eq, Ord, Read, Show
    )

instance Semigroup (Unit f) where
  Unit <> Unit = Unit

instance Monoid (Unit f) where
  mempty  = Unit
  mappend = (<>)

instance FunctorB Unit
instance TraversableB Unit
instance ProductB Unit
instance ConstraintsB Unit
instance ProductBC Unit
