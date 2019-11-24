{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.GenericN
-- Copyright   : (C) 2018 Csongor Kiss
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Generic representation of types with multiple parameters
--
--------------------------------------------------------------------------------

module Data.Generics.GenericN
  ( Param
  , Indexed
  , Zip
  , Rec (Rec, unRec)
  , GenericN (..)
  , module GHC.Generics
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Data.Coerce

data family Param (n :: Nat) (a :: k) :: k

type family Indexed (t :: k) (i :: Nat) :: k where
  Indexed (t a) i = Indexed t (i + 1) (Param i a)
  Indexed t _     = t

newtype Rec (p :: Type) a x = Rec { unRec :: K1 R a x }

type family Zip (a :: Type -> Type) (b :: Type -> Type) :: Type -> Type where
  Zip (M1 mt m s) (M1 mt m t)
    = M1 mt m (Zip s t)
  Zip (l :+: r) (l' :+: r')
    = Zip l l' :+: Zip r r'
  Zip (l :*: r) (l' :*: r')
    = Zip l l' :*: Zip r r'
  Zip (Rec0 p) (Rec0 a)
    = Rec p a
  Zip U1 U1
    = U1
  Zip V1 V1
    = V1


class
  ( Coercible (Rep a) (RepN a)
  , Generic a
  ) => GenericN (a :: Type) where
  type family RepN (a :: Type) :: Type -> Type
  type instance RepN a = Zip (Rep (Indexed a 0)) (Rep a)
  toN :: RepN a x -> a
  fromN :: a -> RepN a x

instance
  ( Coercible (Rep a) (RepN a)
  , Generic a
  ) => GenericN a where
  toN :: forall x. RepN a x -> a
  toN   = coerce (to :: Rep a x -> a)
  {-# INLINE toN #-}

  fromN :: forall x. a -> RepN a x
  fromN = coerce (from :: a -> Rep a x)
  {-# INLINE fromN #-}
