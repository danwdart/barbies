{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
module Spec.Distributive ( laws )

where

import           Clothes               (F, G, GH (..), H, NatTransf (..))

import           Data.Functor.Compose  (Compose (..))
import           Data.Functor.Identity (Identity (..))

import           Data.Functor.Barbie   (DistributiveB (..), FunctorB (..))

import           Data.Typeable         (Proxy (..), Typeable, typeRep)

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), testProperty, (===))

type IsDomain a = (Arbitrary a, Show a)
type IsRange a = (Eq a, Show a)

laws
  :: forall b
  . ( DistributiveB b
    , IsDomain (b F)
    , IsRange (b (Compose H F))
    , IsRange (b (Compose Identity F))
    , IsRange (b (Compose (Compose H G) F))
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "naturality" $ \(GH (NatTransf h)) (fb :: G (b F)) ->
           bdistribute (h fb) === bmap (Compose . h . getCompose) (bdistribute fb)
      , testProperty "identity" $ \(b :: b F) ->
           bdistribute (Identity b) === bmap (Compose . Identity) b
      , testProperty "composition" $ \(fb :: H (G (b F))) ->
           bdistribute (Compose fb) === bmap (Compose . Compose . fmap getCompose . getCompose) (bdistribute . fmap bdistribute $ fb)
      ]
