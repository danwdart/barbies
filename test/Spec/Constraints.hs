{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Constraints
  ( lawAddDictPrj
  )

where

import           Barbies.Constraints   (ClassF, Dict)
import           Clothes               (F)
import           Data.Functor.Barbie   (AllBF, ConstraintsB (..), bmap)

import           Data.Functor.Product  (Product (Pair))
import           Data.Typeable         (Proxy (..), Typeable, typeRep)

import           Test.Tasty            (TestTree)
import           Test.Tasty.QuickCheck (Arbitrary (..), testProperty, (===))


lawAddDictPrj
  :: forall b
  . ( ConstraintsB b, AllBF Show F b
    , Eq (b F)
    , Show (b F)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
lawAddDictPrj
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap second (baddDicts b :: b (Dict (ClassF Show F) `Product` F)) === b
  where
    second (Pair _ b) = b
