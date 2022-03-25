{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Legacy.Spec.Wrapper (
    lawsMonoid
  )

where

import           Data.Barbie           (AllBF, Barbie (..), ProductBC)

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), testProperty)

lawsMonoid
  :: forall b
  .  ( Arbitrary (b []), Eq (b []), Show (b [])
     , ProductBC b
     , AllBF Semigroup [] b
     , AllBF Monoid [] b
     )
  => TestTree
lawsMonoid
  = testGroup "Monoid laws"
      [ testProperty "neutral element" $ \b ->
          unwrap (Barbie b) == b &&
          unwrap (Barbie b) == b

      , testProperty "associativity" $ \b1 b2 b3 ->
          unwrap ((Barbie b1 <>  Barbie b2) <> Barbie b3) ==
          unwrap ( Barbie b1 <> (Barbie b2  <> Barbie b3))
      ]
  where
    unwrap = getBarbie :: Barbie b [] -> b []


instance Arbitrary (b f) => Arbitrary (Barbie b f) where
    arbitrary = Barbie <$> arbitrary
