{-# LANGUAGE AllowAmbiguousTypes #-}
module Legacy.Spec.Bare ( laws )

where

import           Data.Barbie.Bare      (BareB (..), Covered)
import           Data.Functor.Identity

import           Data.Typeable         (Proxy (..), Typeable, typeRep)

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), testProperty, (===))

laws
  :: forall b
  . ( BareB b
    , Eq (b Covered Identity) , Show (b Covered Identity) , Arbitrary (b Covered Identity)
    -- , Show (b Bare Identity), Eq (b Bare Identity), Arbitrary (b Bare Identity)
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "bcover . bstrip = id" $ \b ->
          bcover (bstrip b) === (b :: b Covered Identity)

      -- TODO: FIXME
      -- , testProperty "bstrip . bcover = id" $ \b ->
      --     bstrip (bcover b) === (b :: b Bare)
      ]
