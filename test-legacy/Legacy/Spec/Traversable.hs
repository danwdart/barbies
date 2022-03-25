{-# LANGUAGE AllowAmbiguousTypes #-}
module Legacy.Spec.Traversable ( laws )

where

import           Legacy.Clothes        (F, FG (..), G, GH (..), H,
                                        NatTransf (..))

import           Data.Barbie           (TraversableB (..))

import           Data.Functor.Compose  (Compose (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (maybeToList)
import           Data.Typeable         (Proxy (..), Typeable, typeRep)

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), testProperty, (===))

laws
  :: forall b
  . ( TraversableB b
    , Eq (b F), Eq (b G), Eq (b H)
    , Show (b F), Show (b G), Show (b H)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [testProperty "naturality" $
        \b (FG (NatTransf fg)) ->
          let f = Just . fg
              t = maybeToList
          in (t . btraverse f) (b :: b F) === btraverse (t . f) (b :: b F)

      , testProperty "identity" $ \b ->
          btraverse Identity b === Identity (b :: b F)

      , testProperty "composition" $
          \b (FG (NatTransf fg)) (GH (NatTransf gh)) ->
            let f x = Just (fg x)
                g x = [gh x]
            in btraverse (Compose . fmap g . f) b ===
                 (Compose . fmap (btraverse g) . btraverse f) (b :: b F)
      ]
