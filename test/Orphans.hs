{-# LANGUAGE FlexibleInstances #-}
module Orphans where

import           Padelude

import           Test.QuickCheck
import           Test.QuickCheck.Checkers

import qualified Brick.Widgets.List       as L
import qualified Data.Set.Monad           as S
import qualified Data.Text                as T
import qualified Data.Vector              as V

instance (Arbitrary a, Ord a) => Arbitrary (L.List () a) where
    arbitrary = fmap (flip (L.list ()) 0 . V.fromList) orderedList

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = S.fromList <$> arbitrary

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance EqProp Text where
    (=-=) = eq
