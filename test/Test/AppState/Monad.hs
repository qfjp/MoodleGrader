{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.AppState.Monad where

import           Padelude                 hiding (empty)

import qualified Brick.Widgets.List       as L
import qualified Data.Set.Monad           as S
import qualified Data.Vector              as V

import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Data.AppState.Monad
import           Orphans                  ()

instance (Monoid n, Eq n, Eq a, Ord a) => EqProp (AppState n a) where
    (=-=) = eq

instance (Arbitrary n, Arbitrary a, Ord a) => Arbitrary (AppState n a) where
    arbitrary
      = appState <$> arbitrary <*> arbitrary <*> arbitrary

unitTest :: IO ()
unitTest
  = hspec $ do
      fundamentals
      describe "AppState laws" $ do
        testBatch $ functor (undefined :: AppState (Sum Int) (Bool, Char, Int))
        --testBatch $ monad (undefined :: AppState [Int] (Bool, Char, Int))
        --testBatch $ traversable (undefined :: AppState (Sum Int) (Bool, Char, Text))

fundamentals :: SpecWith ()
fundamentals
  = describe "Fundamental Operations" $ do
      it "empty" $ do
          (empty :: AppState () Int) `shouldBe` (appState () V.empty S.empty)
          getBlist (empty :: AppState () Int) `shouldBe` (L.list () V.empty 1)
      it "singleton->list" $
          property $ \x ->
              ((getBlist . singleton ()) (x :: Int))
                == ((flip (L.list ()) 1 . V.singleton) x)
      it "singleton->marked" $
          property $ \x ->
              ((getMarks . singleton ()) (x :: Int))
                == S.empty
