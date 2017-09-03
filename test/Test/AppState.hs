{-# LANGUAGE ScopedTypeVariables #-}
module Test.AppState where

import           Padelude

import           Test.Hspec
import           Test.QuickCheck

import           Data.AppState
import qualified Data.Set.Monad  as S
import qualified Data.Vector     as V

import           Orphans         ()

unitTest :: IO ()
unitTest = hspec appStateTest

appStateTest :: SpecWith ()
appStateTest
  = describe "AppState operations" $
      it "union" $
          property $ \(lst1 :: [Int]) (mkd1 :: S.Set Int) (lst2 :: [Int]) (mkd2 :: S.Set Int) ->
             let mkList x = (V.fromList x)
             in appState () (mkList (lst1 ++ lst2)) (mkd1 `S.union` mkd2)
             == appState () (mkList lst1) mkd1
                  `union` appState () (mkList lst2) mkd2
