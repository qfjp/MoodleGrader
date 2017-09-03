module Test.Util.Vector where


import           Padelude

import qualified Data.Vector     as V

import           Test.Hspec
import           Test.QuickCheck

import           Util.Vector

unitTest :: IO ()
unitTest
  = hspec mergeSortTest

mergeSortTest :: SpecWith ()
mergeSortTest
  = describe "Vector Sort" $
      it "Vector Algo Library" $
          property $ \lst ->
              (sort (lst :: [Int])) == ((V.toList . vectorSort . V.fromList) lst)
