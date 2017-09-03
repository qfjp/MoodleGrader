module Main where

import           Padelude

import qualified Test.AppState       as A
import qualified Test.AppState.Monad as AM
import qualified Test.Util.Vector    as TU

main :: IO ()
main =
    do
        TU.unitTest
        A.unitTest
        AM.unitTest
