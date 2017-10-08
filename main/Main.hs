module Main where

import           Padelude

import qualified Brick.Main    as M

import           Data.AppState
import           Data.Name
import           Ui            (initialState', theApp)

main :: IO ()
main
  = do
      initState <- initialState' :: IO (AppState Text Name)
      finalState <- M.defaultMain theApp initState
      mapM_ print $ _marked finalState
