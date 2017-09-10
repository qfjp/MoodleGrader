module Main where

import           Padelude

import qualified Brick.Main    as M

--import           Data.AppState.Monad
import           Data.AppState
import           Data.Name
import           Driver.Moodle as Driver
import           Ui            (initialState', theApp)

main :: IO ()
main
  = do
      --nameList <- runEitherT $ getNamesFromCsv "390.csv"
      --finalState <- case nameList of
      --  Left x -> Pre.error x
      --  Right (_, ns) -> M.defaultMain theApp (initialState ns :: AppState Text Name)
      initState <- initialState' :: IO (AppState Text Name)
      finalState <- M.defaultMain theApp initState
      mapM_ print $ (_marked finalState)
      --Driver.run Driver.testNames
