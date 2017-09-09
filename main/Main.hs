module Main where

import           Padelude
import qualified Prelude                    as Pre (error)

import qualified Brick.Main                 as M
import           Control.Monad.Trans.Either

import           Data.AppState.Monad
import           Data.Name
import           Driver.Moodle              as Driver
import           Parser.NameList
import           Ui                         (initialState, theApp)

main :: IO ()
main
  = do
      nameList <- runEitherT $ getNamesFromCsv "390.csv"
      finalState <- case nameList of
        Left x -> Pre.error x
        Right (_, ns) -> M.defaultMain theApp (initialState ns :: AppState Nat Name)
      mapM_ print $ getMarks finalState
      Driver.run Driver.testNames
