module Main where

import           Padelude
import qualified Prelude                   as Pre (error)

import qualified Brick.Main                as M
import           Control.Monad.Trans.Maybe

import           Control.PrettyShow
import           Data.AppState.Monad
import           Data.Name
import           Driver.Moodle             as Driver
import           Parser.NameList
import           Ui                        (initialState, theApp)

main :: IO ()
main
  = do
      nameList <- runMaybeT getNames
      finalState <- case nameList of
        Nothing -> Pre.error "ERROR: trouble parsing name list"
        Just ns -> M.defaultMain theApp (initialState ns :: AppState Nat Name)
      mapM_ print $ getMarks finalState
      Driver.run
