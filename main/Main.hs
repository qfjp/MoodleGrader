module Main where

import           Padelude

import qualified Brick.Main                as M
import           Control.Monad.Trans.Maybe

import           Control.PrettyShow
import           Data.AppState.Monad
import           Data.Name
import           Parser.NameList
import           Ui                        (initialState, theApp)

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Ord, Show)

instance Enum Nat where
    fromEnum Zero     = 0
    fromEnum (Succ n) = 1 + fromEnum n

    toEnum 0 = Zero
    toEnum x = Succ (toEnum (x - 1))

instance Monoid Nat where
    mempty = Zero
    mappend Zero x     = x
    mappend (Succ x) y = mappend x (Succ y)


main :: IO ()
main
  = do
      nameList <- runMaybeT getNames
      finalState <- case nameList of
        Nothing -> error "ERROR: trouble parsing name list"
        Just ns -> M.defaultMain theApp (initialState ns :: AppState Nat Name)
      mapM_ pprint $ getMarks finalState
