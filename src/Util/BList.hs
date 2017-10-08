module Util.BList where

import           Padelude

import           Brick.Types        (Widget)
import qualified Brick.Widgets.List as L

import           Data.Default
import qualified Data.Vector        as V

import           Lens.Micro         ((^.))

import           Control.PrettyShow
import           Data.AppState
import           Util.Text

getSelectedIndex :: BList n a -> Int
getSelectedIndex lst
  = case lst ^. L.listSelectedL of
      Nothing -> -1
      Just i  -> i

getSelectedElem :: (Default a) => BList n a -> a
getSelectedElem lst
  = if ix < 0 || ix >= V.length vec
    then def
    else vec V.! ix
  where vec = lst ^. L.listElementsL
        ix = getSelectedIndex lst

curIndexToWidget :: BList n a -> Widget n
curIndexToWidget
  = txt . pshow . (+1) . getSelectedIndex

lengthToWidget :: BList n a -> Widget n
lengthToWidget
  = txt . pshow . V.length . (^. L.listElementsL)
