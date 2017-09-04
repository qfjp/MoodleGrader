{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.AppState where

import           Padelude           hiding (empty, foldr)
import qualified Padelude           as P (foldr)
import qualified Prelude            as Pre (error)

import           Brick.Widgets.List as L
import           Control.Lens       (makeLenses, (^.))
import qualified Data.Set.Monad     as S
import qualified Data.Vector        as V

import           Util.Vector

type BList n a
  = L.List n a

instance (Eq a, Eq n) => Eq (BList n a) where
    l1 == l2
      = (getElements l1 == getElements l2)
      && (getSelected l1 == getSelected l2)
      && (getName l1 == getName l2)
      && (getItemHeight l1 == getItemHeight l2)
      where
        getElements = (^. L.listElementsL)
        getSelected = (^. L.listSelectedL)
        getName = (^. L.listNameL)
        getItemHeight = (^. listItemHeightL)

instance (Ord a, Ord n) => Ord (BList n a) where
    l1 `compare` l2
      = (getElements l1 `compare` getElements l2)
      ++ (getSelected l1 `compare` getSelected l2)
      ++ (getName l1 `compare` getName l2)
      ++ (getItemHeight l1 `compare` getItemHeight l2)
      where
        getElements = (^. L.listElementsL)
        getSelected = (^. L.listSelectedL)
        getName = (^. L.listNameL)
        getItemHeight = (^. listItemHeightL)

data AppState n a
    = AppState { _blist  :: BList n a
               , _marked :: Set a
               }
    deriving (Show, Ord)

appState :: Ord a => n -> V.Vector a -> Set a -> AppState n a
appState n v = AppState (L.list n (vectorSort v) 1)

appState' :: Ord a => BList n a -> Set a -> AppState n a
appState' = AppState

instance (Ord a, Eq n) => Eq (AppState n a) where
    a == b
      = _blist a == _blist b
      && _marked a == _marked b

makeLenses ''AppState

foldr :: Ord a => (a -> b -> b) -> b -> AppState n a  -> b
foldr f def m
  = V.foldr f def items
  where
    items = (_blist m) ^. L.listElementsL

map :: (Ord a, Ord b) => (a -> b) -> AppState n a -> AppState n b
map f m
  = appState' (L.list lstName (V.map f lstItems) 1) (S.map f setItems)
  where
      lstItems = (_blist m) ^. L.listElementsL
      lstName = (_blist m) ^. L.listNameL
      setItems = _marked m

sequenceA :: AppState n (f a) -> f (AppState n a)
sequenceA = Pre.error "undefined"

singleton :: Ord a => n -> a -> AppState n a
singleton n = flip (appState n) S.empty . V.singleton

empty :: (Monoid n, Ord a) => AppState n a
empty = appState mempty V.empty S.empty

union :: (Monoid n, Ord a) => AppState n a -> AppState n a -> AppState n a
union a1 a2
  = appState (lName1 ++ lName2) combList combSet
  where
    list1 = a1 ^. blist ^. L.listElementsL
    list2 = a2 ^. blist ^. L.listElementsL
    lName1 = a1 ^. blist ^. L.listNameL
    lName2 = a2 ^. blist ^. L.listNameL
    mark1 = a1 ^. marked
    mark2 = a2 ^. marked
    combList = vectorSort (list1 ++ list2)
    combSet = mark1 ++ mark2

unions :: (Monoid n, Ord a) => [AppState n a] -> AppState n a
unions = P.foldr union empty

unionVector :: (Monoid n, Ord a) => V.Vector (AppState n a) -> AppState n a
unionVector = V.foldr union empty
