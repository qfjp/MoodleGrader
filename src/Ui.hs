{-# LANGUAGE ScopedTypeVariables #-}

module Ui where

import           Padelude

import qualified Graphics.Vty         as V

import           Lens.Micro           ((^.))

import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import           Brick.Types          (Widget)
import qualified Brick.Types          as T
import           Brick.Util           (bg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (hLimit, str, vBox, vLimit, withAttr,
                                       (<+>))
import qualified Brick.Widgets.List   as L

import qualified Data.Set.Monad       as S
import           Data.Text            (unpack)
import qualified Data.Vector          as Vec

import           Control.PrettyShow
import           Data.AppState.Monad

txt :: Text -> Widget a
txt = str . unpack

drawUi :: (Enum n, Monoid n, Ord n, Show n, Show a, Ord a, PrettyShow a)
       => AppState n a -> [Widget n]
drawUi s
  = [ui]
  where
    l = getBlist s
    label = txt "Item " <+> cur <+> txt " of " <+> total
    cur
      = case l ^. L.listSelectedL of
          Nothing -> txt "-"
          Just i  -> txt . pshow $ (i + 1)
    total
      = txt . pshow . Vec.length $ l ^. L.listElementsL
    leftBox
      = B.borderWithLabel label
      $ hLimit 25
      $ vLimit 15
      $ L.renderList (listDrawElement s) True l
    rightBox
      = B.borderWithLabel label
      $ hLimit 25
      $ vLimit 15
      $ L.renderList (listDrawElement s) True (L.list (succ mempty) Vec.empty 1)
    ui = C.vCenter
       $ vBox [ C.hCenter leftBox <+> rightBox
              , str " "
              , C.hCenter $ str "Students"
              ]

listDrawElement :: (Monoid n, Ord a, PrettyShow a)
                => AppState n a -> Bool -> a -> Widget n
listDrawElement p sel a
  = selStr . unpack . pshow $ a
  where
    set = getMarks p
    selStr s
      | sel && S.member a set = withAttr (markedAttr ++ L.listSelectedAttr) (str s)
      | sel = withAttr L.listSelectedAttr (str s)
      | S.member a set = withAttr markedAttr (str s)
      | otherwise = str s

appEvent :: forall n a b c. (Ord n, Ord a, Monoid n)
         => AppState n a -> T.BrickEvent b c -> T.EventM n (T.Next (AppState n a))
appEvent p (T.VtyEvent e)
  = case e of
      V.EvKey V.KEsc []
        -> M.halt p
      V.EvKey (V.KChar 'q') []
        -> M.halt p
      V.EvKey V.KEnter []
        ->  M.halt p
      V.EvKey (V.KChar ' ') []
        -> spacePress
      V.EvKey (V.KChar 'k') []
        -> modifyList (return . L.listMoveUp)
      V.EvKey (V.KChar 'j') []
        -> modifyList (return . L.listMoveDown)
      V.EvKey (V.KChar 'g') [] -> modifyList (return . L.listMoveTo 0)
      V.EvKey (V.KChar 'G') []
        -> modifyList (return . L.listMoveTo (Vec.length $ L.listElements l))
      ev
        -> modifyList (L.handleListEvent ev)
  where
    l = getBlist p
    v = getMarks p
    modifyList :: (BList n a -> T.EventM n (BList n a))
               -> T.EventM n (T.Next (AppState n a))
    modifyList f
      = fmap appState' (f l) <*> (return v) >>= M.continue
    spacePress :: T.EventM n (T.Next (AppState n a))
    spacePress
      = case L.listSelectedElement l of
          Nothing -> M.continue p
          Just (_, el)
            -> do
                let newSet = toggleElement el v
                    newState = appState' l newSet
                M.continue newState
appEvent p _ = M.continue p

toggleElement :: Ord a => a -> S.Set a -> S.Set a
toggleElement x set
  = if S.member x set
    then S.delete x set
    else S.insert x set


initialState :: (Monoid n, Ord a) => Vec.Vector a -> AppState n a
initialState names
  = appState
      mempty
      names
      mempty

markedAttr :: A.AttrName
markedAttr = A.attrName "marked"

theMap :: A.AttrMap
theMap
  = A.attrMap V.defAttr
  [ (L.listAttr,         V.white `on` V.brightWhite)
  , (L.listSelectedAttr, V.brightWhite `on` V.white)
  , (markedAttr,         bg V.black)
  ]

theApp :: (Enum n, Monoid n, Ord n, Show n, Show a, PrettyShow a, Ord a) => M.App (AppState n a) e n
theApp
  = M.App { M.appDraw = drawUi
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
