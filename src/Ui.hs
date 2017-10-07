{-# LANGUAGE ScopedTypeVariables #-}

module Ui where

import           Padelude

import qualified Graphics.Vty               as V

import           Lens.Micro                 ((.~), (^.))

import qualified Brick.AttrMap              as A
import qualified Brick.Main                 as M
import           Brick.Types                (Widget)
import qualified Brick.Types                as T
import           Brick.Util                 (bg, on)
import qualified Brick.Widgets.Border       as B
import           Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center       as C
import           Brick.Widgets.Core         (emptyWidget, hBox, hLimit, str,
                                             vBox, vLimit, withAttr,
                                             withBorderStyle, (<+>), (<=>))
import qualified Brick.Widgets.Dialog       as D
import qualified Brick.Widgets.List         as L

import           Data.Default
import qualified Data.Set.Monad             as S
import           Data.Text                  (unpack)
import qualified Data.Vector                as Vec

import           Control.PrettyShow
import           Data.AppState
import           Data.Course
import           Data.Name
import           Driver.Moodle              (getCourses, getStudents)
import           Util.BList
import           Util.Text
import           Util.Vector                (vectorSort)

drawUi :: forall n a.
          (Ord n, Show n, Monoid n, IsString n, Show a, Ord a, PrettyShow a)
       => AppState n a -> [Widget n]
drawUi s
  = [ui]
  where
    studentList = s ^. students
    courseList :: BList n Course
    courseList = s ^. courses
    curStudent = curIndexToWidget studentList
    totalStudents = lengthToWidget studentList
    curCourse = curIndexToWidget courseList
    totalCourses = lengthToWidget courseList
    studentBoxTop :: Widget n
    studentBoxTop
      =  hLimit 25
      $ vLimit 15
      $ L.renderList (listDrawElement s) True studentList
    courseBox :: Widget n
    courseBox
      = hLimit 25
      $ vLimit 15
      $ L.renderList courseListDraw True courseList
    courseBord = if (s ^. focused) == Courses then unicodeBold else unicodeRounded
    studentBord = if (s ^. focused) == Students then unicodeBold else unicodeRounded
    ui = C.vCenter
       $ vBox [ hBox [ withBorderStyle courseBord $ B.borderWithLabel (txt "Courses (c)") $
                         (C.hCenter courseBox) <=> (C.hCenter $ curCourse <+> txt " of " <+> totalCourses)
                     , withBorderStyle studentBord $ B.borderWithLabel (txt "Students (s)") $
                         (C.hCenter studentBoxTop) <=>
                             (C.hCenter $ curStudent <+> txt " of " <+> totalStudents)
                     ]
              , str " "
              , C.hCenter $ txt "Moodle Grader"
              ]

courseListDraw :: Bool -> Course -> Widget n
courseListDraw sel a
  = txt $ pshow a

listDrawElement :: (Monoid n, Ord a, PrettyShow a)
                => AppState n a -> Bool -> a -> Widget n
listDrawElement p sel a
  = selStr . unpack . pshow $ a
  where
    set = p ^. marked
    selStr s
      | sel && S.member a set
          = withAttr (markedAttr ++ L.listSelectedAttr) (str s)
      | sel = withAttr L.listSelectedAttr (str s)
      | S.member a set = withAttr markedAttr (str s)
      | otherwise = str s



appEvent :: forall n b c. (Ord n, Monoid n, IsString n)
         => AppState n Name -> T.BrickEvent b c
         -> T.EventM n (T.Next (AppState n Name))
appEvent p (T.VtyEvent (V.EvKey (V.KChar 'q') []))
  = M.halt p
appEvent p (T.VtyEvent e)
  = case p ^. focused of
      None
        -> case e of
             V.EvKey (V.KChar 's') []
               -> M.continue (focused .~ Students $ p)
             V.EvKey (V.KChar 'c') []
               -> M.continue (focused .~ Courses $ p)
             ev -> M.continue p
      Assignments
        -> undefined
      Courses
        -> case e of
             V.EvKey V.KEnter []
              -> retrieveStudents p . getSelectedElem $ courseList
             V.EvKey (V.KChar 'c') []
               -> M.continue p
             V.EvKey (V.KChar 's') []
               -> M.continue (focused .~ Students $ p)
             V.EvKey (V.KChar 'k') []
               -> modifyCourseList L.listMoveUp
             V.EvKey (V.KChar 'j') []
               -> modifyCourseList L.listMoveDown
             V.EvKey (V.KChar 'g') [] -> modifyCourseList (L.listMoveTo 0)
             V.EvKey (V.KChar 'G') []
               -> modifyCourseList . L.listMoveTo . Vec.length
                . L.listElements $ courseList
             ev
               -> M.continue p
      Students
        -> case e of
             V.EvKey (V.KChar 'c') []
               -> M.continue (focused .~ Courses $ p)
             V.EvKey V.KEsc []
               -> M.halt p
             V.EvKey V.KEnter []
               ->  M.halt p
             V.EvKey (V.KChar ' ') []
               -> spacePress
             V.EvKey (V.KChar 'k') []
               -> modifyStudentList L.listMoveUp
             V.EvKey (V.KChar 'j') []
               -> modifyStudentList L.listMoveDown
             V.EvKey (V.KChar 'g') [] -> modifyStudentList (L.listMoveTo 0)
             V.EvKey (V.KChar 'G') []
               -> modifyStudentList . L.listMoveTo . Vec.length
                $ L.listElements studentList
             ev
               -> do
                   newList <- L.handleListEvent ev studentList
                   M.continue (students .~ newList $ p)
                   --modifyList (L.handleListEvent ev)
  where
    studentList = p ^. students
    courseList = p ^. courses
    markedSet = p ^. marked
    modifyCourseList :: (BList n Course -> BList n Course)
                      -> T.EventM n (T.Next (AppState n Name))
    modifyCourseList f
      = M.continue (courses .~ f courseList $ p)
    modifyStudentList :: (BList n Name -> BList n Name)
                      -> T.EventM n (T.Next (AppState n Name))
    modifyStudentList f
      = M.continue (students .~ f studentList $ p)
    spacePress :: T.EventM n (T.Next (AppState n Name))
    spacePress
      = case L.listSelectedElement studentList of
          Nothing -> M.continue p
          Just (_, el)
            -> do
                let newSet = toggleElement el markedSet
                    newState = marked .~ newSet $ p
                M.continue newState
appEvent p _ = M.continue p

retrieveStudents :: (IsString n) => AppState n Name -> Course -> T.EventM n (T.Next (AppState n Name))
retrieveStudents p course
  = do
      studentLst <- liftIO $ getStudents course
      let studentBlst = L.list "students" (Vec.fromList studentLst) 1
      M.continue (students .~ studentBlst $ p)

toggleElement :: Ord a => a -> S.Set a -> S.Set a
toggleElement x set
  = if S.member x set
    then S.delete x set
    else S.insert x set

initialState' :: (IsString n, Monoid n, Ord a) => IO (AppState n a)
initialState'
  = do
      courseList <- getCourses
      return
        $ courses .~ L.list "courses" (Vec.fromList courseList) 1
        $ def

markedAttr :: A.AttrName
markedAttr = A.attrName "marked"

theMap :: A.AttrMap
theMap
  = A.attrMap V.defAttr
  [ (L.listAttr,         V.white `on` V.brightWhite)
  , (L.listSelectedAttr, V.brightWhite `on` V.white)
  , (markedAttr,         bg V.black)
  ]

theApp :: (IsString n, Monoid n, Ord n, Show n)
       => M.App (AppState n Name) e n
theApp
  = M.App { M.appDraw = drawUi
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
