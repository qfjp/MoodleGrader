{-# LANGUAGE TemplateHaskell #-}
module Data.Course where

import           Padelude

import qualified Prelude            as P (show)

import           Data.Default
import qualified Data.Text          as T
import           Lens.Micro.TH
import           Test.WebDriver     (Element (..))
import           Text.Printf

import           Control.MoodleShow
import           Control.PrettyShow
import           Util.Text

data Season
  = Unknown | Fall | Spring | Summer
  deriving (Eq, Ord)

instance Show Season where
    show Unknown = "Unk"
    show Fall    = "F"
    show Spring  = "S"
    show Summer  = "Su"

data Course
  = Course
      { _name    :: Text
      , _number  :: Int
      , _section :: Int
      , _season  :: Season
      , _year    :: Int
      , _url     :: Text
      }
  deriving (Eq, Ord, Show)

instance Default Course where
    def = Course "" 0 0 Unknown 0 ""

instance PrettyShow Course where
    pshow x = tshow (_number x) ++ " - " ++ _name x

instance MoodleShow Course where
    moodleShow
      = do
          name <- _name
          number <- tshow . _number
          section <- T.pack . printf "%03d" . _section
          season <- tshow . _season
          year <- tshow . _year
          return $ name
                 ++ " (" ++ number
                 ++ "-" ++ section
                 ++ "-" ++ season
                 ++ year ++ ")"

--moodlePrint :: Course -> Text
--moodlePrint
--  = do
--      name <- _name
--      return name

makeLenses ''Course
