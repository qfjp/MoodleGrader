module Control.MoodleShow where

import           Padelude

class MoodleShow a where
    moodleShow :: a -> Text
