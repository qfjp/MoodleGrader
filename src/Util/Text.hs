module Util.Text where

import           Padelude

import           Brick.Types        (Widget)
import           Brick.Widgets.Core (str)

import qualified Data.Text          as T


tshow :: Show a => a -> Text
tshow = T.pack . show

txt :: Text -> Widget a
txt = str . T.unpack
