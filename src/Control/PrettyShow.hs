module Control.PrettyShow where

import           Padelude

import           Data.Text

class PrettyShow a where
    pshow :: a -> Text

instance PrettyShow Int where
    pshow = pack . show

pprint :: PrettyShow a => a -> IO ()
pprint = print . pshow
