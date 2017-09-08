{-# LANGUAGE TemplateHaskell #-}
module Data.Name where

import           Padelude

import           Control.Lens
import           Control.PrettyShow
import           Data.Default

data Name
    = Name { _nFst :: Text
           , _nMid :: Maybe Text
           , _nLst :: Text
           , _nSuf :: Maybe Text
           --, _nEml :: Text
           }
    deriving (Show, Eq, Ord)

makeLenses ''Name

instance PrettyShow Name where
    pshow name
      = name ^. nLst ++ suf ++ ", " ++ name ^. nFst ++ mid
      where
        mid = maybe "" (" " ++) (name ^. nMid)
        suf = maybe "" (" " ++) (name ^. nSuf)

instance Default Name where
    def = Name "" Nothing "" Nothing


moodlePrint :: Name -> Text
moodlePrint name
  =  name ^. nFst
  ++ mid
  ++ " " ++ name ^. nLst
  ++ suf
  where
    mid = maybe "" (" " ++) (name ^. nMid)
    suf = maybe "" (" " ++) (name ^. nSuf)
