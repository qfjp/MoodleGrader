{-# LANGUAGE TemplateHaskell #-}
module Data.Name where

import           Padelude

import           Data.Csv           hiding (Name)
import           Data.Default
import qualified Data.Text          as T
import           Lens.Micro         ((^.))
import           Lens.Micro.TH      (makeLenses)

import           Control.MoodleShow
import           Control.PrettyShow

data Name
    = Name { _nFst :: Text
           , _nMid :: Maybe Text
           , _nLst :: Text
           , _nSuf :: Maybe Text
           , _nEml :: Text
           }
    deriving (Show, Eq)

instance Ord Name where
    n1 `compare` n2
      | _nLst n1 < _nLst n2
          = LT
      | _nLst n1 == _nLst n2 && _nFst n1 < _nFst n2
          = LT
      | _nLst n1 == _nLst n2 && _nFst n1 == _nFst n2 && _nMid n1 < _nMid n2
          = LT
      | _nLst n1 == _nLst n2 && _nFst n1 == _nFst n2 && _nMid n1 == _nMid n2
          = EQ
      | otherwise = GT

makeLenses ''Name

instance PrettyShow Name where
    pshow name
      = name ^. nLst ++ suf ++ ", " ++ name ^. nFst ++ mid
      where
        mid = maybe "" (" " ++) (name ^. nMid)
        suf = maybe "" (" " ++) (name ^. nSuf)

instance FromNamedRecord Name where
    parseNamedRecord m
      = do
          (frst, mid) <- splitName <$> m .: "First name"
          (last, suff) <- splitName <$> m .: "Surname"
          return
              Name { _nFst = frst
                   , _nMid = mid
                   , _nLst = last
                   , _nSuf = suff
                   , _nEml = ""
                   }
      where
        splitName :: Text -> (Text, Maybe Text)
        splitName name
          = case T.words name of
              [x]    -> (x, Nothing)
              [x, y] -> (x, Just y)
              _      -> error "ERROR in parsing name"

instance Default Name where
    def = Name "" Nothing "" Nothing ""


instance MoodleShow Name where
  moodleShow name
    =  name ^. nFst
    ++ mid
    ++ " " ++ name ^. nLst
    ++ suf
    where
      mid = maybe "" (" " ++) (name ^. nMid)
      suf = maybe "" (" " ++) (name ^. nSuf)
