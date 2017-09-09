{-# LANGUAGE TemplateHaskell #-}
module Data.Name where

import           Padelude

import           Control.Lens
import           Control.PrettyShow
import           Data.Csv           hiding (Name)
import           Data.Default
import qualified Data.Text          as T

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

instance FromNamedRecord Name where
    parseNamedRecord m
      = do
          (frst, mid) <- splitName <$> m .: "First name"
          (last, suff) <- splitName <$> m .: "Surname"
          return $
              Name { _nFst = frst
                   , _nMid = mid
                   , _nLst = last
                   , _nSuf = suff
                   }
      where
        splitName :: Text -> (Text, Maybe Text)
        splitName name
          = case T.words name of
              (x:[])   -> (x, Nothing)
              (x:y:[]) -> (x, Just y)
              _        -> error "ERROR in parsing name"

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
