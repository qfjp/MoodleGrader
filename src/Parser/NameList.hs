module Parser.NameList where

import           Padelude
import           Prelude                    as P (String, last, (!!))

import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy       as BS
import           Data.Csv                   hiding (Name)
import           Data.Default
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Lens.Micro                 ((.~))
import           Text.Numeral.Roman

import           Data.Name

getNamesFromCsv :: FilePath -> EitherT String IO (Header, V.Vector Name)
getNamesFromCsv csvFileName
  = EitherT $ do
      fileContents <- BS.readFile csvFileName
      return $ decodeByName fileContents

parseName :: Text -> Maybe Name
parseName nameStr
  | length pieces == 2
      = return
      $ nFst .~ (pieces !! 0)
      $ nLst .~ (pieces !! 1)
      $ def
  | length pieces == 3
      = let lastPiece = P.last pieces
            fstName = (pieces !!0)
            hasSuff
              =  isJust (fromRoman lastPiece :: Maybe Int)
              || lastPiece == "JR"
            midName = if hasSuff then Nothing else return $ pieces !! 1
            lstName = if hasSuff then pieces !! 1 else pieces !! 2
            sufName = if hasSuff then return $ pieces !! 2 else Nothing
        in return $ Name fstName midName lstName sufName ""
  | length pieces == 4
      = return
      $ nFst .~ (pieces !! 0)
      $ nMid .~ (return $ pieces !! 1)
      $ nLst .~ (pieces !! 2)
      $ nSuf .~ (return $ pieces !! 3)
      $ def
  | otherwise = mzero
  where
    pieces :: [Text]
    pieces
      = T.words nameStr
