module Parser.NameList where

import           Padelude
import           Prelude                    (String)

import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy       as BS
import           Data.Csv                   hiding (Name)
import qualified Data.Vector                as V

import           Data.Name

getNamesFromCsv :: FilePath -> EitherT String IO (Header, V.Vector Name)
getNamesFromCsv csvFileName
  = EitherT $ do
      fileContents <- BS.readFile csvFileName
      return $ decodeByName fileContents
