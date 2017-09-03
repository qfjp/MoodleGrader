module Parser.NameList where

{-
Expects a list of duplicated names in a file called 'attendance.txt'
For example:

Daniel J PadeDaniel J Pade
Xian WuXian Wu
William J Sims IIIWilliam J Sims III
-}

import           Padelude
import           Unsafe                    (unsafeLast)

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.List                 ((!!))
import qualified Data.Text                 as T (length, lines, splitAt, take,
                                                 words)

import           Data.Name
import           Text.Numeral.Roman

showAll :: IO ()
showAll = runMaybeT getNames >>= print

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

getNames :: MaybeT IO [Name]
getNames
  = do
      names <- lift $ fmap (map halfIfDoub . T.lines) $ readFile "dubnames.txt"
      let (Identity inner) = runMaybeT $ mapM parseName names
      hoistMaybe inner
      --return $ mapM parseName $ names

parseName :: Text -> MaybeT Identity Name
parseName nameStr
  | length pieces == 2
      = return
      $ nFst .~ (pieces !! 0)
      $ nLst .~ (pieces !! 1)
      $ def
  | length pieces == 3
      = let lastPiece = unsafeLast pieces
            fstName = (pieces !!0)
            hasSuff
              =  isJust (fromRoman lastPiece :: Maybe Int)
              || lastPiece == "JR"
            midName = if hasSuff then Nothing else return $ pieces !! 1
            lstName = if hasSuff then pieces !! 1 else pieces !! 2
            sufName = if hasSuff then return $ pieces !! 2 else Nothing
        in return $ Name fstName midName lstName sufName
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

halfIfDoub :: Text -> Text
halfIfDoub s
  = if isDoub
    then T.take halfLen s
    else s
  where
    halfLen = T.length s `div` 2
    (fHalf, sHalf) = T.splitAt halfLen s
    isDoub = fHalf == sHalf
