module Driver.Moodle.Parser where

import           Padelude
import qualified Prelude        as Pre

import           Data.Text      (pack, strip, unpack)
import           Test.WebDriver (Element)
import           Text.Trifecta

import           Data.Course

-- | Parses the user id from the URL of the attribute of the div that
-- contains that user's name.
parseUserId :: Text -> Integer
parseUserId url
  = case parseString parser mempty (unpack url) of
      Success x -> x
      _         -> Pre.error ""
  where
    parser :: Parser Integer
    parser
      = do
          void $ text "https://dropbox.cse.sc.edu/user/view.php?id="
          uid <- integer
          void $ text "&course="
          void integer
          return uid

parseCourseId :: Text -> Text -> Course
parseCourseId uri fullText
  = case parseString parser mempty (unpack fullText) of
      Success x -> x
      _         -> Pre.error ""
  where
      parser :: Parser Course
      parser
        = do
            thisName <- many $ noneOf "("
            void $ oneOf "("
            thisNumber <- natural
            void . oneOf $ "-"
            thisSection <- natural
            void . oneOf $ "-"
            thisSeasonChars <- many $ noneOf "0123456789"
            thisYear <- natural
            void $ oneOf ")"
            return $ Course
              { _name = strip . pack $ thisName
              , _number = fromIntegral thisNumber
              , _section = fromIntegral thisSection
              , _year = fromIntegral thisYear
              , _season = case thisSeasonChars of
                            "F"  -> Fall
                            "S"  -> Spring
                            "Su" -> Summer
                            _    -> Unknown
              , _url = uri
              }
