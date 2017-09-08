module Driver.Moodle.Parser where

import           Padelude

import           Data.Text     (unpack)
import           Text.Trifecta

-- | Parses the user id from the URL of the attribute of the div that
-- contains that user's name.
parseUserId :: Text -> Integer
parseUserId url
  = case parseString parser mempty (unpack url) of
      Success x -> x
      _         -> error ""
  where
    parser :: Parser Integer
    parser
      = do
          void $ text "https://dropbox.cse.sc.edu/user/view.php?id="
          uid <- integer
          void $ text "&course="
          void $ integer
          return uid
