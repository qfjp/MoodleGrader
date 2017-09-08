{-# LANGUAGE ScopedTypeVariables #-}
module Driver.Moodle where

import           Padelude
import qualified Prelude                      as P ((!!))

import           Control.Monad.Trans.Either
import qualified Data.Text                    as T
import           Test.WebDriver               as S
import           Test.WebDriver.Class         as S
import           Test.WebDriver.Commands.Wait as S

import           Data.Name
import           Driver.Moodle.Parser


-- Data Types --

data Initial = FirstI Char | LastI Char

isFirst :: Initial -> Bool
isFirst (FirstI _) = True
isFirst _          = False

isLast :: Initial -> Bool
isLast = not . isFirst

initialChar :: Initial -> Char
initialChar (FirstI x) = x
initialChar (LastI x)  = x

-- Utility Functions --

rightsT :: forall e m a. Monad m => [EitherT e m a] -> EitherT e m [a]
rightsT
  = lift . fmap rights . mapM runEitherT

maybe2EitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybe2EitherT err mayb
  = case mayb of
      Just x  -> right x
      Nothing -> left err

tshow :: Show a => a -> Text
tshow = T.pack . show

-- Test Data --

username :: Text
username = undefined
password :: Text
password = undefined
section :: Text
section = undefined
assignment :: Text
assignment = undefined

sampleNames :: [Name]
sampleNames
  = undefined

driverConfig :: WDConfig
--driverConfig = useBrowser phantomjs defaultConfig
driverConfig = useBrowser chrome defaultConfig

findUniqueElem :: WebDriver wd => S.Selector -> EitherT Text wd Element
findUniqueElem selector
  = lift elemList >>= \x ->
      case x of
        []     -> left $ "No element " ++ show selector
        (y:[]) -> right y
        _      -> left $ "Too many elements from " ++ show selector
  where
      elemList :: WebDriver wd => wd [Element]
      elemList = findElems selector

findUniqueElemFrom :: WebDriver wd => S.Selector -> Element -> EitherT Text wd Element
findUniqueElemFrom selector el
  = do
      elemText <- lift $ getText el
      elemList >>= \x ->
        case x of
          []     -> left $ "No subelement "
                 ++ show selector ++ " under " ++ elemText
          (y:[]) -> right $ y
          _      -> left $ "Too many subelements "
                 ++ show selector ++ " under " ++ elemText
  where
      elemList :: WebDriver wd => EitherT Text wd [Element]
      elemList = lift . findElemsFrom el $ selector

login :: WebDriver wd => wd ()
login
  = do
    openPage "https://dropbox.cse.sc.edu/login/index.php"
    waitUntil 15 $
        expect . (== "https://dropbox.cse.sc.edu/login/index.php") =<< getCurrentURL
    usernameInput <- findElem ( ById "username" )
    passwordInput <- findElem ( ByCSS "#password" )
    loginButton <- findElem ( ByCSS "#loginbtn")
    sendKeys username usernameInput
    sendKeys password passwordInput
    submit loginButton

-- | Given that the webdriver is at the 'logged in' page, this moves the
-- to the course webpage.
courseLink :: WebDriver wd => EitherT Text wd Element
courseLink
  = do
      courseTitleDivs <- lift $ findElems (ByClass "course_title")
      results
        <- rightsT . map (findUniqueElemFrom (ByPartialLinkText section))
         $ courseTitleDivs
      case results of
          (x:[]) -> right x
          _      -> left "Couldn't find course link"

-- Assumes we are at the course webpage
-- Brings us to the link for the assignment
assignLink :: WebDriver wd => EitherT Text wd Element
assignLink
  = do
      mainDiv <- lift $ findElem (ByClass "weeks")
      results <- lift $ findElemsFrom mainDiv (ByLinkText assignment)
      case results of
          []     -> left "No assignment link available"
          (x:[]) -> right x
          _      -> left "Too many choices for assignment link"

-- Assumes we are at the link for the assignment
-- Brings us to the link for grading the assignment
allSubmissions :: WebDriver wd => wd ()
allSubmissions
  = findElem (ByLinkText "View all submissions") >>= click

-- | Given a name and a grade, saves that grade through moodle.
-- Assumes we are at the link for grading the assignment
gradeStudent :: WebDriver wd => Name -> Int -> EitherT Text wd Integer
gradeStudent name grade
  = do
      lift . click =<< firstInitButton
      lift . click =<< lastInitButton
      studentId <- findUniqueElem . ByLinkText . moodlePrint $ name
      hrefAttr
        <- maybe2EitherT
             "Could not find href attribute for given name" =<<
             (lift . flip attr "href" $ studentId)
      right $ parseUserId hrefAttr
  where
      firstInitial = T.head . _nFst $ name
      lastInitial = T.head . _nLst $ name
      selectInitial :: WebDriver wd => Initial -> EitherT Text wd Element
      selectInitial initial
        = do
            let
              cssSel
                = if isFirst initial
                  then ".initialbar.firstinitial"
                  else ".initialbar.lastinitial"
            alphabetSelectList'
              <- findUniqueElem (ByCSS cssSel)
            anySelected
              <- lift $
                  findElemsFrom alphabetSelectList' (ByLinkText "All")
            alphabetSelectList
              <- case listToMaybe anySelected of
                   Nothing -> return alphabetSelectList'
                   Just x -> (lift . click) x
                          >> findUniqueElem (ByCSS cssSel)
            flip findUniqueElemFrom alphabetSelectList
              . ByLinkText . T.singleton . initialChar $ initial
      -- | The Element that selects the first initial of the given
      -- name
      firstInitButton :: WebDriver wd => EitherT Text wd Element
      firstInitButton = selectInitial (FirstI firstInitial)
      lastInitButton :: WebDriver wd => EitherT Text wd Element
      lastInitButton = selectInitial (LastI lastInitial)

run :: IO ()
run
  = runSession driverConfig $ do
      login
      eitherT error click courseLink
      eitherT error click assignLink
      allSubmissions
      print =<< getCurrentURL
      urlid <- eitherT error return (gradeStudent (sampleNames P.!! 0) 0)
      print urlid
      print =<< getCurrentURL
      closeSession
