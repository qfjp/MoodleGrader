{-# LANGUAGE ScopedTypeVariables #-}
module Driver.Moodle where

import           Padelude

import           Control.Monad.Trans.Either
import qualified Data.Text                    as T
import           Data.Text.IO
import           Test.WebDriver               as S
import           Test.WebDriver.Class         as S
import           Test.WebDriver.Commands.Wait as S
import           Test.WebDriver.Config        as S

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

wd2Io :: (WebDriverConfig conf) => conf -> EitherT e WD a -> EitherT e IO a
wd2Io conf driver
  = EitherT (runSession conf (runEitherT driver))

-- Test Data --

username :: Text
username = undefined
password :: Text
password = undefined
testSection :: Text
testSection = undefined
testAssignment :: Text
testAssignment = undefined

testNames :: [(Name, Int)]
testNames
  = undefined

-- Purely internal --

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

findUniqueElemFrom :: WebDriver wd
                   => S.Selector -> Element -> EitherT Text wd Element
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

findUniqueElemFromEls :: WebDriver wd
                      => S.Selector -> [Element]
                      -> EitherT Text wd Element
findUniqueElemFromEls selector els
  = do
      results
        <- rightsT . map (findUniqueElemFrom selector)
         $ els
      case results of
          (x:[]) -> right x
          _      -> left "Couldn't find course link"
  where

-- | Given that the webdriver is at the 'logged in' page, this moves the
-- to the course webpage.
courseLink :: WebDriver wd => Text -> EitherT Text wd Element
courseLink section
  = do
      courseTitleDivs <- lift $ findElems (ByClass "course_title")
      findUniqueElemFromEls (ByPartialLinkText section) courseTitleDivs

-- Assumes we are at the course webpage
-- Brings us to the link for the assignment
assignLink :: WebDriver wd => Text -> EitherT Text wd Element
assignLink assignment
  = do
      mainDiv <- lift $ findElem (ByClass "weeks")
      findUniqueElemFrom (ByLinkText assignment) mainDiv

-- Assumes we are at the link for the assignment
-- Brings us to the link for grading the assignment
allSubmissionsLink :: WebDriver wd => EitherT Text wd Element
allSubmissionsLink
  = EitherT . fmap Right . findElem . ByLinkText
  $ "View all submissions"

-- Exportable --

login :: WebDriver wd => wd ()
login
  = do
    openPage "https://dropbox.cse.sc.edu/login/index.php"
    waitUntil 15 $
        expect . (== "https://dropbox.cse.sc.edu/login/index.php")
          =<< getCurrentURL
    usernameInput <- findElem ( ById "username" )
    passwordInput <- findElem ( ByCSS "#password" )
    loginButton <- findElem ( ByCSS "#loginbtn")
    sendKeys username usernameInput
    sendKeys password passwordInput
    submit loginButton

getCourses :: IO [Text]
getCourses
  = eitherT
      ((flip (>>)) (return []) . hPutStr stderr)
      return . wd2Io driverConfig $ do
        lift login
        lift gotoMainPage
        courseTitleDivs <- lift $ findElems (ByClass "course_title")
        courseList <- lift $ mapM getText courseTitleDivs
        lift closeSession
        return courseList

gotoAllSubmissions :: WebDriver wd => EitherT Text wd ()
gotoAllSubmissions
  = do
      lift gotoMainPage
      lift . click =<< courseLink testSection
      lift . click =<< assignLink testAssignment
      lift . click =<< allSubmissionsLink

gotoMainPage :: WebDriver wd => wd ()
gotoMainPage = openPage "https://dropbox.cse.sc.edu/my/"

noNotifyOpt :: WebDriver wd => EitherT Text wd ()
noNotifyOpt
  = do
      noOpt
        <- findUniqueElem . ByCSS
         $ "#id_sendstudentnotifications option[value=\"0\"]"
      lift $ click noOpt

-- | Given a name and a grade, saves that grade through moodle.
-- Assumes we are at the link for grading the assignment
gradeStudent :: WebDriver wd => Name -> Int -> EitherT Text wd ()
gradeStudent name grade
  = do
      lift . click =<< firstInitButton
      lift . click =<< lastInitButton
      studentId <- findUniqueElem . ByLinkText . moodlePrint $ name
      hrefAttr
        <- maybe2EitherT
             "Could not find href attribute for given name" =<<
             (lift . flip attr "href" $ studentId)
      let userid = parseUserId hrefAttr
          gradeElementId = "quickgrade_" ++ tshow userid
      gradeElement <- findUniqueElem . ById $ gradeElementId
      saveElement <- findUniqueElem . ById $ "id_savequickgrades"
      lift $ sendKeys (tshow grade) gradeElement
      noNotifyOpt
      lift $ submit saveElement -- TODO: Test before using this!
  where
      firstInitial = T.head . _nFst $ name
      lastInitial = T.head . _nLst $ name
      selectInitial :: WebDriver wd
                    => Initial -> EitherT Text wd Element
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

run :: [(Name, Int)] -> IO ()
run grades
  = runSession driverConfig . eitherT error return $ do
      lift login
      gotoAllSubmissions
      print =<< lift getCurrentURL
      mapM_ ((>> gotoAllSubmissions) . uncurry gradeStudent) grades
      print =<< lift getCurrentURL
      lift closeSession
