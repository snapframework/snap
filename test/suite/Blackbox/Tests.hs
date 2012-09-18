{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blackbox.Tests
  ( tests
  , remove
  , removeDir
  ) where

------------------------------------------------------------------------------
import           Control.Exception              (catch, throwIO)
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8          as S
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T
import qualified Network.HTTP.Conduit           as HTTP
import           Network.HTTP.Types             (Status(..))
import           Prelude                        hiding (catch)
import           System.Directory
import           System.FilePath
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test, path)

------------------------------------------------------------------------------




------------------------------------------------------------------------------
testServer :: String
testServer = "http://127.0.0.1"


------------------------------------------------------------------------------
testPort :: String
testPort = "9753"


------------------------------------------------------------------------------
-- | The server uri, without the leading slash.
testServerUri :: String
testServerUri = testServer ++ ":" ++ testPort


------------------------------------------------------------------------------
-- | The server url, with the leading slash.
testServerUrl :: String
testServerUrl = testServerUri ++ "/"


------------------------------------------------------------------------------
--
--                          --------------------
--                          -   TEST LOADER    -
--                          --------------------
--
------------------------------------------------------------------------------
tests :: Test
tests = testGroup "non-cabal-tests"
    [ requestTest "hello" "hello world"
    , requestTest "index" "index page\n"
    , requestTest "" "index page\n"
    , requestTest "splicepage" "splice page contents of the app splice\n"
    , requestTest "routeWithSplice" "routeWithSplice: foo snaplet data stringz"
    , requestTest "routeWithConfig" "routeWithConfig: topConfigValue"
    , requestTest "foo/foopage" "foo template page\n"
    , requestTest "foo/fooConfig" "fooValue"
    , requestTest "foo/fooRootUrl" "foo"
    , requestTest "barconfig" "barValue"
    , requestTest "bazpage" "baz template page <barsplice></barsplice>\n"
    , requestTest "bazpage2" "baz template page contents of the bar splice\n"
    , requestTest "bazpage3" "baz template page <barsplice></barsplice>\n"
    , requestTest "bazpage4" "baz template page <barsplice></barsplice>\n"
    , requestTest "barrooturl" "url"
    , requestExpectingError "bazbadpage" 500 "A web handler threw an exception. Details:\nTemplate \"cpyga\" not found."
    , requestTest "foo/fooSnapletName" "foosnaplet"

    , fooConfigPathTest

    -- Test the embedded snaplet
    , requestTest "embed/heist/embeddedpage" "embedded snaplet page <asplice></asplice>\n"
    , requestTest "embed/aoeuhtns" "embedded snaplet page splice value42\n"
    , requestTest "embed/heist/onemoredir/extra" "This is an extra template\n"

    -- This set of tests highlights the differences in the behavior of the
    -- get... functions from MonadSnaplet.
    , fooHandlerConfigTest
    , barHandlerConfigTest
    , bazpage5Test
    , bazConfigTest
    , requestTest "sessionDemo" "[(\"foo\",\"bar\")]\n"
    , reloadTest
    ]


------------------------------------------------------------------------------
testName :: String -> String
testName uri = "internal/" ++ uri


------------------------------------------------------------------------------
requestTest :: String -> Text -> Test
requestTest url desired = testCase (testName url) $ requestTest' url desired


------------------------------------------------------------------------------
requestTest' :: String -> Text -> IO ()
requestTest' url desired = do
    actual <- HTTP.simpleHttp $ testServerUrl ++ url
    assertEqual url desired (T.decodeUtf8 actual)


------------------------------------------------------------------------------
requestExpectingError :: String -> Int -> Text -> Test
requestExpectingError url status desired =
    testCase (testName url) $ requestExpectingError' url status desired


------------------------------------------------------------------------------
requestExpectingError' :: String -> Int -> Text -> IO ()
requestExpectingError' url status desired = do
    let fullUrl = testServerUrl ++ url
    req <- HTTP.parseUrl fullUrl
    let req' = req { HTTP.checkStatus = \_ _ -> Nothing }
    resp <- liftIO $ HTTP.withManager $ HTTP.httpLbs req'
    let b = HTTP.responseBody resp
        s = HTTP.responseStatus resp
    assertEqual ("Status code: "++fullUrl) status (statusCode s)
    assertEqual fullUrl desired (T.decodeUtf8 b)


------------------------------------------------------------------------------
fooConfigPathTest :: Test
fooConfigPathTest = testCase (testName "foo/fooFilePath") $ do
    b <- liftM L.unpack $ grab "/foo/fooFilePath"
    assertRelativelyTheSame b "non-cabal-appdir/snaplets/foosnaplet"


------------------------------------------------------------------------------
assertRelativelyTheSame :: FilePath -> FilePath -> IO ()
assertRelativelyTheSame p expected = do
    b <- makeRelativeToCurrentDirectory p
    assertEqual ("expected " ++ expected) expected b


------------------------------------------------------------------------------
grab :: MonadIO m => String -> m L.ByteString
grab path = liftIO $ HTTP.simpleHttp $ testServerUri ++ path


------------------------------------------------------------------------------
testWithCwd :: String
            -> (String -> L.ByteString -> Assertion)
            -> Test
testWithCwd uri f = testCase (testName uri) $
                    testWithCwd' uri f


------------------------------------------------------------------------------
testWithCwd' :: String
             -> (String -> L.ByteString -> Assertion)
             -> Assertion
testWithCwd' uri f = do
    b   <- grab slashUri
    cwd <- getCurrentDirectory

    f cwd b

  where
    slashUri = '/' : uri


------------------------------------------------------------------------------
fooHandlerConfigTest :: Test
fooHandlerConfigTest = testWithCwd "foo/handlerConfig" $ \cwd b -> do
    let response = L.fromChunks [ "([\"app\"],\""
                                , S.pack cwd
                                , "/non-cabal-appdir/snaplets/foosnaplet\","
                                , "Just \"foosnaplet\",\"A demonstration "
                                , "snaplet called foo.\",\"foo\")" ]
    assertEqual "" response b


------------------------------------------------------------------------------
barHandlerConfigTest :: Test
barHandlerConfigTest = testWithCwd "bar/handlerConfig" $ \cwd b -> do
    let response = L.fromChunks [ "([\"app\"],\""
                                , S.pack cwd
                                , "/non-cabal-appdir/snaplets/baz\","
                                , "Just \"baz\",\"An example snaplet called "
                                , "bar.\",\"\")" ]
    assertEqual "" response b


------------------------------------------------------------------------------
-- bazpage5 uses barsplice bound by renderWithSplices at request time
bazpage5Test :: Test
bazpage5Test = testWithCwd "bazpage5" $ \cwd b -> do
    let response = L.fromChunks [ "baz template page ([\"app\"],\""
                                , S.pack cwd
                                , "/non-cabal-appdir/snaplets/baz\","
                                , "Just \"baz\",\"An example snaplet called "
                                , "bar.\",\"\")\n" ]
    assertEqual "" response b


------------------------------------------------------------------------------
-- bazconfig uses two splices, appconfig and fooconfig. appconfig is bound with
-- the non type class version of addSplices in the main app initializer.
-- fooconfig is bound by addSplices in fooInit.
bazConfigTest :: Test
bazConfigTest = testWithCwd "bazconfig" $ \cwd b -> do
    let response = L.fromChunks [
                     "baz config page ([],\""
                   , S.pack cwd
                   , "/non-cabal-appdir\",Just \"app\","
                   , "\"Test application\",\"\") "
                   , "([\"app\"],\""
                   , S.pack cwd
                   , "/non-cabal-appdir/snaplets/foosnaplet\","
                   , "Just \"foosnaplet\",\"A demonstration snaplet "
                   , "called foo.\",\"foo\")\n"
                   ]

    assertEqual "" response b


------------------------------------------------------------------------------
expect404 :: String -> IO ()
expect404 url = action `catch` h
  where
    action = do
        HTTP.simpleHttp $ testServerUrl ++ url
        assertFailure "expected 404"

    h e@(HTTP.StatusCodeException (Status c _) _)
      | c == 404  = return ()
      | otherwise = throwIO e
    h e           = throwIO e


------------------------------------------------------------------------------
request404Test :: String -> Test
request404Test url = testCase (testName url) $ expect404 url


remove :: FilePath -> IO ()
remove f = do
    exists <- doesFileExist f
    when exists $ removeFile f


removeDir :: FilePath -> IO ()
removeDir d = do
    exists <- doesDirectoryExist d
    when exists $ removeDirectoryRecursive "non-cabal-appdir/snaplets/foosnaplet"


------------------------------------------------------------------------------
reloadTest :: Test
reloadTest = testCase "internal/reload-test" $ do
    let goodTplOrig = "non-cabal-appdir" </> "good.tpl"
    let badTplOrig  = "non-cabal-appdir" </> "bad.tpl"
    let goodTplNew  = "non-cabal-appdir" </> "snaplets"  </> "heist"
                                         </> "templates" </> "good.tpl"
    let badTplNew   = "non-cabal-appdir" </> "snaplets"  </> "heist"
                                         </> "templates" </> "bad.tpl"

    goodExists <- doesFileExist goodTplNew
    badExists  <- doesFileExist badTplNew

    assertBool "good.tpl exists" (not goodExists)
    assertBool "bad.tpl exists" (not badExists)
    expect404 "bad"

    copyFile badTplOrig badTplNew
    expect404 "good"
    expect404 "bad"

    testWithCwd' "admin/reload" $ \cwd' b -> do
        let cwd = S.pack cwd'
        let response =
                T.concat     [ "Error reloading site!\n\nInitializer "
                             , "threw an exception...\n"
                             , T.pack cwd'
                             , "/non-cabal-appdir/snaplets/heist"
                             , "/templates/bad.tpl \""
                             , T.pack cwd'
                             , "/non-cabal-appdir/snaplets/heist/templates"
                             , "/bad.tpl\" (line 2, column 1):\nunexpected "
                             , "end of input\nexpecting \"=\", \"/\" or "
                             , "\">\"\n\n...but before it died it generated "
                             , "the following output:\nInitializing app @ /\n"
                             , "Initializing heist @ /heist\n\n" ]
        assertEqual "admin/reload" response (T.decodeUtf8 b)

    remove badTplNew
    copyFile goodTplOrig goodTplNew

    testWithCwd' "admin/reload" $ \cwd' b -> do
        let cwd = S.pack cwd'
        let response =
                L.fromChunks [ "Initializing app @ /\n"
                             , "Initializing heist @ /heist\n"
                             , "...loaded 5 templates from "
                             , cwd
                             , "/non-cabal-appdir/snaplets/heist/templates\n"
                             , "Initializing foosnaplet @ /foo\n"
                             , "...adding 1 templates from "
                             , cwd
                             , "/non-cabal-appdir/snaplets/foosnaplet"
                             , "/templates with route prefix foo/\n"
                             , "Initializing baz @ /\n"
                             , "...adding 2 templates from "
                             , cwd
                             , "/non-cabal-appdir/snaplets/baz/templates "
                             , "with route prefix /\n"
                             , "Initializing CookieSession @ /session\n"
                             , "Initializing embedded @ /\n"
                             , "Initializing heist @ /heist\n"
                             , "...loaded 1 templates from "
                             , cwd
                             , "/non-cabal-appdir/snaplets/embedded"
                             , "/snaplets/heist/templates\n"
                             , "...adding 1 templates from "
                             , cwd
                             , "/non-cabal-appdir/snaplets/embedded"
                             , "/extra-templates with route prefix "
                             , "onemoredir/\n"
                             , "Site successfully reloaded.\n"
                             ]

        assertEqual "admin/reload" response b

    requestTest' "good" "Good template\n"


