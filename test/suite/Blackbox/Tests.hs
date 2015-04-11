{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blackbox.Tests
  ( tests
  , remove
  , removeDir
  ) where

------------------------------------------------------------------------------
import           Control.Exception              (catch, finally, throwIO)
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8          as S
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.Monoid
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T
import           Network.Http.Client
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


                            --------------------
                            --  TEST LOADER   --
                            --------------------

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
--testName = id

------------------------------------------------------------------------------
requestTest :: String -> Text -> Test
requestTest url desired = testCase (testName url) $ requestTest' url desired


------------------------------------------------------------------------------
requestTest' :: String -> Text -> IO ()
requestTest' url desired = do
    actual <- get (S.pack $ testServerUrl ++ url) concatHandler
    assertEqual url desired (T.decodeUtf8 $ L.fromChunks [actual])


------------------------------------------------------------------------------
requestExpectingError :: String -> Int -> Text -> Test
requestExpectingError url status desired =
    testCase (testName url) $ requestExpectingError' url status desired


------------------------------------------------------------------------------
requestExpectingError' :: String -> Int -> Text -> IO ()
requestExpectingError' url status desired = do
    let fullUrl = testServerUrl ++ url
    get (S.pack fullUrl) $ \resp is -> do
      assertEqual ("Status code: "++fullUrl) status
                  (getStatusCode resp)
      res <- concatHandler resp is
      assertEqual fullUrl desired (T.decodeUtf8 $ L.fromChunks [res])


------------------------------------------------------------------------------
fooConfigPathTest :: Test
fooConfigPathTest = testCase (testName "foo/fooFilePath") $ do
    b <- liftM L.unpack $ grab "/foo/fooFilePath"
    assertRelativelyTheSame b "snaplets/foosnaplet"


------------------------------------------------------------------------------
assertRelativelyTheSame :: FilePath -> FilePath -> IO ()
assertRelativelyTheSame p expected = do
    b <- makeRelativeToCurrentDirectory p
    assertEqual ("expected " ++ expected) expected b


------------------------------------------------------------------------------
grab :: MonadIO m => String -> m L.ByteString
grab path = liftIO $ liftM (L.fromChunks . (:[])) $
  get (S.pack $ testServerUri ++ path) concatHandler


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
                                , "/snaplets/foosnaplet\","
                                , "Just \"foosnaplet\",\"A demonstration "
                                , "snaplet called foo.\",\"foo\")" ]
    assertEqual "" response b


------------------------------------------------------------------------------
barHandlerConfigTest :: Test
barHandlerConfigTest = testWithCwd "bar/handlerConfig" $ \cwd b -> do
    let response = L.fromChunks [ "([\"app\"],\""
                                , S.pack cwd
                                , "/snaplets/baz\","
                                , "Just \"baz\",\"An example snaplet called "
                                , "bar.\",\"\")" ]
    assertEqual "" response b


------------------------------------------------------------------------------
-- bazpage5 uses barsplice bound by renderWithSplices at request time
bazpage5Test :: Test
bazpage5Test = testWithCwd "bazpage5" $ \cwd b -> do
    let response = L.fromChunks [ "baz template page ([\"app\"],\""
                                , S.pack cwd
                                , "/snaplets/baz\","
                                , "Just \"baz\",\"An example snaplet called "
                                , "bar.\",\"\")\n" ]
    assertEqual "" (T.decodeUtf8 response) (T.decodeUtf8 b)


------------------------------------------------------------------------------
-- bazconfig uses two splices, appconfig and fooconfig. appconfig is bound with
-- the non type class version of addSplices in the main app initializer.
-- fooconfig is bound by addSplices in fooInit.
bazConfigTest :: Test
bazConfigTest = testWithCwd "bazconfig" $ \cwd b -> do
    let response = L.fromChunks [
                     "baz config page ([],\""
                   , S.pack cwd
                   , "\",Just \"app\"," -- TODO, right?
                   , "\"Test application\",\"\") "
                   , "([\"app\"],\""
                   , S.pack cwd
                   , "/snaplets/foosnaplet\","
                   , "Just \"foosnaplet\",\"A demonstration snaplet "
                   , "called foo.\",\"foo\")\n"
                   ]

    assertEqual "" (T.decodeUtf8 response) (T.decodeUtf8 b)


------------------------------------------------------------------------------
expect404 :: String -> IO ()
expect404 url = do
    get (S.pack $ testServerUrl ++ url) $ \resp i -> do
        case getStatusCode resp of
          404 -> return ()
          _   -> assertFailure "expected 404"


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
    when exists $ removeDirectoryRecursive "snaplets/foosnaplet"


------------------------------------------------------------------------------
reloadTest :: Test
reloadTest = testCase "internal/reload-test" $ do
    let goodTplOrig = "good.tpl"
    let badTplOrig  = "bad.tpl"
    let goodTplNew  = "snaplets"  </> "heist"
                      </> "templates" </> "good.tpl"
    let badTplNew   = "snaplets"  </> "heist"
                      </> "templates" </> "bad.tpl"

    goodExists <- doesFileExist goodTplNew
    badExists  <- doesFileExist badTplNew

    assertBool "good.tpl exists" (not goodExists)
    assertBool "bad.tpl exists"  (not badExists)
    expect404 "bad"

    copyFile badTplOrig badTplNew
    expect404 "good"
    expect404 "bad"

    flip finally (remove badTplNew) $
      testWithCwd' "admin/reload" $ \cwd' b -> do
        let cwd = S.pack cwd'
        let response =
                T.concat     [ "Error reloading site!\n\nInitializer "
                             , "threw an exception...\n"
                             , T.pack cwd'
                             , "/snaplets/heist"
                             , "/templates/bad.tpl \""
                             , T.pack cwd'
                             , "/snaplets/heist/templates"
                             , "/bad.tpl\" (line 2, column 1):\nunexpected "
                             , "end of input\nexpecting \"=\", \"/\" or "
                             , "\">\"\n\n...but before it died it generated "
                             , "the following output:\nInitializing app @ /\n"
                             , "Initializing heist @ /heist\n\n" ]
        assertEqual "admin/reload" response (T.decodeUtf8 b)

    copyFile goodTplOrig goodTplNew

    testWithCwd' "admin/reload" $ \cwd' b -> do  -- TODO/NOTE: Needs cleanup
        let cwd = S.pack cwd'
        let response = L.fromChunks [
              "Initializing app @ /\nInitializing heist @ ",
              "/heist\n...loaded 9 templates from ",
              cwd,
              "/snaplets/heist/templates\nInitializing CookieSession ",
              "@ /session\nInitializing foosnaplet @ /foo\n...adding 1 ",
              "templates from ",
              cwd,
              "/snaplets/foosnaplet/templates with route prefix ",
              "foo/\nInitializing baz @ /\n...adding 2 templates from ",
              cwd,
              "/snaplets/baz/templates with route prefix /\nInitializing ",
              "embedded @ /\nInitializing heist @ /heist\n...loaded ",
              "1 templates from ",
              cwd,
              "/snaplets/embedded/snaplets/heist/templates\n...adding ",
              "1 templates from ",
              cwd,
              "/snaplets/embedded/extra-templates with route prefix ",
              "onemoredir/\n...adding 0 templates from ",
              cwd,
              "/templates with route prefix extraTemplates/\n",
              "Initializing JsonFileAuthManager @ ",
              "/auth\nSite successfully reloaded.\n"
              ]

        assertEqual "admin/reload" response b

    requestTest' "good" "Good template\n"


