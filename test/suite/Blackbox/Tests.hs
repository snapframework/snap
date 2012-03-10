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
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy.Encoding        as T
import qualified Network.HTTP.Enumerator        as HTTP
import           Prelude                        hiding (catch)
import           System.Directory
import           System.FilePath
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test, path)

------------------------------------------------------------------------------
requestTest :: String -> Text -> Test
requestTest url desired = testCase ("/"++url) $ requestTest' url desired


------------------------------------------------------------------------------
expect404 :: IO a -> IO ()
expect404 m = action `catch` h
  where
    action = m >> assertFailure "expected 404"

    h e@(HTTP.StatusCodeException c _) | c == 404  = return ()
                                       | otherwise = throwIO e
    h e                                            = throwIO e


------------------------------------------------------------------------------
request404Test :: String -> Test
request404Test url = testCase ("/" ++ url) $
                     expect404             $
                     HTTP.simpleHttp       $
                     "http://127.0.0.1:9753/" ++ url


------------------------------------------------------------------------------
requestTest' :: String -> Text -> IO ()
requestTest' url desired = do
    actual <- HTTP.simpleHttp $ "http://127.0.0.1:9753/" ++ url
    assertEqual url desired (T.decodeUtf8 actual)


------------------------------------------------------------------------------
requestNoError :: String -> Text -> Test
requestNoError url desired = testCase ("/"++url) $ requestNoError' url desired


------------------------------------------------------------------------------
requestNoError' :: String -> Text -> IO ()
requestNoError' url desired = do
    let fullUrl = "http://127.0.0.1:9753/" ++ url
    url' <- HTTP.parseUrl fullUrl
    HTTP.Response _ _ b <- liftIO $ HTTP.withManager $ HTTP.httpLbsRedirect url'
    assertEqual fullUrl desired (T.decodeUtf8 b)


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
    , requestNoError "bazbadpage" "A web handler threw an exception. Details:\nTemplate \"cpyga\" not found."
    , requestTest "foo/fooSnapletName" "foosnaplet"
    , requestTest "foo/fooFilePath" "snaplets/foosnaplet"

    -- Test the embedded snaplet
    , requestTest "embed/heist/embeddedpage" "embedded snaplet page <asplice></asplice>\n"
    , requestTest "embed/aoeuhtns" "embedded snaplet page splice value42\n"
    , requestTest "embed/heist/onemoredir/extra" "This is an extra template\n"

    -- This set of tests highlights the differences in the behavior of the
    -- get... functions from MonadSnaplet.
    , requestTest "foo/handlerConfig" "([\"app\"],\"snaplets/foosnaplet\",Just \"foosnaplet\",\"A demonstration snaplet called foo.\",\"foo\")"
    , requestTest "bar/handlerConfig" "([\"app\"],\"snaplets/baz\",Just \"baz\",\"An example snaplet called bar.\",\"\")"

    -- bazpage5 uses barsplice bound by renderWithSplices at request time
    , requestTest "bazpage5" "baz template page ([\"app\"],\"snaplets/baz\",Just \"baz\",\"An example snaplet called bar.\",\"\")\n"

    -- bazconfig uses two splices, appconfig and fooconfig.  appconfig is bound
    -- with the non type class version of addSplices in the main app
    -- initializer.  fooconfig is bound by addSplices in fooInit.
    , requestTest "bazconfig" "baz config page ([],\"\",Just \"app\",\"Test application\",\"\") ([\"app\"],\"snaplets/foosnaplet\",Just \"foosnaplet\",\"A demonstration snaplet called foo.\",\"foo\")\n"

    , requestTest "sessionDemo" "[(\"foo\",\"bar\")]\n"
    , reloadTest
    ]

remove :: FilePath -> IO ()
remove f = do
    exists <- doesFileExist f
    when exists $ removeFile f


removeDir :: FilePath -> IO ()
removeDir d = do
    exists <- doesDirectoryExist d
    when exists $ removeDirectoryRecursive "non-cabal-appdir/snaplets/foosnaplet"


------------------------------------------------------------------------------
-- | 
reloadTest :: Test
reloadTest = testCase "reload test" $ do
    let goodTplOrig = "non-cabal-appdir" </> "good.tpl"
    let badTplOrig = "non-cabal-appdir" </> "bad.tpl"
    let goodTplNew = "non-cabal-appdir" </> "snaplets" </> "heist"
                                        </> "templates" </> "good.tpl"
    let badTplNew = "non-cabal-appdir" </> "snaplets" </> "heist"
                                       </> "templates" </> "bad.tpl"
    goodExists <- doesFileExist goodTplNew
    badExists <- doesFileExist badTplNew
    assertBool "good.tpl exists" (not goodExists)
    assertBool "bad.tpl exists" (not badExists)
    requestNoError' "bad" "404"
    copyFile badTplOrig badTplNew
    requestNoError' "good" "404"
    requestNoError' "bad" "404"
    requestTest' "admin/reload" "Error reloading site!\n\nInitializer threw an exception...\nsnaplets/heist/templates/bad.tpl \"snaplets/heist/templates/bad.tpl\" (line 2, column 1):\nunexpected end of input\nexpecting \"=\", \"/\" or \">\"\n\n\n...but before it died it generated the following output:\nInitializing app @ /\nInitializing heist @ /heist\n\n"
    remove badTplNew
    copyFile goodTplOrig goodTplNew
    requestTest' "admin/reload" "Initializing app @ /\nInitializing heist @ /heist\n...loaded 5 templates from snaplets/heist/templates\nInitializing foosnaplet @ /foo\n...adding 1 templates from snaplets/foosnaplet/templates with route prefix foo/\nInitializing baz @ /\n...adding 2 templates from snaplets/baz/templates with route prefix /\nInitializing CookieSession @ /session\nInitializing embedded @ /\nInitializing heist @ /heist\n...loaded 1 templates from snaplets/embedded/snaplets/heist/templates\n...adding 1 templates from snaplets/embedded/extra-templates with route prefix onemoredir/\nSite successfully reloaded.\n"
    requestTest' "good" "Good template\n"

