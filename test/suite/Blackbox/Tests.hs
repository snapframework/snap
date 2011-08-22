{-# LANGUAGE OverloadedStrings #-}

module Blackbox.Tests
  ( tests
  , remove
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.HTTP.Enumerator as HTTP
import           System.Directory
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

------------------------------------------------------------------------------
requestTest :: String -> Text -> Test
requestTest url desired = testCase ("/"++url) $ requestTest' url desired

requestTest' :: String -> Text -> IO ()
requestTest' url desired = do
    actual <- HTTP.simpleHttp $ "http://127.0.0.1:9753/" ++ url
    assertEqual url desired (T.decodeUtf8 actual)

requestNoError :: String -> Text -> Test
requestNoError url desired = testCase ("/"++url) $ requestNoError' url desired

requestNoError' :: String -> Text -> IO ()
requestNoError' url desired = do
    url' <- HTTP.parseUrl $ "http://127.0.0.1:9753/" ++ url
    HTTP.Response _ _ b <- liftIO $ HTTP.withManager $ HTTP.httpLbsRedirect url'
    assertEqual url desired (T.decodeUtf8 b)

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

reloadTest :: Test
reloadTest = testCase "reload test" $ do
    goodExists <- doesFileExist "templates/good.tpl"
    badExists <- doesFileExist "templates/bad.tpl"
    assertBool "good.tpl exists" (not goodExists)
    assertBool "bad.tpl exists" (not badExists)
    requestNoError' "bad" "404"
    copyFile "bad.tpl" "templates/bad.tpl"
    requestNoError' "good" "404"
    requestNoError' "bad" "404"
    requestTest' "admin/reload" "Error reloading site!\n\ntemplates/bad.tpl \"templates/bad.tpl\" (line 2, column 1):\nunexpected end of input\nexpecting \"=\", \"/\" or \">\"\n"
    remove "templates/bad.tpl"
    copyFile "good.tpl" "templates/good.tpl"
    requestTest' "admin/reload" "Initializing app @ /\nInitializing heist @ /heist\nInitializing foosnaplet @ /foo\nAdding templates from snaplets/foosnaplet/templates with route prefix foo/\nInitializing baz @ /\nAdding templates from snaplets/baz/templates with route prefix /\nInitializing CookieSession @ /session\nSite successfully reloaded.\n"
    requestTest' "good" "Good template\n"

    remove "templates/bad.tpl"
    remove "templates/good.tpl"

