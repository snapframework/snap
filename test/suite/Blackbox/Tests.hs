{-# LANGUAGE OverloadedStrings #-}

module Blackbox.Tests (tests) where

import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Enumerator as HTTP
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

------------------------------------------------------------------------------
requestTest :: String -> L.ByteString -> Test
requestTest url desired = testCase ("/"++url) $ do
    actual <- HTTP.simpleHttp $ "http://127.0.0.1:9753/" ++ url
    assertEqual url desired actual

requestNoError :: String -> L.ByteString -> Test
requestNoError url desired = testCase ("/"++url) $ do
    url' <- HTTP.parseUrl $ "http://127.0.0.1:9753/" ++ url
    HTTP.Response _ _ b <- liftIO $ HTTP.withManager $ HTTP.httpLbsRedirect url'
    assertEqual url desired b

tests :: Test
tests = testGroup "non-cabal-tests"
    [ requestTest "hello" "hello world"
    , requestTest "index" "index page\n"
    , requestTest "" "index page\n"
    , requestTest "splicepage" "splice page contents of the app splice\n"
    , requestTest "routeWithSplice" "routeWithSplice: foo snaplet data string"
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
    ]

