{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Network.HTTP.Enumerator as HTTP
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           System.Directory
import           Test.Framework (defaultMain, Test)
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.Http.Server (simpleHttpServe)
import           Blackbox.App
import qualified Blackbox.Tests
import qualified Snap.Snaplet.Internal.Lensed.Tests
import qualified Snap.Snaplet.Internal.LensT.Tests
import qualified Snap.Snaplet.Internal.RST.Tests
import qualified Snap.Snaplet.Internal.Tests
import           Snap.TestCommon


------------------------------------------------------------------------------
main :: IO ()
main = do
    Blackbox.Tests.remove "non-cabal-appdir/templates/bad.tpl"
    Blackbox.Tests.remove "non-cabal-appdir/templates/good.tpl"
    Blackbox.Tests.removeDir "non-cabal-appdir/snaplets/foosnaplet"

    -- Test generated projects before we start the test server.
    -- TODO Get this working properly.  Might have to put in the test list.
--    defaultMain [testBarebones, testDefault]

    tid <- startServer
    defaultMain tests
    throwTo tid UserInterrupt

  where tests = [ Blackbox.Tests.tests
                , Snap.Snaplet.Internal.Lensed.Tests.tests
                , Snap.Snaplet.Internal.LensT.Tests.tests
                , Snap.Snaplet.Internal.RST.Tests.tests
                , Snap.Snaplet.Internal.Tests.tests
--                , testBarebones
--                , testDefault
                ]

startServer :: IO ThreadId
startServer = do
    setCurrentDirectory "non-cabal-appdir"
    tid <- forkIO $ serve (setPort 9753 defaultConfig) app
    threadDelay $ 2*10^(6::Int)
    return tid
  where
    serve config initializer = do
        (_, handler, doCleanup) <- runSnaplet initializer
        (conf, site)            <- combineConfig config handler
        _ <- try $ simpleHttpServe conf $ site
             :: IO (Either SomeException ())
        doCleanup


testBarebones :: Test
testBarebones = testCase "snap/barebones" go
  where
    go = testGeneratedProject "barebonesTest"
                              "-b"
                              ""
                              port
                              testIt
    port = 9990
    testIt = do
        body <- HTTP.simpleHttp "http://127.0.0.1:9990"
        assertEqual "server not up" "hello world" body


testDefault :: Test
testDefault = testCase "snap/default" go
  where
    go = testGeneratedProject "defaultTest"
                              ""
                              ""
                              port
                              testIt
    port = 9991
    testIt = do
        body <- liftM (S.concat . L.toChunks) $
                HTTP.simpleHttp "http://127.0.0.1:9991"
        assertBool "response contains phrase 'it works!'"
                   $ "It works!" `S.isInfixOf` body

