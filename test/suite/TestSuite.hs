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
import           System.Posix.Process
import           System.Posix.Types
--import           Test.Framework (defaultMain, Test)
import           Test.Framework
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

import SafeCWD


------------------------------------------------------------------------------
main :: IO ()
main = do
    Blackbox.Tests.remove "non-cabal-appdir/templates/bad.tpl"
    Blackbox.Tests.remove "non-cabal-appdir/templates/good.tpl"
    Blackbox.Tests.removeDir "non-cabal-appdir/snaplets/foosnaplet"

    inDir False "non-cabal-appdir" startServer
    threadDelay $ 2*10^(6::Int)
    defaultMain tests

  where tests = [ internalServerTests
                , testDefault
                , testBarebones
                , testTutorial
                ]


internalServerTests :: Test
internalServerTests =
    testGroup "internal server tests"
        [ Blackbox.Tests.tests
        , Snap.Snaplet.Internal.Lensed.Tests.tests
        , Snap.Snaplet.Internal.LensT.Tests.tests
        , Snap.Snaplet.Internal.RST.Tests.tests
        , Snap.Snaplet.Internal.Tests.tests
        ]

startServer :: IO ProcessID
startServer = forkProcess $ serve (setPort 9753 defaultConfig) app
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
                              "barebones"
                              ""
                              port
                              testIt
    port = 9990
    testIt = do
        body <- HTTP.simpleHttp $ "http://127.0.0.1:"++(show port)
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
                HTTP.simpleHttp $ "http://127.0.0.1:"++(show port)
        assertBool "response contains phrase 'it works!'"
                   $ "It works!" `S.isInfixOf` body


testTutorial :: Test
testTutorial = testCase "snap/tutorial" go
  where
    go = testGeneratedProject "tutorialTest"
                              "tutorial"
                              ""
                              port
                              testIt
    port = 9992
    testIt = do
        body <- HTTP.simpleHttp $ "http://127.0.0.1:"++(show port)++"/hello"
        assertEqual "server not up" "hello world" body


