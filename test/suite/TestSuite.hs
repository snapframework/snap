{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import           Network.Http.Client
import           Prelude hiding (catch)
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           System.IO
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
------------------------------------------------------------------------------
import           Blackbox.App
import qualified Blackbox.Tests
import           Snap.Http.Server (simpleHttpServe)
import qualified Snap.Snaplet.Internal.Lensed.Tests
import qualified Snap.Snaplet.Internal.LensT.Tests
import qualified Snap.Snaplet.Internal.RST.Tests
import qualified Snap.Snaplet.Internal.Tests
import qualified Snap.Snaplet.Auth.Tests
import           Snap.TestCommon

import           SafeCWD


------------------------------------------------------------------------------
main :: IO ()
main = do
    Blackbox.Tests.remove
                "non-cabal-appdir/snaplets/heist/templates/bad.tpl"
    Blackbox.Tests.remove
                "non-cabal-appdir/snaplets/heist/templates/good.tpl"
    Blackbox.Tests.removeDir "non-cabal-appdir/snaplets/foosnaplet"

    (tid, mvar) <- inDir False "non-cabal-appdir" startServer
    defaultMain [tests] `finally` killThread tid

    putStrLn "waiting for termination mvar"
    takeMVar mvar

  where tests = mutuallyExclusive $
                testGroup "snap" [ internalServerTests
                                 , Snap.Snaplet.Auth.Tests.tests
                                 , testDefault
                                 , testBarebones
                                 , testTutorial
                                 ]


------------------------------------------------------------------------------
internalServerTests :: Test
internalServerTests =
    mutuallyExclusive $
    testGroup "internal server tests"
        [ Blackbox.Tests.tests
        , Snap.Snaplet.Internal.Lensed.Tests.tests
        , Snap.Snaplet.Internal.LensT.Tests.tests
        , Snap.Snaplet.Internal.RST.Tests.tests
        , Snap.Snaplet.Internal.Tests.tests
        ]


------------------------------------------------------------------------------
startServer :: IO (ThreadId, MVar ())
startServer = do
    mvar <- newEmptyMVar
    t    <- forkIO $ serve mvar (setPort 9753 defaultConfig) app
    threadDelay $ 2*10^(6::Int)
    return (t, mvar)

  where
    serve mvar config initializer =
        flip finally (putMVar mvar ()) $
        handle handleErr $ do
            hPutStrLn stderr "initializing snaplet"
            (_, handler, doCleanup) <- runSnaplet Nothing initializer

            flip finally doCleanup $ do
                (conf, site) <- combineConfig config handler
                hPutStrLn stderr "bringing up server"
                simpleHttpServe conf site
                hPutStrLn stderr "server killed"

    handleErr :: SomeException -> IO ()
    handleErr e = hPutStrLn stderr $ "startServer exception: " ++ show e


------------------------------------------------------------------------------
testBarebones :: Test
testBarebones = testCase "snap/barebones" go
  where
    go = testGeneratedProject "barebonesTest"
                              "barebones"
                              "--force-reinstalls"
                              port
                              testIt
    port = 9990 :: Int
    testIt = do
        body <- get (S.pack $ "http://127.0.0.1:"++(show port)) concatHandler
        assertEqual "server not up" "hello world" body


------------------------------------------------------------------------------
testDefault :: Test
testDefault = testCase "snap/default" go
  where
    go = testGeneratedProject "defaultTest"
                              ""
                              "--force-reinstalls"
                              port
                              testIt
    port = 9991 :: Int
    testIt = do
        body <- get (S.pack $ "http://127.0.0.1:"++(show port)) concatHandler
        assertBool "response contains phrase 'Snap Example App Login'"
                   $ "Snap Example App Login" `S.isInfixOf` body


------------------------------------------------------------------------------
testTutorial :: Test
testTutorial = testCase "snap/tutorial" go
  where
    go = testGeneratedProject "tutorialTest"
                              "tutorial"
                              "--force-reinstalls"
                              port
                              testIt
    port = 9992 :: Int
    testIt = do
        body <- get (S.pack $ "http://127.0.0.1:"++(show port)++"/hello") concatHandler
        assertEqual "server not up" "hello world" body


