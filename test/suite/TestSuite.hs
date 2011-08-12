{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           System.Directory
import           Test.Framework (defaultMain)

import           Snap.Snaplet.App
import qualified Snap.Snaplet.Tests
import qualified Snap.Snaplet.Internal.Lensed.Tests


------------------------------------------------------------------------------
main :: IO ()
main = do
    -- tid <- startServer
    defaultMain tests
    -- throwTo tid UserInterrupt
  where tests = [ -- Snap.Snaplet.Tests.tests
                  Snap.Snaplet.Internal.Lensed.Tests.tests
                ]

-- FIXME: we cannot use 'serveSnaplet' here! Pass in config in code instead
startServer :: IO ThreadId
startServer = do
    setCurrentDirectory "non-cabal-appdir"
    tid <- forkIO $ serveSnaplet (setPort 9753 defaultConfig) app
    threadDelay $ 2*10^(6::Int)
    return tid

--testBarebones :: Test
--testBarebones = testCase "snap/barebones" go
--  where
--    go = testGeneratedProject "barebonesTest"
--                              "-b"
--                              ""
--                              port
--                              testIt
--    port = 9990
--    testIt = do
--        body <- HTTP.simpleHttp "http://127.0.0.1:9990"
--        assertEqual "server not up" "hello world" body


