{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception                  (SomeException (..), bracket, catch, finally)
import           Control.Monad                      (void)
import           System.Directory                   (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath                    ((</>))
import           System.IO

------------------------------------------------------------------------------
import qualified Blackbox.Tests
import           Prelude                            (Bool (False), IO, Int, Maybe (Nothing), Monad (..), Num (..), flip, return, ($), (.), (^))
import           Snap.Http.Server                   (simpleHttpServe)
import           Snap.Http.Server.Config
import           Snap.Snaplet
import qualified Snap.Snaplet.Auth.Tests
import qualified Snap.Snaplet.Config.Tests
import qualified Snap.Snaplet.Heist.Tests
import qualified Snap.Snaplet.Internal.Lensed.Tests
import qualified Snap.Snaplet.Internal.LensT.Tests
import qualified Snap.Snaplet.Internal.RST.Tests
import qualified Snap.Snaplet.Internal.Tests
import           Snap.Snaplet.Test.Common.App
import qualified Snap.Snaplet.Test.Tests
import           Test.Framework

import           SafeCWD

------------------------------------------------------------------------------
main :: IO ()
main = do
    -- chdir into test/
    cwd <- getCurrentDirectory
    setCurrentDirectory (cwd </> "test")

    Blackbox.Tests.remove
                "snaplets/heist/templates/bad.tpl"
    Blackbox.Tests.remove
                "snaplets/heist/templates/good.tpl"
 {- Why were we removing this?
    Blackbox.Tests.removeDir "snaplets/foosnaplet"
 -}

--    (tid, mvar) <- inDir False "non-cabal-appdir" startServer
    (tid, mvar) <- inDir False "." startServer

    defaultMain [tests]
      `finally` do
          setCurrentDirectory cwd
          killThread tid
          putStrLn "waiting for termination mvar"
          takeMVar mvar

      where tests = mutuallyExclusive $
                testGroup "snap" [ internalServerTests
                                 , Snap.Snaplet.Auth.Tests.tests
                                 , Snap.Snaplet.Test.Tests.tests
                                 , Snap.Snaplet.Heist.Tests.heistTests
                                 , Snap.Snaplet.Config.Tests.configTests
                                 , Snap.Snaplet.Internal.RST.Tests.tests
                                 , Snap.Snaplet.Internal.LensT.Tests.tests
                                 , Snap.Snaplet.Internal.Lensed.Tests.tests
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
    t    <- forkIOWithUnmask $ \restore ->
            serve restore mvar (setPort 9753 .
                                setBind "127.0.0.1" $ defaultConfig) appInit
    threadDelay $ 2*10^(6::Int)
    return (t, mvar)

  where
    gobble m = void m `catch` \(_::SomeException) -> return ()
    serve restore mvar config initializer =
        flip finally (putMVar mvar ()) $
        gobble $ restore $ do
            hPutStrLn stderr "initializing snaplet"
            bracket (runSnaplet Nothing initializer)
                    (\(_, _, doCleanup) -> doCleanup)
                    (\(_, handler, _  ) -> do
                         (conf, site) <- combineConfig config handler
                         hPutStrLn stderr "bringing up server"
                         simpleHttpServe conf site)
