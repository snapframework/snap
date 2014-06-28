{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Test.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (race)
import qualified Data.Map                       as Map
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Core                      (readRequestBody,
                                                 writeLBS, writeText)
import qualified Snap.Test                      as ST
import           Snap.Snaplet.Test              (closeSnaplet,
                                                 getSnaplet,
                                                 evalHandler, evalHandler',
                                                 runHandler, runHandler')
import           Snap.Snaplet.Test.App          (appInit, failingAppInit)

------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Test"
    [ testRunHandler
    , testRunHandler'
    , testEvalHandler
    , testEvalHandler'
    , testFailingEvalHandler
    , testFailingGetSnaplet
--    , readRequestBodyHangIssue -- TODO/NOTE fix
    ]


------------------------------------------------------------------------------
testRunHandler :: Test
testRunHandler = testCase "runHandler simple" assertRunHandler
  where
    assertRunHandler :: Assertion
    assertRunHandler =
      do let hdl = writeText "Hello!"
         res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
         either (assertFailure . show)
           (ST.assertBodyContains "Hello!") res


------------------------------------------------------------------------------
testRunHandler' :: Test
testRunHandler' = testCase "runHandler' simple" assertRunHandler'
  where
    assertRunHandler' :: Assertion
    assertRunHandler' =
      do let hdl = writeText "Hello!"
         initS <- getSnaplet Nothing appInit
         case initS of
           Left err -> assertFailure (show err)
           Right (a,is) -> do
             res <- runHandler' a is (ST.get "" Map.empty) hdl
             closeSnaplet is
             either (assertFailure . show)
               (ST.assertBodyContains "Hello!") res


------------------------------------------------------------------------------
testEvalHandler :: Test
testEvalHandler = testCase "evalHandler simple" assertEvalHandler
  where
    assertEvalHandler :: Assertion
    assertEvalHandler =
      do let hdl = return "1+1=2"
         res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
         either (assertFailure . show)
           (assertEqual "" ("1+1=2"::String)) res
------------------------------------------------------------------------------
testEvalHandler' :: Test
testEvalHandler' = testCase "evalHandler' simple" assertEvalHandler'
  where
    assertEvalHandler' :: Assertion
    assertEvalHandler' =
      do let hdl = return "1+1=2"
         initS <- getSnaplet Nothing appInit
         case initS of
           Left err -> assertFailure (show err)
           Right (a,is) -> do
             res <- evalHandler' a is (ST.get "" Map.empty) hdl
             closeSnaplet is
             either (assertFailure . show)
               (assertEqual "" ("1+1=2"::String)) res

testFailingEvalHandler :: Test
testFailingEvalHandler = testCase "evalHandler failing simple" assertEvalHandler
  where
    assertEvalHandler :: Assertion
    assertEvalHandler =
      do let hdl = return ("1+1=2" :: String)
         res <- evalHandler Nothing (ST.get "" Map.empty) hdl failingAppInit
         case res of
           Left _ -> assertBool "" True
           Right _ -> assertFailure "Should have failed in initializer"


------------------------------------------------------------------------------
testFailingGetSnaplet :: Test
testFailingGetSnaplet = testCase "getSnaplet failing" assertGetSnaplet
 where
   assertGetSnaplet :: Assertion
   assertGetSnaplet =
     do initS <- getSnaplet Nothing failingAppInit
        case initS of
          Left _ -> assertBool "" True
          Right _ -> assertFailure "Should have failed in initializer"


------------------------------------------------------------------------------
readRequestBodyHangIssue :: Test
readRequestBodyHangIssue =
  testCase "readRequestBody doesn't hang" assertReadRqBody
  where
    assertReadRqBody =
      do let hdl = readRequestBody 5000 >>= writeLBS
         res <- race
                (threadDelay 1000000)
                (runHandler Nothing (ST.get "" Map.empty) hdl appInit)
         either (assertFailure . ("readRequestBody timeout" ++) . show)
           (either (assertFailure . show) ST.assertSuccess) res
