{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Test.Tests
  ( tests ) where


------------------------------------------------------------------------------
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


------------------------------------------------------------------------------
import           Snap.Core
import qualified Snap.Test as ST
import           Snap.Snaplet.Test
import           Snap.Snaplet.Test.App

------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Test"
    [ testRunHandler
    , testRunHandler'
    , testEvalHandler
    , testEvalHandler'
    , testFailingEvalHandler
    , testFailingGetSnaplet
    ]

testRunHandler :: Test
testRunHandler = testCase "runHandler simple" assertRunHandler
  where
    assertRunHandler :: Assertion
    assertRunHandler = do let hdl = writeText "Hello!"
                          res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
                          either (assertFailure . show)
                                 (ST.assertBodyContains "Hello!") res

testRunHandler' :: Test
testRunHandler' = testCase "runHandler' simple" assertRunHandler'
  where
    assertRunHandler' :: Assertion
    assertRunHandler' = do let hdl = writeText "Hello!"
                           init <- getSnaplet Nothing appInit
                           case init of
                             Left err -> assertFailure (show err)
                             Right (a,is) -> do
                               res <- runHandler' a is (ST.get "" Map.empty) hdl
                               closeSnaplet is
                               either (assertFailure . show)
                                      (ST.assertBodyContains "Hello!") res

testEvalHandler :: Test
testEvalHandler = testCase "evalHandler simple" assertEvalHandler
  where
    assertEvalHandler :: Assertion
    assertEvalHandler = do let hdl = return "1+1=2"
                           res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
                           either (assertFailure . show)
                                  (assertEqual "" "1+1=2") res
testEvalHandler' :: Test
testEvalHandler' = testCase "evalHandler' simple" assertEvalHandler'
  where
    assertEvalHandler' :: Assertion
    assertEvalHandler' = do let hdl = return "1+1=2"
                            init <- getSnaplet Nothing appInit
                            case init of
                              Left err -> assertFailure (show err)
                              Right (a,is) -> do
                                res <- evalHandler' a is (ST.get "" Map.empty) hdl
                                closeSnaplet is
                                either (assertFailure . show)
                                       (assertEqual "" "1+1=2") res

testFailingEvalHandler :: Test
testFailingEvalHandler = testCase "evalHandler failing simple" assertEvalHandler
  where
    assertEvalHandler :: Assertion
    assertEvalHandler = do let hdl = return "1+1=2"
                           res <- evalHandler Nothing (ST.get "" Map.empty) hdl failingAppInit
                           case res of
                             Left _ -> assertBool "" True
                             Right _ -> assertFailure "Should have failed in initializer"

testFailingGetSnaplet :: Test
testFailingGetSnaplet = testCase "getSnaplet failing" assertGetSnaplet
 where
   assertGetSnaplet :: Assertion
   assertGetSnaplet = do init <- getSnaplet Nothing failingAppInit
                         case init of
                           Left _ -> assertBool "" True
                           Right _ -> assertFailure "Should have failed in initializer"
