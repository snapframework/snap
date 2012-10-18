{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Handlers.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Prelude hiding (id)
import           Control.Category
import           Control.Error (isLeft, isRight)
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)


------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.App
import qualified Snap.Test as ST
import           Snap.Snaplet.Test


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth.Handlers"
    [mutuallyExclusive $ testGroup "createUser tests"
        [testCreateUserGood
        ,testCreateEmptyUser
        ,testCreateDupUser
        ,testUsernameExists 
        ]
    ]


------------------------------------------------------------------------------
createGoodUserHdlr :: Handler App App (Either AuthFailure AuthUser)
createGoodUserHdlr = with auth $ createUser "foo" "foo"


------------------------------------------------------------------------------
createEmptyUserHdlr :: Handler App App (Either AuthFailure AuthUser)
createEmptyUserHdlr = with auth $ createUser "" "foo"


------------------------------------------------------------------------------
createDupUserHdlr :: Handler App App (Either AuthFailure AuthUser)
createDupUserHdlr = with auth $ createUser "foo" "foo"


------------------------------------------------------------------------------
testCreateUserGood :: Test
testCreateUserGood = testCase "createUser good params" assertGoodUser
  where 
    assertGoodUser :: Assertion
    assertGoodUser = withTemporaryFile "users.json" $ do
        res <- evalHandler (ST.get "" Map.empty) createGoodUserHdlr appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool "user successfully created" $ isRight res'


------------------------------------------------------------------------------
-- Here we are gaining in generality but losing specificity, because we
-- don't check the specific error raised. This can be cause brittle tests.
testCreateEmptyUser :: Test
testCreateEmptyUser = testCase "createUser empty username" assertEmptyUser
  where 
    assertEmptyUser :: Assertion
    assertEmptyUser = do
        res <- evalHandler (ST.get "" Map.empty) createEmptyUserHdlr appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool "empty username rejected" $ isLeft res'


------------------------------------------------------------------------------
-- Is the tests execution order garanteed? When this runs, the user "foo"
-- will be already present in the backend.
testCreateDupUser :: Test
testCreateDupUser = testCase "createUser duplicate user" assertDupUser
  where 
    assertDupUser :: Assertion
    assertDupUser = do
        res <- evalHandler (ST.get "" Map.empty) createDupUserHdlr appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool "duplicate user rejected" $ isLeft res'


------------------------------------------------------------------------------
-- A non desirable thing is to be couple by the temporal execution of
-- tests. The problem has been resolved using fixtures, so something like
-- that would be beneficial for the next releases.
testUsernameExists :: Test
testUsernameExists = testCase "username exists" assertUserExists
  where
    assertUserExists :: Assertion
    assertUserExists = do
        res <- evalHandler (ST.get "" Map.empty)
                          (with auth $ usernameExists "foo")
                          appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool "username exists" res'

