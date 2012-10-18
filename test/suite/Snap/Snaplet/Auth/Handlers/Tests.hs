{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Handlers.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Control.Error
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
        ,testLoginByUsername 
        ,testLoginByUsernameEnc
        ,testLoginByUsernameNoU 
        ,testLoginByUsernameInvPwd
        ,testLoginByRememberTokenKO
        ,testLoginByRememberTokenOK
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
          (Right res') -> assertBool failMsg $ isRight res'

    failMsg = "createUser failed: Couldn't create a new user."


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
          (Right res') -> assertBool failMsg $ isLeft res'

    failMsg = "createUser: Was created an empty username despite they aren't allowed."


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
          (Right res') -> assertBool failMsg $ isLeft res'

    failMsg = "createUser: Expected to find a duplicate user, but I haven't."


------------------------------------------------------------------------------
-- A non desirable thing is to be couple by the temporal execution of
-- tests. The problem has been resolved using fixtures, so something like
-- that would be beneficial for the next releases.
testUsernameExists :: Test
testUsernameExists = testCase "username exists" assertUserExists
  where
    assertUserExists :: Assertion
    assertUserExists = do
        let hdl = with auth $ usernameExists "foo"
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg res'

    failMsg = "usernameExists: Expected to return True, but it didn't."


------------------------------------------------------------------------------
testLoginByUsername :: Test
testLoginByUsername = testCase "successful loginByUsername" assertion
  where
    assertion :: Assertion
    assertion = do
        let pwd = ClearText "foo"
        let hdl = with auth $ loginByUsername "foo" pwd False
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isRight res'

    failMsg = "loginByUsername: Failed with ClearText pwd."


------------------------------------------------------------------------------
testLoginByUsernameEnc :: Test
testLoginByUsernameEnc = testCase "loginByUsername encrypted pwd" assertion
  where
    assertion :: Assertion
    assertion = do
        let pwd = Encrypted "foo"
        let hdl = with auth $ loginByUsername "foo" pwd False
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isLeft res'

    failMsg = "loginByUsername: Expected to find an Encrypted password, but I haven't."


------------------------------------------------------------------------------
testLoginByUsernameNoU :: Test
testLoginByUsernameNoU = testCase "loginByUsername invalid user" assertion
  where
    assertion :: Assertion
    assertion = do
        let pwd = ClearText "foo"
        let hdl = with auth $ loginByUsername "doesnotexist" pwd False
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isLeft res'

    failMsg = "loginByUsername: Expected to fail for an invalid user, but I didn't."


------------------------------------------------------------------------------
testLoginByUsernameInvPwd :: Test
testLoginByUsernameInvPwd = testCase "loginByUsername invalid user" assertion
  where
    assertion :: Assertion
    assertion = do
        let pwd = ClearText "invalid"
        let hdl = with auth $ loginByUsername "foo" pwd False
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isLeft res'

    failMsg = "loginByUsername: Expected to fail for an invalid pwd, but I didn't."


------------------------------------------------------------------------------
testLoginByRememberTokenKO :: Test
testLoginByRememberTokenKO = testCase "loginByRememberToken no token" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth loginByRememberToken
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isNothing res'

    failMsg = "loginByRememberToken: Expected to fail for the " ++
              "absence of a token, but I didn't."


------------------------------------------------------------------------------
testLoginByRememberTokenOK :: Test
testLoginByRememberTokenOK = testCase "loginByRememberToken token" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isJust res'

    hdl :: Handler App App (Maybe AuthUser)
    hdl = with auth $ do
        res <- loginByUsername "foo" (ClearText "foo") True
        either (\_ -> return Nothing) (\_ -> loginByRememberToken) res

    failMsg = "loginByRememberToken: Expected to succeed but I didn't."
