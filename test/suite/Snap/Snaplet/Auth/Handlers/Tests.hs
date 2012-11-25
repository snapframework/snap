{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Handlers.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad.State as S
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.App
import qualified Snap.Test as ST
import           Snap.Snaplet.Test


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth.Handlers"
    [mutuallyExclusive $ testGroup "createUser tests"
        [ testCreateUserGood
        , testCreateEmptyUser
        , testCreateDupUser
        , testUsernameExists 
        , testLoginByUsername 
        , testLoginByUsernameEnc
        , testLoginByUsernameNoU 
        , testLoginByUsernameInvPwd
        , testLoginByRememberTokenKO
        , testLoginByRememberTokenOK
        , testLogoutKO
        , testLogoutOK
        , testCurrentUserKO
        , testCurrentUserOK
        , testIsLoggedInKO
        , testIsLoggedInOK
        , testSaveUserKO
        , testSaveUserOK
        , testMarkAuthFail
        --, testMarkAuthFailLockedOut
        , testMarkAuthSuccess
        , testCheckPasswordAndLoginOK
        , testCheckPasswordAndLoginKO
        , testAuthenticatePasswordOK
        , testAuthenticatePasswordPwdMissing
        , testAuthenticatePasswordPwdWrong
        , testRegisterUserOK
        , testRegisterUserNoUser
        , testRegisterUserNoPwd
        , testRequireUserOK
        , testRequireUserKO
        ]
    ]

------------------------------------------------------------------------------
isJustFailure :: AuthFailure -> Maybe AuthFailure -> Bool
isJustFailure failure (Just expected) = failure == expected
isJustFailure _ _ = False


------------------------------------------------------------------------------
isLeftFailure :: AuthFailure -> Either AuthFailure AuthUser -> Bool
isLeftFailure failure (Left expected) = failure == expected
isLeftFailure _ _ = False


------------------------------------------------------------------------------
testCreateUserGood :: Test
testCreateUserGood = testCase "createUser good params" assertGoodUser
  where 
    assertGoodUser :: Assertion
    assertGoodUser = withTemporaryFile "users.json" $ do
        let hdl = with auth $ createUser "foo" "foo"
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isRight) res

    failMsg = "createUser failed: Couldn't create a new user."


------------------------------------------------------------------------------
testCreateEmptyUser :: Test
testCreateEmptyUser = testCase "createUser empty username" assertEmptyUser
  where 
    assertEmptyUser :: Assertion
    assertEmptyUser = do
        let hdl = with auth $ createUser "" "foo"
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show)
               (assertBool failMsg . isLeftFailure UsernameMissing) res

    failMsg = "createUser: Was created an empty username despite they aren't allowed."


------------------------------------------------------------------------------
-- Is the tests execution order garanteed? When this runs, the user "foo"
-- will be already present in the backend.
testCreateDupUser :: Test
testCreateDupUser = testCase "createUser duplicate user" assertDupUser
  where 
    assertDupUser :: Assertion
    assertDupUser = do
        let hdl = with auth $ createUser "foo" "foo"
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show)
               (assertBool failMsg . isLeftFailure DuplicateLogin) res

    failMsg = "createUser: Expected to find a duplicate user, but I haven't."


------------------------------------------------------------------------------
-- A non desirable thing is to be couple by the temporal execution of
-- tests. The problem has been resolved using fixtures, so something like
-- that would be beneficial for next releases.
testUsernameExists :: Test
testUsernameExists = testCase "username exists" assertUserExists
  where
    assertUserExists :: Assertion
    assertUserExists = do
        let hdl = with auth $ usernameExists "foo"
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg) res

    failMsg = "usernameExists: Expected to return True, but it didn't."


------------------------------------------------------------------------------
testLoginByUsername :: Test
testLoginByUsername = testCase "successful loginByUsername" assertion
  where
    assertion :: Assertion
    assertion = do
        let pwd = ClearText "foo"
        res <- evalHandler (ST.get "" Map.empty) (loginByUnameHdlr pwd) appInit
        either (assertFailure . show) (assertBool failMsg . isRight) res

    failMsg = "loginByUsername: Failed with ClearText pwd."


------------------------------------------------------------------------------
-- Reused below.
loginByUnameHdlr :: Password -> Handler App App (Either AuthFailure AuthUser)
loginByUnameHdlr pwd = with auth $ loginByUsername "foo" pwd False


------------------------------------------------------------------------------
testLoginByUsernameEnc :: Test
testLoginByUsernameEnc = testCase "loginByUsername encrypted pwd" assertion
  where
    assertion :: Assertion
    assertion = do
        let pwd = Encrypted "foo"
        res <- evalHandler (ST.get "" Map.empty) (loginByUnameHdlr pwd) appInit
        either (assertFailure . show)
               (assertBool failMsg . isLeftFailure EncryptedPassword) res

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
        either (assertFailure . show)
               (assertBool failMsg . isLeftFailure UserNotFound) res

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
        either (assertFailure . show) (assertBool failMsg . isLeft) res

    failMsg = "loginByUsername: Expected to fail for an invalid pwd, but I didn't."


------------------------------------------------------------------------------
testLoginByRememberTokenKO :: Test
testLoginByRememberTokenKO = testCase "loginByRememberToken no token" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth loginByRememberToken
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isNothing) res

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


------------------------------------------------------------------------------
testLogoutKO :: Test
testLogoutKO = testCase "logout no user logged in." $ assertLogout hdl failMsg
  where
    hdl :: Handler App App (Maybe AuthUser)
    hdl = with auth $ do
        logout
        mgr <- S.get
        return (activeUser mgr)

    failMsg = "logout: Expected to get Nothing as the active user, " ++
              " but I didn't."


------------------------------------------------------------------------------
assertLogout :: Handler App App (Maybe AuthUser) -> String -> Assertion
assertLogout hdl failMsg = do
    res <- evalHandler (ST.get "" Map.empty) hdl appInit
    either (assertFailure . show) (assertBool failMsg . isNothing) res


------------------------------------------------------------------------------
testLogoutOK :: Test
testLogoutOK = testCase "logout user logged in." $ assertLogout hdl failMsg
  where
    hdl :: Handler App App (Maybe AuthUser)
    hdl = with auth $ do
        loginByUsername "foo" (ClearText "foo") True
        logout
        mgr <- get
        return (activeUser mgr)

    failMsg = "logout: Expected to get Nothing as the active user, " ++
              " but I didn't."


------------------------------------------------------------------------------
testCurrentUserKO :: Test
testCurrentUserKO = testCase "currentUser unsuccesful call" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth currentUser
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isNothing) res

    failMsg = "currentUser: Expected Nothing as the current user, " ++
              " but I didn't."


------------------------------------------------------------------------------
testCurrentUserOK :: Test
testCurrentUserOK = testCase "successful currentUser call" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isJust) res

    hdl :: Handler App App (Maybe AuthUser)
    hdl = with auth $ do
        res <- loginByUsername "foo" (ClearText "foo") True
        either (\_ -> return Nothing) (\_ -> currentUser) res

    failMsg = "currentUser: Expected to get the current user, " ++
              " but I didn't."


------------------------------------------------------------------------------
testIsLoggedInKO :: Test
testIsLoggedInKO = testCase "isLoggedIn, no user logged" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth isLoggedIn
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . not) res

    failMsg = "isLoggedIn: Expected False, but got True."


------------------------------------------------------------------------------
testIsLoggedInOK :: Test
testIsLoggedInOK = testCase "isLoggedIn, user logged" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg) res

    hdl :: Handler App App Bool
    hdl = with auth $ do
        loginByUsername "foo" (ClearText "foo") True
        isLoggedIn

    failMsg = "isLoggedIn: Expected True, but got False."


------------------------------------------------------------------------------
-- It fails because destroy is not yet implemented for the Json backend.
testDestroyUser :: Test
testDestroyUser = testCase "destroyUser" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . not) res

    hdl :: Handler App App Bool
    hdl = with auth $ do
        newUser <- createUser "bar" "bar"
        either (\_ -> return True)
               (\u -> destroyUser u >> usernameExists "bar")
               newUser

    failMsg = "destroyUser: I've tried to destroy an existing user, " ++
              "but user is still there."


------------------------------------------------------------------------------
testSaveUserKO :: Test
testSaveUserKO = testCase "saveUser null username" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isLeft) res

    hdl :: Handler App App (Either AuthFailure AuthUser)
    hdl = with auth $ do
        user <- loginByUsername "foo" (ClearText "foo") True
        case user of
          (Left e) -> return $ Left e
          (Right u) -> saveUser (u { userLogin = "" })

    failMsg = "saveUser: I expected to fail since I'm saving an " ++
              "empty username, but I didn't."


------------------------------------------------------------------------------
-- Trying to update a Cleartext text pwd result in an error. Feature or
-- bug? (error: Json can't serialize ClearText pwd)
testSaveUserOK :: Test
testSaveUserOK = testCase "saveUser good update params" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isRight) res

    hdl :: Handler App App (Either AuthFailure AuthUser)
    hdl = with auth $ do
        user <- loginByUsername "foo" (ClearText "foo") True
        case user of
          (Left e) -> return $ Left e
          (Right u) -> saveUser (u { userLoginCount = 99 })

    failMsg = "saveUser: I expected to success since I'm saving a " ++
              "valid user, but I didn't."


------------------------------------------------------------------------------
testMarkAuthFail :: Test
testMarkAuthFail = testCase "successful markAuthFail call" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg) res

    -- Lot of destructuring here, but the idea is to test if
    -- failedLoginCount increased by 1.
    hdl :: Handler App App Bool
    hdl = with auth $ do
        user <- loginByUsername "foo" (ClearText "foo") True
        case user of
          (Left _) -> return False
          (Right u) ->
              let failCount = userFailedLoginCount u
                  in do
                      res <- markAuthFail u
                      either (\_ -> return False)
                             (\u' -> return $
                                    userFailedLoginCount u' == failCount + 1)
                             res

    failMsg = "markAuthFail: I expected to increase the userFailedLoginCount, " ++
              "but I didn't."


------------------------------------------------------------------------------
testMarkAuthFailLockedOut :: Test
testMarkAuthFailLockedOut = testCase "markAuthFail lockedOut" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isLockedOut) res

    hdl :: Handler App App (Either AuthFailure AuthUser)
    hdl = with auth $ do
        user <- loginByUsername "bar" (ClearText "bar") True
        case user of
          (Left e) -> return $ Left e
          (Right u) ->
              let u' = u {userFailedLoginCount = 99}
                  in do
                      modify (\s -> s { lockout = Just (5, 1000000) })
                      markAuthFail u'

    failMsg = "markAuthFail: I expected the user to be LockedOut, " ++
              "but he didn't."

    isLockedOut :: Either AuthFailure AuthUser -> Bool
    isLockedOut (Left _) = False
    isLockedOut (Right u) = isJust $ userLockedOutUntil u

------------------------------------------------------------------------------
testMarkAuthSuccess :: Test
testMarkAuthSuccess = testCase "successful markAuthSuccess call" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg) res

    hdl :: Handler App App Bool
    hdl = with auth $ do
        user <- loginByUsername "foo" (ClearText "foo") True
        case user of
          (Left _) -> return False
          (Right u) ->
              let count = userLoginCount u
                  in do
                      res <- markAuthSuccess u
                      either (\_ -> return False)
                             (\u' -> return $
                                    userLoginCount u' == count + 1)
                             res

    failMsg = "markAuthSuccess: I expected to increase the userLoginCount, " ++
              "but I didn't."


------------------------------------------------------------------------------
testCheckPasswordAndLoginOK :: Test
testCheckPasswordAndLoginOK = testCase "checkPasswordAndLogin OK" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isRight) res

    hdl :: Handler App App (Either AuthFailure AuthUser)
    hdl = with auth $ do
        let pwd = ClearText "foo"
        res <- loginByUsername "foo" pwd False
        either (return . Left) (`checkPasswordAndLogin` pwd) res

    failMsg = "checkPasswordAndLogin: I expected to succeed " ++
              "but I didn't."


------------------------------------------------------------------------------
testCheckPasswordAndLoginKO :: Test
testCheckPasswordAndLoginKO = testCase "checkPasswordAndLogin KO" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isLeft) res

    hdl :: Handler App App (Either AuthFailure AuthUser)
    hdl = with auth $ do
        let pwd = ClearText "wrongpass"
        res <- loginByUsername "foo" pwd False
        either (return . Left) (`checkPasswordAndLogin` pwd) res

    failMsg = "checkPasswordAndLogin: I expected to succeed " ++
              "but I didn't."


------------------------------------------------------------------------------
testAuthenticatePasswordOK :: Test
testAuthenticatePasswordOK = testCase "authenticatePassword OK" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isNothing) res

    hdl :: Handler App App (Maybe AuthFailure)
    hdl = with auth $ do
        let pwd = ClearText "foo"
        res <- loginByUsername "foo" pwd False
        either (return . Just)
               (\u -> return $ authenticatePassword u pwd) res

    failMsg = "authenticatePassword: I expected to succeed " ++
              "but I didn't."


------------------------------------------------------------------------------
testAuthenticatePasswordPwdMissing :: Test
testAuthenticatePasswordPwdMissing = testCase "authenticatePassword no pwd" a
  where
    a :: Assertion
    a = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show)
               (assertBool failMsg . isJustFailure PasswordMissing) res

    hdl :: Handler App App (Maybe AuthFailure)
    hdl = with auth $ do
        let pwd = ClearText "foo"
        res <- loginByUsername "foo" pwd False
        either (return . Just)
               (\u -> let u' = u { userPassword = Nothing }
                         in return $ authenticatePassword u' pwd) res

    failMsg = "authenticatePassword: I expected to fail due to " ++
              " MissingPassword, but I didn't."
    

------------------------------------------------------------------------------
testAuthenticatePasswordPwdWrong :: Test
testAuthenticatePasswordPwdWrong = testCase "authenticatePassword wrong pwd" a
  where
    a :: Assertion
    a = do
        res <- evalHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show)
               (assertBool failMsg . isJustFailure IncorrectPassword) res

    hdl :: Handler App App (Maybe AuthFailure)
    hdl = with auth $ do
        let pwd = ClearText "foo"
        res <- loginByUsername "foo" pwd False
        either (return . Just)
               (\u -> return $ authenticatePassword u (ClearText "bar")) res

    failMsg = "authenticatePassword: I expected to fail due to " ++
              " IncorrectPassword, but I didn't."


------------------------------------------------------------------------------
testRegisterUserOK :: Test
testRegisterUserOK = testCase "registerUser OK" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth $ registerUser "user" "pwd"
        let params = [("user", ["fizz"]), ("pwd", ["buzz"])]
        res <- evalHandler (ST.get "" $ Map.fromList params) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isRight) res

    failMsg = "registerUser: I expected to succeed " ++
              ", but I didn't."


------------------------------------------------------------------------------
testRegisterUserNoUser :: Test
testRegisterUserNoUser = testCase "registerUser no user given" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth $ registerUser "user" "pwd"
        let params = [("user", []), ("pwd", ["buzz"])]
        res <- evalHandler (ST.get "" $ Map.fromList params) hdl appInit
        either (assertFailure . show)
               (assertBool failMsg . isLeftFailure UsernameMissing) res

    failMsg = "registerUser: I expected to fail due to UsernameMissing " ++
              ", but I didn't."


------------------------------------------------------------------------------
testRegisterUserNoPwd :: Test
testRegisterUserNoPwd = testCase "registerUser no pwd given" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth $ registerUser "user" "pwd"
        let params = [("user", ["fizz"]), ("pwd", [])]
        res <- evalHandler (ST.get "" $ Map.fromList params) hdl appInit
        either (assertFailure . show)
               (assertBool failMsg . isLeftFailure PasswordMissing) res

    failMsg = "registerUser: I expected to fail due to PasswordMissing " ++
              ", but I didn't."


------------------------------------------------------------------------------
testRequireUserOK :: Test
testRequireUserOK = testCase "requireUser good handler exec" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- runHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (ST.assertBodyContains "good") res

    hdl :: Handler App App ()
    hdl = with auth $ do
        let badHdl = writeText "bad" 
        let goodHdl = writeText "good" 
        loginByUsername "foo" (ClearText "foo") True
        requireUser auth badHdl goodHdl


------------------------------------------------------------------------------
testRequireUserKO :: Test
testRequireUserKO = testCase "requireUser bad handler exec" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- runHandler (ST.get "" Map.empty) hdl appInit
        either (assertFailure . show) (ST.assertBodyContains "bad") res

    hdl :: Handler App App ()
    hdl = with auth $ do
        let badHdl = writeText "bad" 
        let goodHdl = writeText "good" 
        loginByUsername "doesnotexist" (ClearText "") True
        requireUser auth badHdl goodHdl
