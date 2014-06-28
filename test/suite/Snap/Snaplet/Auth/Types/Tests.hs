module Snap.Snaplet.Auth.Types.Tests (
  tests
  ) where

------------------------------------------------------------------------------
import           Control.DeepSeq                      (deepseq)
import           Control.Exception                    (SomeException, try)
import           Control.Monad                        (liftM)
import           Data.Aeson                           (decode, eitherDecode,
                                                       encode)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy.Char8           as BSL
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Time
import           Test.HUnit                           hiding (Test)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Monadic              as QCM
------------------------------------------------------------------------------
import qualified Snap.Snaplet.Auth                    as A
import           Snap.TestCommon                      (eqTestCase,
                                                       ordTestCase,
                                                       readTestCase,
                                                       showTestCase)


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Auth type tests" [
    testCase     "Password serialization"          dontSerializeClearText
  , testCase     "Fill in [] roles"                deserializeDefaultRoles
  , testCase     "Fail deserialization"            failDeserialize
  , testProperty "AuthFailure show instances"      authFailureShows
  , testProperty "Encrypt agrees with password"    encryptByteString
  , testCase     "Reject clear encrypted pw check" rejectCheckClearText
  , testCase     "Test Role Show instance"         $ showTestCase (A.Role "a")
  , testCase     "Test Role Read instance"         $ readTestCase (A.Role "a")
  , testCase     "Test Role Ord  instance"         $
    ordTestCase (A.Role "a") (A.Role "b")
  , testCase     "Test PW Show instance"           $
    showTestCase (A.ClearText "pw")
  , testCase     "Test PW Read instance"           $
    readTestCase (A.ClearText "pw")
  , testCase     "Test PW Ord  instance"           $
    ordTestCase (A.ClearText "a") (A.ClearText "b")
  , testCase     "Test AuthFailure Eq instance"    $
    eqTestCase A.BackendError A.DuplicateLogin --TODO better as property
  , testCase     "Test AuthFailure Show instance"  $
    showTestCase A.BackendError
--  , testCase     "Test AuthFailure Read instance"  $
--    readTestCase BackendError -- TODO/NOTE: show . read isn't id for 
  , testCase     "Test AuthFailure Ord instance"   $
    ordTestCase A.BackendError A.DuplicateLogin
  , testCase     "Test UserId Show instance"       $
    showTestCase (A.UserId "1")
  , testCase     "Test UserId Read instance"       $
    readTestCase (A.UserId "2")
  , testCase     "Test AuthUser Show instance"     $
    showTestCase A.defAuthUser
  , testCase     "Test AuthUser Eq instance"       $
    eqTestCase A.defAuthUser A.defAuthUser
  ]


------------------------------------------------------------------------------
dontSerializeClearText :: Assertion
dontSerializeClearText = do
  let s = encode (A.ClearText "passwordisnthamster")
  r <- try $ s `deepseq` return s
  case r of
    Left  e -> (e :: SomeException) `seq` return ()
    Right j -> assertFailure $
               "Failed to reject ClearText password serialization: "
               ++ show j


------------------------------------------------------------------------------
sampleUserJson :: T.Text -> T.Text -> T.Text
sampleUserJson reqPair optPair = T.intercalate "," [
    "{\"uid\":\"1\""
  , "\"login\":\"foo\""
  , "\"email\":\"test@example.com\""
  , "\"pw\":\"sha256|12|gz47sA0OvbVjos51OJRauQ==|Qe5aU2zAH0gIKHP68KrHJkvvwTvTAqA6UgA33BRpNEo=\""
  , reqPair
  , "\"suspended_at\":null"
  , "\"remember_token\":\"81160620ef9b64865980c2ab760fcf7f14c06e057cbe1e723cba884a9be05547\""
  , "\"login_count\":2"
  , "\"failed_login_count\":1"
  , "\"locked_until\":null"
  , "\"current_login_at\":\"2014-06-24T14:43:51.241Z\""
  , "\"last_login_at\":null"
  , "\"current_ip\":\"127.0.0.1\""
  , "\"last_ip\":null"
  , "\"created_at\":\"2014-06-24T14:43:51.236Z\""
  , "\"updated_at\":\"2014-06-24T14:43:51.242Z\""
  , "\"reset_token\":null"
  , "\"reset_requested_at\":null"
  , optPair
  , "\"meta\":{}}"
  ]


------------------------------------------------------------------------------
deserializeDefaultRoles :: Assertion
deserializeDefaultRoles =
  either
  (\e -> assertFailure $ "Failed user deserialization: " ++ e)
  (\u -> assertEqual "Roles wasn't initialized to empty" [] (A.userRoles u))
  (eitherDecode . BSL.fromStrict . encodeUtf8 $
   sampleUserJson "\"activated_at\":null" "\"extra\":null")


------------------------------------------------------------------------------
failDeserialize :: Assertion
failDeserialize = do
  case decode . BSL.fromStrict . encodeUtf8 $ t of
    Nothing -> return ()
    Just a  -> assertFailure $
               "Expected deserialization failure, got authUser: "
               ++ show (a :: A.AuthUser)

  where
    t = T.replace "login" "loogin" $
        sampleUserJson "\"extra\":null" "\"extra2\":null"


------------------------------------------------------------------------------
authFailureShows :: A.AuthFailure -> Bool
authFailureShows ae = length (show ae) > 0


------------------------------------------------------------------------------
instance QC.Arbitrary A.AuthFailure where
  arbitrary = do
    s <- (QC.arbitrary `QC.suchThat` (( > 0 ) . length))
    tA <- QC.arbitrary
    tB <- QC.arbitrary
    let t = UTCTime
            (ModifiedJulianDay tA)
            (realToFrac (tB :: Double))
    QC.oneof $ map return [A.AuthError s,       A.BackendError
                          ,A.DuplicateLogin,    A.EncryptedPassword
                          ,A.IncorrectPassword, A.LockedOut t
                          ,A.PasswordMissing,   A.UsernameMissing
                          ,A.UserNotFound
                          ]


------------------------------------------------------------------------------
encryptByteString :: QC.Property
encryptByteString = QCM.monadicIO testStringEq
  where
    clearPw = BS.pack `liftM` (QC.arbitrary `QC.suchThat` ((>0) . length))
    testStringEq = QCM.forAllM clearPw $ \s -> do
      ePW  <- A.Encrypted `liftM` (QCM.run $ A.encrypt s)

      let cPW  = A.ClearText s
{-      ePW' <- QCM.run $ encryptPassword (ClearText s)
      QCM.assert $ (checkPassword cPW ePW
                    && checkPassword cPW cPW
                    && checkPassword ePW ePW') --TODO/NOTe: This fails.
                                                 Surpsising?
                                                 Encrypt twice and get two
                                                 different password hashes -}
      QCM.assert $ (A.checkPassword cPW ePW
                    && A.checkPassword cPW (A.ClearText s))


------------------------------------------------------------------------------
rejectCheckClearText :: Assertion
rejectCheckClearText = do
  let b = A.checkPassword (A.Encrypted "") (A.ClearText "")
  r <- try $ b `seq` return b
  case r of
    Left  e -> (e :: SomeException) `seq` return ()
    Right _ -> assertFailure
               "checkPassword should not accept encripted-clear pair"

