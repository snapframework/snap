module Snap.Snaplet.Auth.Types.Tests (
  tests
  ) where

------------------------------------------------------------------------------
import           Control.Exception                    (SomeException, evaluate, try)
import           Control.Monad                        (liftM)
import           Data.Aeson                           (decode, eitherDecode, encode)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy.Char8           as BSL
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Time
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           hiding (Test)
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Monadic              as QCM
------------------------------------------------------------------------------
import qualified Snap.Snaplet.Auth                    as A
import           Snap.TestCommon                      (eqTestCase, ordTestCase, readTestCase, showTestCase)


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Auth type tests" [
    testCase     "Fail deserialization"            failDeserialize
  , testProperty "AuthFailure show instances"      authFailureShows
  , testProperty "Encrypt agrees with password"    encryptByteString
  , testCase     "Test PW Show instance"           $
    showTestCase (A.HashedPassword "pw")
  , testCase     "Test PW Ord  instance"           $
    ordTestCase (A.HashedPassword "a") (A.HashedPassword "b")
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
failDeserialize :: Assertion
failDeserialize = do
  case decode . BSL.fromChunks . (:[]) . encodeUtf8 $ t of
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
    testStringEq = QCM.forAllM clearPw $ \cPW -> do
      ePW  <- A.HashedPassword `liftM` (QCM.run $ A.encrypt cPW)

{-      ePW' <- QCM.run $ encryptPassword (ClearText s)
      QCM.assert $ (checkPassword cPW ePW
                    && checkPassword cPW cPW
                    && checkPassword ePW ePW') --TODO/NOTe: This fails.
                                                 Surpsising?
                                                 Encrypt twice and get two
                                                 different password hashes -}
      QCM.assert $ (A.checkPassword cPW ePW)
