module Snap.Snaplet.Auth.Types.Tests (
  tests
  ) where

------------------------------------------------------------------------------
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Time.Clock
import GHC.Read
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Text.ParserCombinators.ReadPrec
------------------------------------------------------------------------------
import Snap.Core
import qualified Snap.Test as ST
import Snap.Snaplet.Test
import Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Snap.TestCommon

------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Auth type tests" [
    testCase     "Password serialization"          dontSerializeClearText
  , testCase     "Fill in [] roles"                deserializeDefaultRoles
  , testCase     "Fail deserialization"            failDeserialize
  , testProperty "AuthFailure show instances"      authFailureShows
  , testProperty "Encrypt agrees with password"    encryptByteString
  , testCase     "Reject clear encrypted pw check" rejectCheckClearText
  , testCase     "Test Role Show instance"         $ showTestCase (Role "a")
  , testCase     "Test Role Read instance"         $ readTestCase (Role "a")
  , testCase     "Test Role Ord  instance"         $
    ordTestCase (Role "a") (Role "b")
  , testCase     "Test PW Show instance"           $
    showTestCase (ClearText "pw")
  , testCase     "Test PW Read instance"           $
    readTestCase (ClearText "pw")
  , testCase     "Test PW Ord  instance"           $
    ordTestCase (ClearText "a") (ClearText "b")
  , testCase     "Test AuthFailure Eq instance"    $
    eqTestCase BackendError DuplicateLogin --TODO better as property
  , testCase     "Test AuthFailure Show instance"  $
    showTestCase BackendError
--  , testCase     "Test AuthFailure Read instance"  $
--    readTestCase BackendError -- TODO/NOTE: show . read isn't id for 
  , testCase     "Test AuthFailure Ord instance"   $
    ordTestCase BackendError DuplicateLogin
  , testCase     "Test UserId Show instance"       $
    showTestCase (UserId "1")
  , testCase     "Test UserId Read instance"       $
    readTestCase (UserId "2")
  , testCase     "Test AuthUser Show instance"     $
    showTestCase defAuthUser
  , testCase     "Test AuthUser Eq instance"       $
    eqTestCase defAuthUser defAuthUser
  ]


------------------------------------------------------------------------------
dontSerializeClearText :: Assertion
dontSerializeClearText = do
  let s = encode (ClearText "passwordisnthamster")
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
  (\u -> assertEqual "Roles wasn't initialized to empty" [] (userRoles u))
  (eitherDecode . BSL.fromStrict . encodeUtf8 $
   sampleUserJson "\"activated_at\":null" "\"extra\":null")


------------------------------------------------------------------------------
failDeserialize :: Assertion
failDeserialize = do
  case decode . BSL.fromStrict . encodeUtf8 $ t of
    Nothing -> return ()
    Just a  -> assertFailure $
               "Expected deserialization failure, got authUser: "
               ++ show (a :: AuthUser)

  where
    t = T.replace "login" "loogin" $
        sampleUserJson "\"extra\":null" "\"extra2\":null"


------------------------------------------------------------------------------
authFailureShows :: AuthFailure -> Bool
authFailureShows ae = length (show ae) > 0


------------------------------------------------------------------------------
instance Arbitrary AuthFailure where
  arbitrary = do
    s <- (arbitrary `suchThat` (( > 0 ) . length))
    tA <- arbitrary
    tB <- arbitrary
    let t = UTCTime
            (ModifiedJulianDay tA)
            (realToFrac (tB :: Double))
    oneof (map return [AuthError s, BackendError, DuplicateLogin
                      ,EncryptedPassword, IncorrectPassword, LockedOut t
                      ,PasswordMissing, UsernameMissing, UserNotFound])


------------------------------------------------------------------------------
encryptByteString :: Property
encryptByteString = QCM.monadicIO testStringEq
  where
    clearPw = BS.pack `liftM` (arbitrary `suchThat` ((>0) . length))
    testStringEq = QCM.forAllM clearPw $ \s -> do
      ePW  <- Encrypted `liftM` (QCM.run $ encrypt s)
      ePW' <- QCM.run $ encryptPassword (ClearText s)
      let cPW  = ClearText s
{-      QCM.assert $ (checkPassword cPW ePW
                    && checkPassword cPW cPW
                    && checkPassword ePW ePW') --TODO/NOTe: This fails.
                                                 Surpsising?
                                                 Encrypt twice and get two
                                                 different password hashes -}
      QCM.assert $ (checkPassword cPW ePW
                    && checkPassword cPW (ClearText s))


------------------------------------------------------------------------------
rejectCheckClearText :: Assertion
rejectCheckClearText = do
  let b = checkPassword (Encrypted "") (ClearText "")
  r <- try $ b `seq` return b
  case r of
    Left  e -> (e :: SomeException) `seq` return ()
    Right _ -> assertFailure
               "checkPassword should not accept encripted-clear pair"

