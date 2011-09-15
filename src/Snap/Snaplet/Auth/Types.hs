{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Types where

import           Control.Monad.CatchIO
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Lens.Lazy
import           Data.Time
import           Data.Typeable
import           Data.Text (Text)
import           Crypto.PasswordStore
import           Web.ClientSession

import           Snap.Snaplet
import           Snap.Snaplet.Session


------------------------------------------------------------------------------
-- | Password is clear when supplied by the user and encrypted later when
-- returned from the db.
data Password = ClearText ByteString
              | Encrypted ByteString
              deriving (Read, Show, Ord, Eq)


------------------------------------------------------------------------------
-- Turn a 'ClearText' password into an 'Encrypted' password, ready to be
-- stuffed into a database.
encryptPassword :: Password -> IO Password
encryptPassword p@(Encrypted {}) = return p
encryptPassword (ClearText p) = do
  hashed <- makePassword p 12
  return $ Encrypted hashed


checkPassword :: Password -> Password -> Bool 
checkPassword (ClearText pw) (Encrypted pw') = verifyPassword pw pw'
checkPassword _ _ = 
  error "checkPassword failed. Make sure you pass ClearText passwords"


------------------------------------------------------------------------------
-- | Authentication failures indicate what went wrong during authentication.
-- They may provide useful information to the developer, although it is
-- generally not advisable to show the user the exact details about why login
-- failed.
data AuthFailure = 
    UserNotFound
  | IncorrectPassword
  | PasswordMissing
  | LockedOut UTCTime
  -- ^ Locked out until given time
  | AuthError String
  deriving (Read, Show, Ord, Eq, Typeable)


instance Exception AuthFailure


------------------------------------------------------------------------------
-- | Internal representation of a 'User'. By convention, we demand that the
-- application is able to directly fetch a 'User' using this identifier.
--
-- Think of this type as a secure, authenticated user. You should normally
-- never see this type unless a user has been authenticated.
newtype UserId = UserId { unUid :: Text }
    deriving (Read,Show,Ord,Eq,FromJSON,ToJSON,Hashable)


-- | This will be replaced by Greg's role-based permission system.
data Role = Role ByteString
  deriving (Read,Show,Ord,Eq)


------------------------------------------------------------------------------
-- | Type representing the concept of a User in your application.
data AuthUser = AuthUser 
  { userId :: Maybe UserId
  , userLogin :: Text
  , userPassword :: Maybe Password
  , userActivatedAt :: Maybe UTCTime
  , userSuspendedAt :: Maybe UTCTime
  , userRememberToken :: Maybe Text
  , userLoginCount :: Int
  , userFailedLoginCount :: Int
  , userLockedOutUntil :: Maybe UTCTime
  , userCurrentLoginAt :: Maybe UTCTime
  , userLastLoginAt :: Maybe UTCTime
  , userCurrentLoginIp :: Maybe ByteString
  , userLastLoginIp :: Maybe ByteString
  , userCreatedAt :: Maybe UTCTime
  , userUpdatedAt :: Maybe UTCTime
  , userRoles :: [Role]
  , userMeta :: HashMap Text Value
  } deriving (Show,Eq)


defAuthUser :: AuthUser
defAuthUser = AuthUser {
    userId = Nothing
  , userLogin = ""
  , userPassword = Nothing
  , userActivatedAt = Nothing
  , userSuspendedAt = Nothing
  , userRememberToken = Nothing
  , userLoginCount = 0
  , userFailedLoginCount = 0
  , userLockedOutUntil = Nothing
  , userCurrentLoginAt = Nothing
  , userLastLoginAt = Nothing
  , userCurrentLoginIp = Nothing
  , userLastLoginIp = Nothing
  , userCreatedAt = Nothing
  , userUpdatedAt = Nothing
  , userRoles = []
  , userMeta = HM.empty
}


------------------------------------------------------------------------------
-- | Set a new password for the given user. Given password should be
-- clear-text; it will be encrypted into a 'Encrypted'.
setPassword :: AuthUser -> ByteString -> IO AuthUser
setPassword au pass = do
  pw <- Encrypted `fmap` (makePassword pass 12)
  return $ au { userPassword = Just pw }


------------------------------------------------------------------------------
-- | Authetication settings defined at initialization time
data AuthSettings = AuthSettings {
    asMinPasswdLen :: Int
  -- ^ Currently not used/checked
  , asRememberCookieName :: ByteString
  -- ^ Name of the desired remember cookie
  , asRememberPeriod :: Maybe Int
  -- ^ How long to remember when the option is used in rest of the API.
  -- 'Nothing' means remember indefinitely.
  , asLockout :: Maybe (Int, NominalDiffTime)
  -- ^ Lockout strategy: ([MaxAttempts], [LockoutDuration])
  , asSiteKey :: FilePath
  -- ^ Location of app's encryption key 
}

defAuthSettings = AuthSettings {
    asMinPasswdLen = 8
  , asRememberCookieName = "remember"
  , asRememberPeriod = Just $ 14 * 24 * 60
  , asLockout = Nothing
  , asSiteKey = "site_key.txt"
}


data BackendError = 
    DuplicateLogin 
  | BackendError String
  deriving (Eq,Show,Read,Typeable)


instance Exception BackendError

