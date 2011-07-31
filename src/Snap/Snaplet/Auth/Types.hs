{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snap.Snaplet.Auth.Types where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Record.Label
import           Data.Time
import           Data.Text (Text)
import           Crypto.PasswordStore

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


------------------------------------------------------------------------------
-- | Authentication failures indicate what went wrong during authentication.
-- They may provide useful information to the developer, although it is
-- generally not advisable to show the user the exact details about why login
-- failed.
data AuthFailure = 
    FindFailure
  | IncorrectPassword
  | PasswordMissing
  | LockedOut Int               -- ^ Locked out with given seconds to go
  deriving (Read, Show, Ord, Eq)


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
  , userLockedOutAt :: Maybe UTCTime
  , userCurrentLoginAt :: Maybe UTCTime
  , userLastLoginAt :: Maybe UTCTime
  , userCurrentLoginIp :: Maybe ByteString
  , userLastLoginIp :: Maybe ByteString
  , userCreatedAt :: Maybe UTCTime
  , userUpdatedAt :: Maybe UTCTime
  , userRoles :: [Role]
  , userMeta :: HashMap Text Value
  } deriving (Show,Eq)



data AuthManager b = forall r. IAuthBackend r => AuthManager { 
	  backend :: r
	-- ^ Storage back-end 

	, session :: (b :-> Snaplet SessionManager)

  , minPasswdLen      :: Int
  -- ^ Password length range

  , rememberCookieName     :: ByteString
  -- ^ Cookie name for the remember token

  , rememberPeriod         :: Int
  -- ^ Remember period in seconds. Defaults to 2 weeks.

  , lockout        :: Maybe (Int, Int)
  -- ^ Lockout after x tries, re-allow entry after y seconds
  }


class IAuthBackend r where
  
  save :: r -> AuthUser -> IO AuthUser

  lookupByUserId :: r -> UserId -> IO (Maybe AuthUser)

  lookupByLogin :: r -> Text -> IO (Maybe AuthUser)

  lookupByRememberToken :: r -> Text -> IO (Maybe AuthUser)

  destroy :: r -> AuthUser -> IO ()


