{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-|

  This module provides simple and secure high-level authentication
  functionality for Snap applications.

-}
module Snap.Snaplet.Auth.Types where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import           Data.Time

import           Snap.Types
import           Snap.Snaplet
import           Snap.Snaplet.Auth.Password
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Common
import           Snap.Snaplet.Session.SecureCookie
import           Snap.Snaplet.Session.Types


------------------------------------------------------------------------------
-- | Password is clear when supplied by the user and encrypted later when
-- returned from the db.
data Password = ClearText ByteString
              | Encrypted ByteString
              deriving (Read, Show, Ord, Eq)


------------------------------------------------------------------------------
-- | Authentication failures indicate what went wrong during authentication.
-- They may provide useful information to the developer, although it is
-- generally not advisable to show the user the exact details about why login
-- failed.
data AuthFailure = ExternalIdFailure
                 | PasswordFailure
                 deriving (Read, Show, Ord, Eq)

------------------------------------------------------------------------------
-- | Type representing the concept of a User in your application.
data AuthUser = AuthUser
  { userId               :: Maybe UserId
  , userUsername         :: Maybe ByteString
  , userEmail            :: Maybe ByteString
  , userPassword         :: Maybe Password
  , userSalt             :: Maybe ByteString
  , userActivatedAt      :: Maybe UTCTime
  , userSuspendedAt      :: Maybe UTCTime
  {-, userPerishableToken :: Maybe ByteString-}
  , userPersistenceToken :: Maybe ByteString
  {-, userSingleAccessToken :: Maybe ByteString-}
  , userLoginCount       :: Int
  , userFailedLoginCount :: Int
  , userCurrentLoginAt   :: Maybe UTCTime
  , userLastLoginAt      :: Maybe UTCTime
  , userCurrentLoginIp   :: Maybe ByteString
  , userLastLoginIp      :: Maybe ByteString
  , userCreatedAt        :: Maybe UTCTime
  , userUpdatedAt        :: Maybe UTCTime
  } deriving (Read,Show,Ord,Eq)


------------------------------------------------------------------------------
-- | A blank 'User' as a starting point
emptyAuthUser :: AuthUser
emptyAuthUser = AuthUser
  { userId               = Nothing
  , userUsername         = Nothing
  , userEmail            = Nothing
  , userPassword         = Nothing
  , userSalt             = Nothing
  , userActivatedAt      = Nothing
  , userSuspendedAt      = Nothing
  {-, userPerishableToken = Nothing-}
  , userPersistenceToken = Nothing
  {-, userSingleAccessToken = Nothing-}
  , userLoginCount       = 0
  , userFailedLoginCount = 0
  , userCurrentLoginAt   = Nothing
  , userLastLoginAt      = Nothing
  , userCurrentLoginIp   = Nothing
  , userLastLoginIp      = Nothing
  , userCreatedAt        = Nothing
  , userUpdatedAt        = Nothing
  }


------------------------------------------------------------------------------
-- | Make 'SaltedHash' from 'AuthUser'
mkSaltedHash :: AuthUser -> SaltedHash
mkSaltedHash u = SaltedHash s p'
  where s = Salt . B.unpack $ s'
        s' = maybe (error "No user salt") id $ userSalt u
        p' = case p of
          ClearText _ ->
            error "Can't mkSaltedHash with a ClearText user password"
          Encrypted x -> B.unpack x
        p = maybe (error "Can't mkSaltedHash with empty password") id $
            userPassword u

------------------------------------------------------------------------------
-- | Typeclass for authentication and user session functionality.
--
-- Your have to make your Application's monad a member of this typeclass.
--
--  - Your app monad has to be a 'MonadSnap'.
--
--  - Your app monad has to be a 'MonadSession'. See 'Snap.Snaplet.Session'.
--  This is needed so we can persist your users' login in session.
class (MonadSnap m, MonadSession m, MonadAuth m) => MonadAuthUser m where
    type Extra m :: *

    --------------------------------------------------------------------------
    -- | Define a function that can resolve to a 'AuthUser' from an internal
    -- 'UserId'.
    --
    -- The 'UserId' is persisted in your application's session
    -- to check for the existence of an authenticated user in your handlers.
    -- A typical 'UserId' would be the unique database key given to your user's
    -- record.
    getUserInternal :: UserId -> m (Maybe (AuthUser, (Extra m)))

    --------------------------------------------------------------------------
    -- | Define a function that can resolve to a 'AuthUser'
    --
    -- This is typically passed directly from the POST request.
    getUserExternal :: m (Maybe (AuthUser, (Extra m)))

    --------------------------------------------------------------------------
    -- | A way to find users by the remember token.
    getUserByRememberToken :: ByteString -> m (Maybe (AuthUser, (Extra m)))

    --------------------------------------------------------------------------
    -- | Implement a way to save given user in the DB.
    saveAuthUser :: (AuthUser, (Extra m)) -> m (Maybe AuthUser)



type instance Base (Handler b AuthEnv) = b

class MonadAuth m where
    withAuth :: Handler (Base m) AuthEnv a -> m a

instance MonadAuth (Handler b AuthEnv) where
    withAuth = id


------------------------------------------------------------------------------
-- | Typeclass for authentication and user session functionality.
--
-- Your have to make your Application's monad a member of this typeclass.
-- Minimum complete definition: 'getUserInternal', 'getUserExternal'
--
-- Requirements:
--
--  - Your app monad has to be a 'MonadSnap'.
--
--  - Your app monad has to be a 'MonadSession'. See 'Snap.Snaplet.Session'.
--  This is needed so we can persist your users' login in session.
data AuthEnv = AuthEnv

    { authHash                   :: HashFunc
    -- ^ Define a hash function to be used. Defaults to 'defaultHash', which
    -- should be quite satisfactory for most purposes.

    , authUserTable              :: String
    -- ^ Name of the table that will store user data

    , authMinPasswordLength      :: Int
    -- ^ Password length range

    , authAuthenticationKeys     :: [ByteString]
    -- ^ What are the database fields and the user-supplied
    -- fields that are going to be used to find a user?

    , authRememberCookieName     :: ByteString
    -- ^ Cookie name for the remember token

    , authRememberPeriod         :: Int
    -- ^ Remember period in seconds. Defaults to 2 weeks.

    , authRememberAcrossBrowsers :: Bool
    -- ^ Should it be possible to login multiple times?

    , authEmailValidationRegex   :: ByteString

    , authLockoutStrategy        :: Maybe (Int, Int)
    -- ^ Lockout after x tries, re-allow entry after y seconds
    }


defaultAuthEnv :: AuthEnv
defaultAuthEnv = AuthEnv
    { authHash = defaultHash
    , authUserTable = "users"
    , authMinPasswordLength = 7
    , authAuthenticationKeys = ["email"]
    , authRememberCookieName = "auth_remember_token"
    , authRememberPeriod = 60 * 60 * 24 * 14
    , authRememberAcrossBrowsers = True
    , authEmailValidationRegex =
        "^([\\w\\.%\\+\\-]+)@([\\w\\-]+\\.)+([\\w]{2,})$"
    , authLockoutStrategy = Nothing
    }


------------------------------------------------------------------------------
-- | Authenticates a user.
--
-- Returns the internal 'UserId' if successful, 'Nothing' otherwise.
-- Note that this will not persist the authentication. See 'performLogin' for
-- that.
authenticate :: MonadAuthUser m
             => ByteString            -- ^ Password
             -> Bool                  -- ^ Remember user?
             -> m (Either AuthFailure (AuthUser, Extra m))
authenticate password remember = do
    hf <- withAuth $ gets authHash
    user <- getUserExternal
    case user of
      Nothing            -> return $ Left ExternalIdFailure
      Just user'@(u', _) -> case check hf password u' of
        True -> do
          markLogin user'
          return $ Right user'
        False -> do
          markLoginFail user'
          return $ Left PasswordFailure
    where
      check hf p u = checkSalt hf p $ mkSaltedHash u

      markLoginFail (u,d) = do
        u' <- incFailLogCtr u
        saveAuthUser (u', d)

      markLogin :: (MonadAuthUser m) => (AuthUser, Extra m) -> m (Maybe AuthUser)
      markLogin (u,d) = do
        u' <- (incLogCtr >=> updateIP >=> updateLoginTS >=>
               setPersistenceToken) u
        saveAuthUser (u', d)

      incLogCtr :: (MonadAuthUser m) => AuthUser -> m AuthUser
      incLogCtr u = return $ u { userLoginCount = userLoginCount u + 1 }

      incFailLogCtr :: (MonadAuthUser m) => AuthUser -> m AuthUser
      incFailLogCtr u = return $
        u { userFailedLoginCount = userFailedLoginCount u + 1 }

      updateIP :: (MonadAuthUser m) => AuthUser -> m AuthUser
      updateIP u = do
        ip <- getRequest >>= return . rqRemoteAddr
        return $
          u { userCurrentLoginIp = Just ip
            , userLastLoginIp = userCurrentLoginIp u }

      updateLoginTS :: (MonadAuthUser m) => AuthUser -> m AuthUser
      updateLoginTS u = do
        t <- liftIO getCurrentTime
        return $
          u { userCurrentLoginAt = Just t
            , userLastLoginAt = userCurrentLoginAt u }

      setPersistenceToken u = do
        multi_logon <- withAuth $ gets authRememberAcrossBrowsers
        to <- withAuth $ gets authRememberPeriod
        site_key <- secureSiteKey
        cn <- withAuth $ gets authRememberCookieName
        rt <- liftIO $ randomToken 15
        token <- case userPersistenceToken u of
          Nothing -> return rt
          Just x -> if multi_logon then return x else return rt
        case remember of
          False -> return u
          True -> do
            setSecureCookie cn site_key token (Just to)
            return $ u { userPersistenceToken = Just token }



-- $higherlevel
-- These are the key functions you will use in your handlers. Once you have set
-- up your application's monad with 'MonadAuth', you really should not need to
-- use anything other than what is in this section.


------------------------------------------------------------------------------
-- | Authenticates the user and persists the authentication in the session if
-- successful.
performLogin :: MonadAuthUser m
             => ByteString            -- ^ Password
             -> Bool                  -- ^ Remember user?
             -> m (Either AuthFailure (AuthUser, Extra m))
performLogin p r = authenticate p r >>= either (return . Left) login
  where
    login x@(user, _) = do
      setSessionUserId (userId user)
      return (Right x)


------------------------------------------------------------------------------
-- | Logs a user out from the current session.
performLogout :: MonadAuthUser m => m ()
performLogout = do
  cn <- withAuth $ gets authRememberCookieName
  let ck = Cookie cn "" Nothing Nothing (Just "/")
  modifyResponse $ addResponseCookie ck
  setSessionUserId Nothing


------------------------------------------------------------------------------
-- | Takes a clean-text password and returns a fresh pair of password and salt
-- to be stored in your app's DB.
mkAuthCredentials :: MonadAuthUser m
                  => ByteString
                  -- ^ A given password
                  -> m (ByteString, ByteString)
                  -- ^ (Salt, Encrypted password)
mkAuthCredentials pwd = do
  hf <- withAuth $ gets authHash
  SaltedHash (Salt s) pwd' <- liftIO $ buildSaltAndHash hf pwd
  return $ (B.pack s, B.pack pwd')


------------------------------------------------------------------------------
-- | True if a user is present in current session.
isLoggedIn :: MonadAuthUser m => m Bool
isLoggedIn = authenticatedUserId >>= return . maybe False (const True)


------------------------------------------------------------------------------
-- | Get the current 'AuthUser' if authenticated, 'Nothing' otherwise.
currentAuthUser :: MonadAuthUser m => m (Maybe (AuthUser, Extra m))
currentAuthUser = authenticatedUserId >>= maybe (return Nothing) getUserInternal


------------------------------------------------------------------------------
-- | Return if there is an authenticated user id. Try to remember the user
-- if possible.
authenticatedUserId :: MonadAuthUser m => m (Maybe UserId)
authenticatedUserId = getSessionUserId >>= maybe rememberUser (return . Just)

------------------------------------------------------------------------------
-- | Remember user from remember token if possible.
rememberUser :: MonadAuthUser m => m (Maybe UserId)
rememberUser = do
  to <- withAuth $ gets authRememberPeriod
  key <- secureSiteKey
  cn <- withAuth $ gets authRememberCookieName
  remToken <- getSecureCookie cn key (Just to)
  u <- maybe (return Nothing) getUserByRememberToken remToken
  case u of
    Nothing -> return Nothing
    Just (au, _) -> do
      setSessionUserId $ userId au
      return $ userId au

