------------------------------------------------------------------------------
-- | Internal module exporting AuthManager implementation.
--
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Snap.Snaplet.Auth.AuthManager
  ( -- * AuthManager Datatype
    AuthManager(..)

    -- * Backend Typeclass
    , IAuthBackend(..)

    -- * Context-free Operations
    , buildAuthUser
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Time
import           Web.ClientSession

import           Snap.Snaplet
import           Snap.Snaplet.Session
import           Snap.Snaplet.Auth.Types


------------------------------------------------------------------------------
-- | Creates a new user from a username and password.
--
buildAuthUser :: IAuthBackend r =>
                 r            -- ^ An auth backend
              -> Text         -- ^ Username
              -> ByteString   -- ^ Password
              -> IO (Either AuthFailure AuthUser)
buildAuthUser r unm pass = do
  now <- getCurrentTime
  let au = defAuthUser {
              userLogin     = unm
            , userPassword  = Nothing
            , userCreatedAt = Just now
            , userUpdatedAt = Just now
            }
  au' <- setPassword au pass
  save r au'


------------------------------------------------------------------------------
-- | All storage backends need to implement this typeclass
--
class IAuthBackend r where
  -- | Create or update the given 'AuthUser' record.  A 'userId' of Nothing
  -- indicates that a new user should be created, otherwise the user
  -- information for that userId should be updated.
  save                  :: r -> AuthUser -> IO (Either AuthFailure AuthUser)
  lookupByUserId        :: r -> UserId   -> IO (Maybe AuthUser)
  lookupByLogin         :: r -> Text     -> IO (Maybe AuthUser)
  lookupByEmail         :: r -> Text     -> IO (Maybe AuthUser)
  lookupByRememberToken :: r -> Text     -> IO (Maybe AuthUser)
  destroy               :: r -> AuthUser -> IO ()


------------------------------------------------------------------------------
-- | Abstract data type holding all necessary information for auth operation
data AuthManager b = forall r. IAuthBackend r => AuthManager {
      backend               :: r
        -- ^ Storage back-end

    , session               :: SnapletLens b (SessionManager b)
        -- ^ A lens pointer to a SessionManager

    , activeUser            :: Maybe AuthUser
        -- ^ A per-request logged-in user cache

    , minPasswdLen          :: Int
        -- ^ Password length range

    , rememberCookieName    :: ByteString
        -- ^ Cookie name for the remember token

    , rememberCookieDomain  :: Maybe ByteString
        -- ^ Domain for which remember cookie will be created.

    , rememberPeriod        :: Maybe Int
        -- ^ Remember period in seconds. Defaults to 2 weeks.

    , siteKey               :: Key
        -- ^ A unique encryption key used to encrypt remember cookie

    , lockout               :: Maybe (Int, NominalDiffTime)
        -- ^ Lockout after x tries, re-allow entry after y seconds

    , randomNumberGenerator :: RNG
        -- ^ Random number generator
    }

instance IAuthBackend (AuthManager b) where
    save AuthManager{..} u = save backend u
    lookupByUserId AuthManager{..} u = lookupByUserId backend u
    lookupByLogin AuthManager{..} u = lookupByLogin backend u
    lookupByEmail AuthManager{..}  u = lookupByEmail backend u
    lookupByRememberToken AuthManager{..} u = lookupByRememberToken backend u
    destroy AuthManager{..} u = destroy backend u
