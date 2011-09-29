{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

  This module contains all the central authentication functionality.

  It exports a number of high-level functions to be used directly in your
  application handlers. 
  
  We also export a number of mid-level functions that
  should be helpful when you are integrating with another way of confirming the
  authentication of login requests.

-}

module Snap.Snaplet.Auth 
  ( 

  -- * Higher Level Handler Functions
    createUser
  , saveUser
  , destroyUser
  , loginByUsername
  , loginByRememberToken
  , forceLogin
  , logout
  , currentUser
  , isLoggedIn

  -- * Lower Level Functions
  , markAuthSuccess
  , markAuthFail
  , checkPasswordAndLogin

  -- * Types
  , AuthManager
  , IAuthBackend
  , AuthSettings(..)
  , defAuthSettings
  , AuthUser(..)
  , UserId(..)
  , Password(..)
  , AuthFailure(..)
  , BackendError(..)

  -- * Other Utilities
  , authenticatePassword
  , setPassword
  )
  where

import           Control.Monad.State
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Maybe (isJust)
import           Data.Time
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Text (Text)

import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.Auth.AuthManager as AM
import           Snap.Snaplet.Auth.AuthManager (IAuthBackend(..), AuthManager(..))
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Common
import           Snap.Snaplet.Session.SecureCookie



------------------------------------------------------------------------------
-- Higher level functions 
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Create a new user from just a username and password
--
-- May throw a "DuplicateLogin' if given username is not unique
createUser
  :: Text -- Username
  -> ByteString -- Password
  -> Handler b (AuthManager b) AuthUser
createUser unm pass = do
  (AuthManager r _ _ _ _ _ _ _) <- getSnapletState
  liftIO $ AM.createUser r unm pass


------------------------------------------------------------------------------
-- | Lookup a user by her username, check given password and perform login
loginByUsername
  :: ByteString       -- ^ Username/login for user
  -> Password         -- ^ Should be ClearText
  -> Bool             -- ^ Set remember token?
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
loginByUsername _ (Encrypted _) _ = error "Cannot login with encrypted password"
loginByUsername unm pwd rm  = do
  AuthManager r s _ _ cn rp sk _ <- getSnapletState
  au <- liftIO $ lookupByLogin r (decodeUtf8 unm)
  case au of
    Nothing  -> return $ Left UserNotFound
    Just au' -> do
      res <- checkPasswordAndLogin au' pwd
      case res of
        Left e -> return $ Left e
        Right au'' -> do
          case rm of
            True -> do
              token <- liftIO $ randomToken 64
              setRememberToken sk cn rp token
              let au''' = au'' { userRememberToken = Just (decodeUtf8 token) }
              saveUser au'''
              return $ Right au'''
            False -> return $ Right au''


------------------------------------------------------------------------------
-- | Remember user from the remember token if possible and perform login
loginByRememberToken :: Handler b (AuthManager b) (Maybe AuthUser)
loginByRememberToken = do
  mgr@(AuthManager r _ _ _ rc rp sk _) <- getSnapletState
  token <- getRememberToken sk rc rp
  au <- maybe (return Nothing) (liftIO . lookupByRememberToken r . decodeUtf8) token
  case au of
    Just au' -> forceLogin au' >> return au
    Nothing -> return Nothing


------------------------------------------------------------------------------
-- | Logout the active user
logout :: Handler b (AuthManager b) ()
logout = do 
  s <- getsSnapletState session
  withTop s $ withSession s removeSessionUserId 
  AuthManager _ _ _ _ rc _ _ _ <- getSnapletState
  forgetRememberToken rc
  modifySnapletState (\mgr -> mgr { activeUser = Nothing } )


------------------------------------------------------------------------------
-- | Return the current user; trying to remember from cookie if possible.
currentUser :: Handler b (AuthManager b) (Maybe AuthUser)
currentUser = cacheOrLookup f
  where 
    f = do
      mgr@(AuthManager r s _ _ _ _ _ _) <- getSnapletState
      uid <- withTop s getSessionUserId 
      case uid of
        Nothing -> loginByRememberToken 
        Just uid' -> liftIO $ lookupByUserId r uid'


------------------------------------------------------------------------------
-- | Convenience wrapper around 'rememberUser' that returns a bool result
isLoggedIn :: Handler b (AuthManager b) Bool
isLoggedIn = isJust `fmap` currentUser


------------------------------------------------------------------------------
-- | Create or update a given user
--
-- May throw a 'BackendError' if something goes wrong.
saveUser :: AuthUser -> Handler b (AuthManager b) AuthUser
saveUser u = do
  (AuthManager r _ _ _ _ _ _ _) <- getSnapletState
  liftIO $ save r u


------------------------------------------------------------------------------
-- | Destroy the given user
--
-- May throw a 'BackendError' if something goes wrong.
destroyUser :: AuthUser -> Handler b (AuthManager b) ()
destroyUser u = do
  (AuthManager r _ _ _ _ _ _ _) <- getSnapletState
  liftIO $ destroy r u


------------------------------------------------------------------------------
--  Lower level helper functions
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Mutate an 'AuthUser', marking failed authentication
--
-- This will save the user to the backend.
markAuthFail :: AuthUser -> Handler b (AuthManager b) AuthUser
markAuthFail u = do
  (AuthManager r _ _ _ _ _ _ lo) <- getSnapletState
  incFailCtr u >>= checkLockout lo >>= liftIO . save r
  where
    incFailCtr u' = return $ u' 
                      { userFailedLoginCount = userFailedLoginCount u' + 1}
    checkLockout lo u' = case lo of
      Nothing -> return u'
      Just (mx, wait) -> 
        case userFailedLoginCount u' >= mx of
          True -> do
            now <- liftIO getCurrentTime
            let reopen = addUTCTime wait now
            return $ u' { userLockedOutUntil = Just reopen }


------------------------------------------------------------------------------
-- | Mutate an 'AuthUser', marking successful authentication
--
-- This will save the user to the backend.
markAuthSuccess :: AuthUser -> Handler b (AuthManager b) AuthUser
markAuthSuccess u = do
  (AuthManager r _ _ _ _ _ _ _) <- getSnapletState
  now <- liftIO getCurrentTime
  incLoginCtr u >>= updateIp >>= updateLoginTS 
    >>= resetFailCtr >>= liftIO . save r
  where
    incLoginCtr u' = return $ u' { userLoginCount = userLoginCount u' + 1 }
    updateIp u' = do
      ip <- rqRemoteAddr `fmap` getRequest 
      return $ u' { userLastLoginIp = userCurrentLoginIp u'
                  , userCurrentLoginIp = Just ip }
    updateLoginTS u' = do
      now <- liftIO getCurrentTime
      return $
        u' { userCurrentLoginAt = Just now
           , userLastLoginAt = userCurrentLoginAt u' }
    resetFailCtr u' = return $ 
      u' { userFailedLoginCount = 0 
         , userLockedOutUntil = Nothing }


------------------------------------------------------------------------------
-- | Authenticate and log the user into the current session if successful.
--
-- This is a mid-level function exposed to allow roll-your-own ways of looking
-- up a user from the database.
--
-- This function will:
--
-- 1. Check the password
--
-- 2. Login the user into the current session
--
-- 3. Mark success/failure of the authentication trial on the user record
checkPasswordAndLogin
  :: AuthUser               -- ^ An existing user, somehow looked up from db
  -> Password               -- ^ A ClearText password
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
checkPasswordAndLogin u pw = 
  case userLockedOutUntil u of
    Just x -> do
      now <- liftIO getCurrentTime
      if now > x 
        then auth u
        else return . Left $ LockedOut x
    Nothing -> auth u
  where
    auth u = 
      case authenticatePassword u pw of
        Just e -> do
          markAuthFail u
          return $ Left e
        Nothing -> do
          forceLogin u 
          modifySnapletState (\mgr -> mgr { activeUser = Just u })
          u' <- markAuthSuccess u
          return $ Right u'


------------------------------------------------------------------------------
-- | Login and persist the given 'AuthUser' in the active session
--
-- Meant to be used if you have other means of being sure that the person is
-- who she says she is.
forceLogin 
  :: AuthUser
  -- ^ An existing user, somehow looked up from db
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
forceLogin u = do
  AuthManager _ s _ _ _ _ _ _ <- getSnapletState
  withSession s $ do
    case userId u of
      Just x -> do
        withTop s (setSessionUserId x) 
        return $ Right u
      Nothing -> return . Left $ 
        AuthError "forceLogin: Can't force the login of a user without userId"



------------------------------------------------------------------------------
-- Internal, non-exported helpers
--
------------------------------------------------------------------------------


getRememberToken sk rc rp = getSecureCookie rc sk rp


setRememberToken sk rc rp token = setSecureCookie rc sk rp token


forgetRememberToken rc = expireCookie rc (Just "/")

                                       
------------------------------------------------------------------------------
-- | Set the current user's 'UserId' in the active session
setSessionUserId :: UserId -> Handler b SessionManager ()
setSessionUserId (UserId t) = setInSession "__user_id" t


------------------------------------------------------------------------------
-- | Remove 'UserId' from active session, effectively logging the user out.
removeSessionUserId :: Handler b SessionManager ()
removeSessionUserId = deleteFromSession "__user_id"


------------------------------------------------------------------------------
-- | Get the current user's 'UserId' from the active session
getSessionUserId :: Handler b SessionManager (Maybe UserId)
getSessionUserId = do
  uid <- getFromSession "__user_id" 
  return $ uid >>= return . UserId


------------------------------------------------------------------------------
-- | Check password for a given user. 
--
-- Returns 'Nothing" if check is successful and an 'IncorrectPassword' error
-- otherwise
authenticatePassword 
  :: AuthUser        -- ^ Looked up from the back-end
  -> Password        -- ^ Check against this password
  -> Maybe AuthFailure
authenticatePassword u pw = auth
  where
    auth = case userPassword u of
      Nothing -> Just PasswordMissing
      Just upw -> check $ checkPassword pw upw 
    check b = if b then Nothing else Just IncorrectPassword


------------------------------------------------------------------------------
-- | Wrap lookups around request-local cache
cacheOrLookup 
  :: Handler b (AuthManager b) (Maybe AuthUser)
  -- ^ Lookup action to perform if request local cache is empty
  -> Handler b (AuthManager b) (Maybe AuthUser)
cacheOrLookup f = do
  au <- getsSnapletState activeUser
  if isJust au 
    then return au
    else do
      au' <- f
      modifySnapletState (\mgr -> mgr { activeUser = au' })
      return au'



