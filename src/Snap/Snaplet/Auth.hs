{-# LANGUAGE ExistentialQuantification #-}

module Snap.Snaplet.Auth where

import           Control.Monad.State
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Maybe (isJust)
import           Data.Time

import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet
import           Snap.Snaplet.Session



-- $higherlevel
-- These are the key functions you will use in your handlers. 


------------------------------------------------------------------------------
-- | Lookup a user by her username, check given password and perform login
loginByUsername
  :: ByteString       -- ^ Username/login for user
  -> Password         -- ^ Should be ClearText
  -> Bool             -- ^ Set remember token?
  -> Handler b (AuthManager b) (Maybe AuthUser)
loginByUsername = undefined


------------------------------------------------------------------------------
-- | Remember user from the remember token if possible.
rememberUser :: Handler b (AuthManager b) (Maybe AuthUser)
rememberUser = cacheOrLookup f
  where 
    f = do
      mgr@(AuthManager r _ _ _ rc to _) <- get
      uid <- undefined
      case uid of
        Nothing -> return Nothing
        Just uid' -> liftIO $ lookupByUserId r uid'


------------------------------------------------------------------------------
-- Logout the active user
logout :: Handler b (AuthManager b) ()
logout = do 
  s <- gets session
  withTop s removeSessionUserId 
  modify (\mgr -> mgr { activeUser = Nothing } )


------------------------------------------------------------------------------
-- | Return the current user
currentUser :: Handler b (AuthManager b) (Maybe AuthUser)
currentUser = cacheOrLookup f
  where 
    f = do
      mgr@(AuthManager r s _ _ _ _ _) <- get
      uid <- withTop s getSessionUserId 
      case uid of
        Nothing -> return Nothing
        Just uid' -> liftIO $ lookupByUserId r uid'


------------------------------------------------------------------------------
-- | Convenience wrapper around 'rememberUser' that returns a bool result
isLoggedIn :: Handler b (AuthManager b) Bool
isLoggedIn = do
  au <- currentUser
  return $ if isJust au then True else False 


-- $midlevel
-- You might need these if you are rolling your own handlers/authenticators

------------------------------------------------------------------------------
-- | Mutate an 'AuthUser', marking failed authentication now.
markAuthFail :: AuthUser -> Handler b (AuthManager b) AuthUser
markAuthFail u = do
  (AuthManager r _ _ _ _ _ _) <- get
  proc u >>= liftIO . save r
  where
    proc = incFailCtr >=> checkLockout
    incFailCtr = undefined
    checkLockout = undefined


------------------------------------------------------------------------------
-- | Mutate an 'AuthUser', marking successful authentication now.
markAuthSuccess :: AuthUser -> Handler b (AuthManager b) AuthUser
markAuthSuccess u = do
  (AuthManager r _ _ _ _ _ _) <- get
  proc u >>= liftIO . save r
  where
    proc = incLoginCtr >=> updateIp >=> updateLoginTS >=> 
           setRememberToken >=> resetFailCtr
    incLoginCtr = undefined
    updateIp = undefined
    updateLoginTS = undefined
    setRememberToken = undefined
    resetFailCtr = undefined


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
  :: AuthUser
  -> Password               -- ^ A ClearText password
  -> Bool                   -- ^ Set remember cookie?
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
checkPasswordAndLogin u pw remember = 
  case authenticatePassword u pw of
    Just e -> do
      markAuthFail u
      return $ Left e
    Nothing -> do
      forceLoginUser u remember
      modify (\mgr -> mgr { activeUser = Just u })
      u' <- markAuthSuccess u
      return $ Right u'


------------------------------------------------------------------------------
-- | Login and persist the given 'AuthUser' in the active session
--
-- Meant to be used if you have other means of being sure that the person is
-- who she says she is.
forceLoginUser 
  :: AuthUser 
  -> Bool                               -- ^ Set remember cookie?
  -> Handler b (AuthManager b) Bool
forceLoginUser u rc = do
  AuthManager _ s _ _ _ _ _ <- get
  case userId u of
    Just x -> withTop s (setSessionUserId x) >> return True
    Nothing -> return False


-- $lowlevel
-- You shouldn't need to use these explicitly

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
  au <- gets activeUser
  if isJust au then return au
  else do
    au' <- f
    modify (\mgr -> mgr { activeUser = au' })
    return au'



