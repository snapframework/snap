{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

  Provides generic, somewhat customizable handlers that can be plugged 
  directly into Snap applications.

  The core 'Snap.Auth' module is pretty much stand-alone and taking these as
  starting point examples, you should be able to write your own custom
  handlers.

-}

module Snap.Snaplet.Auth.Handlers 
  ( 
    registerUser
  , loginUser
  , logoutUser
  , requireUser
  ) where

import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Crypto.PasswordStore
import           Data.ByteString (ByteString)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text)
import           Data.Time

import Snap.Core
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Types
import Snap.Snaplet



registerUser
  :: ByteString -- Login field
  -> ByteString -- Password field
  -> Handler b (AuthManager b) AuthUser
registerUser lf pf = do
  mgr@(AuthManager r _ _ _ _ _ _ _) <- get
  l <- fmap decodeUtf8 `fmap` getParam lf
  p <- getParam pf
  case liftM2 (,) l p of
    Nothing -> throw PasswordMissing
    Just (lgn, pass) -> do
      createUser lgn pass


------------------------------------------------------------------------------
-- | A 'MonadSnap' handler that processes a login form.
--
-- The request paremeters are passed to 'performLogin'
loginUser 
  :: ByteString
  -- ^ The password param field
  -> Maybe ByteString
  -- ^ Remember field; Nothing if you want no remember function.
  -> (AuthFailure -> Handler b (AuthManager b) ())
  -- ^ Upon failure
  -> Handler b (AuthManager b) ()
  -- ^ Upon success
  -> Handler b (AuthManager b) ()
loginUser pwdf remf loginFail loginSucc = do
    password <- getParam pwdf
    remember <- maybe (return Nothing) getParam remf
    let r = maybe False (=="1") remember
    mMatch <- case password of
      Nothing -> return $ Left IncorrectPassword
      Just p  -> checkPasswordAndLogin undefined (ClearText p) r
    either loginFail (const loginSucc) mMatch


------------------------------------------------------------------------------
-- | Simple handler to log the user out. Deletes user from session.
logoutUser 
  :: Handler b (AuthManager b) ()
  -- ^ What to do after logging out
  -> Handler b (AuthManager b) ()
logoutUser target = logout >> target


------------------------------------------------------------------------------
-- | Require that an authenticated 'AuthUser' is present in the current session.
--
-- This function has no DB cost - only checks to see if a user_id is present in
-- the current session.
requireUser 
  :: Handler b (AuthManager b) a
  -- ^ Do this if no authenticated user is present.
  -> Handler b (AuthManager b) a
  -- ^ Do this if an authenticated user is present.
  -> Handler b (AuthManager b) a
requireUser bad good = do
  loggedIn <- isLoggedIn
  if loggedIn then good else bad
