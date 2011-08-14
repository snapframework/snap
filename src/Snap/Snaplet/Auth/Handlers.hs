{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-|

  Provides generic, somewhat customizable handlers that can be plugged 
  directly into Snap applications.

  The core 'Snap.Auth' module is pretty much stand-alone and taking these as
  starting point examples, you should be able to write your own custom
  handlers.

-}

module Snap.Snaplet.Auth.Handlers 
  ( AuthHandlerConfig(..)
  , loginHandler
  , logoutHandler
  , requireUser
  ) where

import Data.ByteString (ByteString)

import Snap.Core
import Snap.Snaplet.Auth.Types
import Snap.Snaplet

data AuthHandlerConfig b = AuthHandlerConfig
    { passwordParamField :: ByteString
    , rememberField :: Maybe ByteString
    , loginFailure :: AuthFailure -> Handler b b ()
    , loginSuccess :: Handler b b ()
    , afterLogout :: Handler b b ()
    }

------------------------------------------------------------------------------
-- | A 'MonadSnap' handler that processes a login form. 
--
-- The request paremeters are passed to 'performLogin'
loginHandler :: MonadAuthUser (Handler b b)
             => ByteString 
             -- ^ The password param field
             -> Maybe ByteString
             -- ^ Remember field; Nothing if you want to remember function.
             -> (AuthFailure -> Handler b b ())
             -- ^ Upon failure
             -> Handler b b ()
             -- ^ Upon success
             -> Handler b b ()
loginHandler pwdf remf loginFail loginSucc = do
    password <- getParam pwdf
    remember <- maybe (return Nothing) getParam remf
    let r = maybe False (=="1") remember
    mMatch <- case password of
      Nothing -> return $ Left PasswordFailure
      Just p  -> performLogin p r
    either loginFail (const loginSucc) mMatch


------------------------------------------------------------------------------
-- | Simple handler to log the user out. Deletes user from session.
logoutHandler :: MonadAuthUser (Handler b b)
              => Handler b b ()
              -- ^ What to do after logging out
              -> Handler b b ()
logoutHandler target = performLogout >> target


------------------------------------------------------------------------------
-- | Require that an authenticated 'AuthUser' is present in the current session.
--
-- This function has no DB cost - only checks to see if a user_id is present in
-- the current session.
requireUser :: MonadAuthUser m => m a   
            -- ^ Do this if no authenticated user is present.
            -> m a    
            -- ^ Do this if an authenticated user is present.
            -> m a
requireUser bad good = authenticatedUserId >>= maybe bad (const good)
