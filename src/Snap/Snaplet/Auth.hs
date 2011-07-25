{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-|

  This module provides simple and secure high-level authentication
  functionality for Snap applications.

-}
module Snap.Snaplet.Auth
  (

  -- * Higher Level Functions
  -- $higherlevel
    mkAuthCredentials
  , performLogin
  , performLogout
  , currentAuthUser
  , isLoggedIn
  , authenticatedUserId

  -- * MonadAuth Class
  , MonadAuth(..)
  , MonadAuthUser(..)
  , AuthEnv(..)
  , defaultAuthEnv

  -- * Types
  , AuthUser(..)
  , emptyAuthUser
  , UserId(..)
  , Password(..)
  , AuthFailure(..)

  -- * Crypto Stuff You May Need
  , HashFunc

  , authInit

  ) where

import           Snap.Snaplet
import           Snap.Snaplet.Auth.Handlers
import           Snap.Snaplet.Auth.Password
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet.Session.Types

------------------------------------------------------------------------------
-- | Initializes the auth snaplet.
authInit :: (MonadAuthUser (Handler b b))
         => FilePath
         -> AuthEnv
         -> Maybe (AuthHandlerConfig b)
         -> Initializer b (Snaplet AuthEnv)
authInit dir env ahc = makeSnaplet "auth" $ do
    case ahc of
        Nothing -> return env
        (Just (AuthHandlerConfig a b c d e)) -> do
            -- We don't call "with authLens" here so we can avoid needing the
            -- lens passed into this function.  Might as well let the end user
            -- do it in a context where the lens is already available.
            addRoutes [ ("login", loginHandler a b c d)
                      , ("logout", logoutHandler e)
                      ]
            return env

