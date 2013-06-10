{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

------------------------------------------------------------------------------
-- |
--
-- This module contains all the central authentication functionality.
--
-- It exports a number of high-level functions to be used directly in your
-- application handlers.
--
-- We also export a number of mid-level functions that should be helpful when
-- you are integrating with another way of confirming the authentication of
-- login requests.
--

module Snap.Snaplet.Auth
  (
  -- * Higher Level Handler Functions
    createUser
  , usernameExists
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
  , AuthManager(..)
  , IAuthBackend(..)
  , AuthSettings(..)
  , defAuthSettings
  , AuthUser(..)
  , defAuthUser
  , UserId(..)
  , Password(..)
  , AuthFailure(..)
  , Role(..)

  -- * Other Utilities
  , authSettingsFromConfig 
  , withBackend
  , encryptPassword
  , checkPassword
  , authenticatePassword
  , setPassword
  , encrypt
  , verify

  -- * Handlers
  , registerUser
  , loginUser
  , logoutUser
  , requireUser
  , setPasswordResetToken
  , clearPasswordResetToken

  -- * Splice helpers
  , addAuthSplices
  , compiledAuthSplices
  , userCSplices
  , userISplices
  , ifLoggedIn
  , ifLoggedOut
  , loggedInUser
  )
  where

------------------------------------------------------------------------------
import Snap.Snaplet.Auth.AuthManager
import Snap.Snaplet.Auth.Handlers
import Snap.Snaplet.Auth.SpliceHelpers
import Snap.Snaplet.Auth.Types

