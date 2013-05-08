{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Snap.Snaplet.Auth.Backends.Ldap
  ( initLdapAuthManager
  , mkLdapAuthMgr
  ) where


import           Control.Applicative
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Attoparsec as Atto
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as HM
import           Data.Map (Map)
import           Data.Maybe (fromJust, isJust)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Time
import           Web.ClientSession
import           System.Directory

import           Snap.Snaplet
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet.Auth.AuthManager
import           Snap.Snaplet.Session
import qualified LDAP as L
import qualified Data.ByteString.UTF8 as Butf8

------------------------------------------------------------------------------
-- | Initialize a JSON file backed 'AuthManager'
initLdapAuthManager :: AuthSettings
                    -- ^ Authentication settings for your app
                    -> Text
                    -- ^ LDAP hostname
                    -> Integer
                    -- ^ LDAP port
                    -> Maybe Text
                    -- ^ username postfix
                    -> SnapletLens b SessionManager
                    -- ^ Lens into a 'SessionManager' auth snaplet will
                    -- use
                    -> SnapletInit b (AuthManager b)
initLdapAuthManager s hn prt postfix l = do
  makeSnaplet
    "LdapAuthManager"
    "A snaplet providing user authentication using LDAP backend"
    Nothing $ liftIO $ do
      rng <- liftIO mkRNG
      key <- getKey (asSiteKey s)
      ldapMgr <- mkLdapAuthMgr hn prt postfix
      return $! AuthManager {
                  backend               = ldapMgr
                  , session               = l
                  , activeUser            = Nothing
                  , minPasswdLen          = asMinPasswdLen s
                  , rememberCookieName    = asRememberCookieName s
                  , rememberPeriod        = asRememberPeriod s
                  , siteKey               = key
                  , lockout               = asLockout s
                  , randomNumberGenerator = rng
                  }
  

------------------------------------------------------------------------------
-- | Load/create a datafile into memory cache and return the manager.
--
-- This data type can be used by itself for batch/non-handler processing.
mkLdapAuthMgr :: Text -> Integer -> Maybe Text -> IO LdapAuthManager
mkLdapAuthMgr hn prt postfix = do
  return $! LdapAuthManager {
      hostname = hn
    , port = prt
    , queryUser = Nothing
    , queryPwd = Nothing
    , usernamePostfix = postfix
  }


------------------------------------------------------------------------------
data LdapAuthManager = LdapAuthManager {
      hostname :: Text
    , port :: Integer
    , queryUser :: Maybe Text
    , queryPwd :: Maybe Text
    , usernamePostfix :: Maybe Text
    }

      ------------------------------------------------------------------------------
instance IAuthBackend LdapAuthManager where
  save r = return . Right

  destroy = error "LdapAuthManager: destroy is not yet implemented"

  lookupByUserId mgr u@(UserId uid) = return $ Just (defAuthUser { userId = Just $ u
                                                                 , userLogin = uid })
  lookupByLogin mgr login = return $ Just (defAuthUser { userId = Just $ UserId login
                                                       , userLogin = login })

  lookupByRememberToken mgr token = error "LdapAuthManager : lookupByRememberToken is not yet implemented"
  authenticate mgr usr pwd = case pwd of
      ClearText pwd' -> do 
        ld <- L.ldapInit (unpack $ hostname mgr) (fromInteger $ port mgr)
        x' <- L.handleLDAP (return . Just . AuthError . show)
              (L.ldapSimpleBind ld getDn (Butf8.toString pwd') >> return Nothing)
        return x'
      Encrypted _ -> return $ Just $ AuthError "cannot do LDAP authentication with encrypted password"
    where getDn = let usrStr = T.unpack $ userLogin usr
                  in maybe usrStr (\x -> usrStr ++ T.unpack x) (usernamePostfix mgr)
