{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Snap.Snaplet.Session.SessionManager where

import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Serialize (Serialize)
import           Data.Text (Text)
import           Prelude hiding (lookup)

import           Snap.Types (Snap)
import           Snap.Snaplet


-- | Any Haskell record that is a member of the 'ISessionManager' typeclass can
-- be stuffed inside a 'SessionManager' to enable all session-related
-- functionality.
data SessionManager = forall a. ISessionManager a => SessionManager a


class ISessionManager r where

  -- | Load a session from given payload. 
  --
  -- Will always be called before any other operation. If possible, cache and
  -- do nothing when called multiple times within the same request cycle.
  load :: r -> Snap r

  -- | Commit session, return a possibly updated paylaod
  commit :: r -> Snap ()

  -- | Reset session
  reset :: r -> Snap r

  -- | Touch session
  touch :: r -> r

  -- | Insert a key-value pair into session
  insert :: Text -> Text -> r -> r
  
  -- | Lookup a key in session
  lookup :: Text -> r -> (Maybe Text)

  -- | Delete a key in session
  delete :: Text -> r -> r

  -- | Return a session-specific CSRF protection token. See 'mkCSRFToken' for
  -- help in creating the value.
  csrf :: r -> Text

  -- | Return all key-value pairs as an association list
  toList :: r -> [(Text,Text)]

