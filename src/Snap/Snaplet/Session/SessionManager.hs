{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-| This module is meant to be used mainly by Session backend
developers, who would naturally need access to ISessionManager class
internals. You can also use it if you need low-level access to the
backend functionality.-}

module Snap.Snaplet.Session.SessionManager where

-------------------------------------------------------------------------------
import           Data.Text    (Text)
import           Prelude      hiding (lookup)
-------------------------------------------------------------------------------
import           Snap.Snaplet (Handler)
-------------------------------------------------------------------------------



-- | Any Haskell record that is a member of the 'ISessionManager'
-- typeclass can be stuffed inside a 'SessionManager' to enable all
-- session-related functionality.
--
-- To use sessions in your application, just find a Backend that would
-- produce one for you inside of your 'Initializer'. See
-- 'initCookieSessionManager' in
-- 'Snap.Snaplet.Session.Backends.CookieSession' for a built-in option
-- that would get you started.
data SessionManager b = forall a. ISessionManager a b => SessionManager a


class ISessionManager r b | r -> b where

  -- | Load a session from given payload.
  --
  -- Will always be called before any other operation. If possible, cache and
  -- do nothing when called multiple times within the same request cycle.
  load :: r -> Handler b (SessionManager b) r

  -- | Commit session, return a possibly updated paylaod
  commit :: r -> Handler b (SessionManager b) ()

  -- | Reset session
  reset :: r -> Handler b (SessionManager b) r

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

