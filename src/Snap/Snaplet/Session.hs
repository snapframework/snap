module Snap.Snaplet.Session
  ( SessionManager
  , withSession
  , commitSession
  , setInSession
  , getFromSession
  , deleteFromSession
  , csrfToken
  , sessionToList
  , resetSession
  , touchSession

  -- * Utilities Exported For Convenience
  , module Snap.Snaplet.Session.Common
  , module Snap.Snaplet.Session.SecureCookie
  ) where

------------------------------------------------------------------------------
import           Control.Monad.State
import           Data.Text                           (Text)
import           Snap.Core
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Session.Common
import           Snap.Snaplet.Session.SecureCookie
import           Snap.Snaplet.Session.SessionManager 
                   ( ISessionManager(..), SessionManager(..) )
import qualified Snap.Snaplet.Session.SessionManager as SM
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Wrap around a handler, committing any changes in the session at the end
--
withSession :: SnapletLens b SessionManager
            -> Handler b v a
            -> Handler b v a
withSession l h = do
    a <- h
    withTop l commitSession
    return a


------------------------------------------------------------------------------
-- | Commit changes to session within the current request cycle
--
commitSession :: Handler b SessionManager ()
commitSession = do
    SessionManager b <- loadSession
    liftSnap $ commit b


------------------------------------------------------------------------------
-- | Set a key-value pair in the current session
--
setInSession :: Text -> Text -> Handler b SessionManager ()
setInSession k v = do
    SessionManager r <- loadSession
    let r' = SM.insert k v r
    put $ SessionManager r'


------------------------------------------------------------------------------
-- | Get a key from the current session
--
getFromSession :: Text -> Handler b SessionManager (Maybe Text)
getFromSession k = do
    SessionManager r <- loadSession
    return $ SM.lookup k r


------------------------------------------------------------------------------
-- | Remove a key from the current session
--
deleteFromSession :: Text -> Handler b SessionManager ()
deleteFromSession k = do
    SessionManager r <- loadSession
    let r' = SM.delete k r
    put $ SessionManager r'


------------------------------------------------------------------------------
-- | Returns a CSRF Token unique to the current session
--
csrfToken :: Handler b SessionManager Text
csrfToken = do
    mgr@(SessionManager r) <- loadSession
    put mgr
    return $ SM.csrf r


------------------------------------------------------------------------------
-- | Return session contents as an association list
--
sessionToList :: Handler b SessionManager [(Text, Text)]
sessionToList = do
    SessionManager r <- loadSession
    return $ SM.toList r


------------------------------------------------------------------------------
-- | Deletes the session cookie, effectively resetting the session
--
resetSession :: Handler b SessionManager ()
resetSession = do
    SessionManager r <- loadSession
    r' <- liftSnap $ SM.reset r
    put $ SessionManager r'


------------------------------------------------------------------------------
-- | Touch the session so the timeout gets refreshed
--
touchSession :: Handler b SessionManager ()
touchSession = do
    SessionManager r <- loadSession
    let r' = SM.touch r
    put $ SessionManager r'


------------------------------------------------------------------------------
-- | Load the session into the manager
--
loadSession :: Handler b SessionManager SessionManager
loadSession = do
    SessionManager r <- get
    r' <- liftSnap $ load r
    return $ SessionManager r'

