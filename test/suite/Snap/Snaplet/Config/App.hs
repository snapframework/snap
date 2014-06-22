{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Config.App where

------------------------------------------------------------------------------
import Control.Lens
------------------------------------------------------------------------------
import Heist
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Config
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------
data App = App {
    _heist :: Snaplet (Heist App)
  , _auth  :: Snaplet (AuthManager App)
  , _sess  :: Snaplet SessionManager
  }
$(makeLenses ''App)


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do

  h <- nestSnaplet "heist" heist $ heistInit "templates"
  a <- nestSnaplet "auth"  auth    authInit
  s <- nestSnaplet "sess"  sess  $ initCookieSessionManager
                                   "site_key.txt" "sess" (Just 3600)

  return $ App h a s


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
