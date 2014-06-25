{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Config.App where

------------------------------------------------------------------------------
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.IO.Class
------------------------------------------------------------------------------
import Heist
import Snap.Core
import Snap.Http.Server.Config
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Config
import Snap.Snaplet.Heist
import Snap.Snaplet.Internal.Initializer
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------
data App = App {
    _heist :: Snaplet (Heist App)
  , _auth  :: Snaplet (AuthManager App)
  , _sess  :: Snaplet SessionManager
  }
$(makeLenses ''App)

instance HasHeist App where
  heistLens = subSnaplet heist

------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do

  h <- nestSnaplet "heist" heist $ heistInit "templates"
  a <- nestSnaplet "auth"  auth    authInit
  s <- nestSnaplet "sess"  sess  $ initCookieSessionManager
                                   "site_key.txt" "sess" (Just 3600)

--  liftIO $ print $ appOpts defaultConfig
  cfg <- liftIO $ completeConfig =<< commandLineAppConfig defaultConfig :: Initializer App App (Config (Handler App App) AppConfig) --TODO doesn't seem to touch tests

  liftIO $ print cfg

  return $ App h a s


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
