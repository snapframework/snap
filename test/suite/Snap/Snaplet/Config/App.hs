{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Config.App where

------------------------------------------------------------------------------
import Control.Lens                                  (makeLenses)
import Control.Monad.IO.Class                        (liftIO)
------------------------------------------------------------------------------
import Snap.Http.Server.Config                       (Config, completeConfig,
                                                      defaultConfig)
import Snap.Snaplet                                  (Handler, Initializer,
                                                      Snaplet, SnapletInit,
                                                      makeSnaplet,
                                                      nestSnaplet,
                                                      subSnaplet)
import Snap.Snaplet.Auth                             (AuthManager,
                                                      defAuthSettings)
import Snap.Snaplet.Auth.Backends.JsonFile           (initJsonFileAuthManager)
import Snap.Snaplet.Config                           (AppConfig,
                                                      commandLineAppConfig)
import Snap.Snaplet.Heist                            (Heist, HasHeist,
                                                      heistInit, heistLens)
import Snap.Snaplet.Session                          (SessionManager)
import Snap.Snaplet.Session.Backends.CookieSession   (initCookieSessionManager)

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

  cfg <- liftIO $ completeConfig =<< commandLineAppConfig defaultConfig :: Initializer App App (Config (Handler App App) AppConfig) --TODO doesn't seem to touch tests

  liftIO $ print cfg

  return $ App h a s


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
