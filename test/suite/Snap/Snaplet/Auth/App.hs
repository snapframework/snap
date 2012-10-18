
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}


module Snap.Snaplet.Auth.App
  ( App(..)
  , auth
  , authInit
  , appInit
  ) where


------------------------------------------------------------------------------
import           Data.Lens.Template
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }

makeLens ''App


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth authInit

    -- addAuthSplices auth --probably not necessary
    return $ App s a


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
