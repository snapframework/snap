{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}


module Snap.Snaplet.Heist.App
  ( App(..)
  , auth
  , heist
  , authInit
  , appInit
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.IO.Class
import           System.FilePath
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession

import           Paths_snap (getDataDir)

------------------------------------------------------------------------------
data App = App
    { _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _heist :: Snaplet (Heist App)
    }
$(makeLenses ''App)

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
dataDir :: IO FilePath
dataDir =  (</> "resources") <$> getDataDir

------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "foosnaplet" "Test application" (Just dataDir) $ do

    h <- nestSnaplet "" heist $ heistInit "templates"
    
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth authInit

    -- addAuthSplices auth --probably not necessary
    addTemplates h "extraTemplates"

    sPath    <- getSnapletFilePath
    let extraTemplatesPath = sPath </> "test" </> "heistTestApp" </> "templates" </> "evenMoreTemplates"
    liftIO $ putStrLn . ("dataDir: " ++) . show =<< dataDir
    liftIO $ print extraTemplatesPath
    liftIO $ threadDelay 500000

    addTemplatesAt h "evenMoreTemplates" extraTemplatesPath
    
    return $ App s a h


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
