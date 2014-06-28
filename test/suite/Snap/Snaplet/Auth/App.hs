{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}

module Snap.Snaplet.Auth.App
  ( App(..)
  , auth
  , heist
  , authInit
  , appInit
  , appInit'
  ) where

------------------------------------------------------------------------------
import           Control.Lens                                (makeLenses)
import           Control.Monad.Trans                         (lift)
import           Data.Monoid                                 (mempty)
------------------------------------------------------------------------------
import           Data.Map.Syntax                             ((#!))
import           Heist                                       (Splices,
                                                              hcCompiledSplices)
import qualified Heist.Compiled                              as C
import           Snap.Core                                   (pass)
import           Snap.Snaplet                                (Handler,
                                                              Snaplet,
                                                              SnapletInit,
                                                              makeSnaplet,
                                                              nestSnaplet,
                                                              subSnaplet,
                                                              with)
import           Snap.Snaplet.Auth                           (AuthManager,
                                                              AuthSettings(..),
                                                              addAuthSplices,
                                                              authSettingsFromConfig,
                                                              currentUser,
                                                              defAuthSettings,
                                                              userCSplices)
import           Snap.Snaplet.Session                        (SessionManager)
import           Snap.Snaplet.Auth.Backends.JsonFile         (initJsonFileAuthManager)
import           Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import           Snap.Snaplet.Heist                          (Heist,
                                                              HasHeist,
                                                              heistLens,
                                                              heistInit')


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
compiledSplices :: Splices (C.Splice (Handler App App))
compiledSplices = do
  "userSplice" #! C.withSplices C.runChildren userCSplices $
    lift $ maybe pass return =<< with auth currentUser


------------------------------------------------------------------------------
appInit' :: Bool -> SnapletInit App App
appInit' useConfigFile = makeSnaplet "app" "Test application" Nothing $ do

    h <- nestSnaplet "heist" heist $
           heistInit'
           "templates"
           (mempty {hcCompiledSplices = compiledSplices})

  
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    authSettings <- if useConfigFile
                    then authSettingsFromConfig
                    else return defAuthSettings
    
    a <- nestSnaplet "auth" auth $ authInit authSettings

    addAuthSplices h auth

    return $ App s a h


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = appInit' False


------------------------------------------------------------------------------
authInit :: AuthSettings -> SnapletInit App (AuthManager App)
authInit settings = initJsonFileAuthManager
           settings { asLockout = Just (3, 1) }
           sess "users.json"
