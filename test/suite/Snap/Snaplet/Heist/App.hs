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
  , appInit'
  , appInitCompiled
  ) where


------------------------------------------------------------------------------
import           Control.Monad                                (when)
import           Control.Lens                                 (makeLenses)
import           System.FilePath                              ((</>))
------------------------------------------------------------------------------
import qualified Heist.Interpreted                            as I
import           Snap.Snaplet                                 (Snaplet,
                                                               SnapletInit,
                                                               getSnapletFilePath,
                                                               makeSnaplet,
                                                               nestSnaplet,
                                                               subSnaplet)
import           Snap.Snaplet.Auth                            (AuthManager,
                                                               defAuthSettings)
import qualified Snap.Snaplet.Heist                           as HS
import qualified Snap.Snaplet.Heist.Compiled                  as HSC
import qualified Snap.Snaplet.HeistNoClass                    as Unclassed
import           Snap.Snaplet.Session                         (SessionManager)
import           Snap.Snaplet.Auth.Backends.JsonFile          (initJsonFileAuthManager)
import           Snap.Snaplet.Session.Backends.CookieSession  (initCookieSessionManager)
import qualified Text.XmlHtml                                 as XML

------------------------------------------------------------------------------
data App = App
    { _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _heist :: Snaplet (HS.Heist App)
    }
$(makeLenses ''App)

instance HS.HasHeist App where
    heistLens = subSnaplet heist

aTestTemplate :: [XML.Node]
aTestTemplate = [XML.TextNode "littleTemplateNode"]


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = appInit' False

------------------------------------------------------------------------------
appInit' :: Bool -> SnapletInit App App
appInit' defInterp = makeSnaplet "foosnaplet" "Test application" Nothing $ do

    h <- nestSnaplet "" heist $ HS.heistInit "templates"

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth authInit

    -- addAuthSplices auth --probably not necessary
    HS.addTemplates h "extraTemplates"

    sPath    <- getSnapletFilePath
    let extraTemplatesPath = sPath </> "test" </> "evenMoreTemplates"

    HS.addTemplatesAt h "evenMoreTemplates" extraTemplatesPath
    HS.modifyHeistState (I.addTemplate "smallTemplate" aTestTemplate Nothing)

    when defInterp (Unclassed.setInterpreted h)

    return $ App s a h

appInitCompiled :: SnapletInit App App
appInitCompiled = makeSnaplet "foosnaplet" "TestApplication" Nothing $ do

  h <- nestSnaplet "" heist $ HSC.heistInit "templates"

  s <- nestSnaplet "sess" sess $
       initCookieSessionManager "site_key.txt" "sess" (Just 3600)

  a <- nestSnaplet "auth" auth authInit

  return $ App s a h

------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
