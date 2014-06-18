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
  ) where


------------------------------------------------------------------------------
import           Control.Monad
import           Control.Lens
import           System.FilePath
------------------------------------------------------------------------------
import           Data.Map.Syntax ((##))
import qualified Heist.Interpreted                            as I
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.HeistNoClass                    as Unclassed
import           Snap.Snaplet.Session
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Text.XmlHtml                                 as XML

------------------------------------------------------------------------------
data App = App
    { _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _heist :: Snaplet (Heist App)
    }
$(makeLenses ''App)

instance HasHeist App where
    heistLens = subSnaplet heist

aTestTemplate :: [XML.Node]
aTestTemplate = [XML.TextNode "littleTemplateNode"]


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = appInit' False

------------------------------------------------------------------------------
appInit' :: Bool -> SnapletInit App App
appInit' defInterp = makeSnaplet "foosnaplet" "Test application" Nothing $ do

    h <- nestSnaplet "" heist $ heistInit "templates"

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth authInit

    -- addAuthSplices auth --probably not necessary
    addTemplates h "extraTemplates"

    sPath    <- getSnapletFilePath
    let extraTemplatesPath = sPath </> "test" </> "evenMoreTemplates"

    addTemplatesAt h "evenMoreTemplates" extraTemplatesPath
    modifyHeistState (I.addTemplate "smallTemplate" aTestTemplate Nothing)

    when defInterp (Unclassed.setInterpreted h)

    return $ App s a h


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"
