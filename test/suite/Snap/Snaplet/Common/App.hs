{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Snap.Snaplet.Common.App (
  appInit,
  appInit',
  heist,
  session,
  embedded,
  foo,
  bar
  )where

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
import Snap.Snaplet.Common.BarSnaplet
import Snap.Snaplet.Common.FooSnaplet
import Snap.Snaplet.Common.Handlers
import Snap.Snaplet.Common.Types
import Snap.Snaplet.Config                           (AppConfig,
                                                      commandLineAppConfig)
import Snap.Snaplet.Heist                            (Heist, HasHeist,
                                                      heistInit, heistLens)
import Snap.Snaplet.HeistNoClass                     (setInterpreted)
import Snap.Snaplet.Session                          (SessionManager)
import Snap.Snaplet.Session.Backends.CookieSession   (initCookieSessionManager)


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = appInit' False


------------------------------------------------------------------------------
appInit' :: Bool -> SnapletInit App App
appInit' hInterp = makeSnaplet "app" "Test application" Nothing $ do

  ------------------------------
  -- Initial subSnaplet setup --
  ------------------------------
  
  hs <- nestSnaplet "heist"   heist   $ heistInit "templates"
  sm <- nestSnaplet "session" session $
        initCookieSessionManager "sitekey.txt" "_session" (Just (30 * 60))
  au <- nestSnaplet "auth" auth authInit
  fs <- nestSnaplet "foo"     foo     $ fooInit hs
  bs <- nestSnaplet ""        bar     $ nameSnaplet "baz" $ barInit hs foo
  ns <- embedSnaplet "embed" embedded embeddedInit

  --------------------------------
  -- Exercise the Heist snaplet --
  --------------------------------
  
  addAuthSplices auth -- TODO/NOTE: probably not necessary (?)
  HS.addTemplates h "extraTemplates"

  sPath    <- getSnapletFilePath

  let extraTemplatesPath = sPath </> "test" </> "evenMoreTemplates"   -- TODO/NODE is this right?
  HS.addTemplatesAt h "evenMoreTemplates" extraTemplatesPath  -- TODO/NODE is this right?
  HS.modifyHeistState (I.addTemplate "smallTemplate" aTestTemplate Nothing)

  when hInterp (setInterpreted hs)



  _lens <- getLens
  addConfig hs $
    mempty { hcInterpretedSplices = do
                "appsplice" ## textSplice "contents of the app splice"
                  "appconfig" ## shConfigSplice _lens
           }
    
  addRoutes [ ("/hello",           writeText "hello world")
            , ("/routeWithSplice", routeWithSplice)
            , ("/routeWithConfig", routeWithConfig)
            , ("/public",          serveDirectory "public")
            , ("/sessionDemo",     sessionDemo)
            , ("/sessionTest",     sessionTest)
            ]

  wrapSite (<|> heistServe)
  return $ App hs (over snapletValue fooMod fs) bs sm ns


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"


------------------------------------------------------------------------------
fooMod :: FooSnaplet -> FooSnaplet
fooMod f = f { fooField = fooField f ++ "z" }


------------------------------------------------------------------------------
aTestTemplate :: Template
aTestTemplate =  [XML.TextNode "littleTemplateNode"]

