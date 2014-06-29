{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Snap.Snaplet.Common.App (
  App,
  appInit,
  appInit',
  auth,
  heist,
  session,
  embedded,
  foo,
  bar
  )where

------------------------------------------------------------------------------
import Control.Lens                                  (makeLenses, over)
import Control.Monad                                 (when)
import Control.Monad.IO.Class                        (liftIO)
import Control.Monad.Trans                           (lift)
import Data.Monoid                                   (mempty)
import System.FilePath                               ((</>))
------------------------------------------------------------------------------
import Data.Map.Syntax                               ((##),(#!))
import Text.XmlHtml                                  (Node(TextNode))
import Heist                                         (Splices, Template,
                                                      hcCompiledSplices,
                                                      hcInterpretedSplices)
import Heist.Compiled                                (Splice, withSplices,
                                                      runChildren)
import Snap                                          ((<|>))
import Snap.Core                                     (pass, writeText)
import Snap.Http.Server.Config                       (Config, completeConfig,
                                                      defaultConfig)
import Snap.Snaplet                                  (Handler, Initializer,
                                                      Snaplet, SnapletInit,
                                                      addRoutes,
                                                      embedSnaplet, getLens,
                                                      getSnapletFilePath,
                                                      makeSnaplet,
                                                      nameSnaplet,
                                                      nestSnaplet,
                                                      snapletValue,
                                                      subSnaplet,
                                                      with,
                                                      wrapSite)
import Snap.Snaplet.Auth                             (AuthManager,
                                                      AuthSettings,
                                                      addAuthSplices,
                                                      authSettingsFromConfig,
                                                      currentUser,
                                                      defAuthSettings,
                                                      userCSplices)
import Snap.Snaplet.Auth.Backends.JsonFile           (initJsonFileAuthManager)
import Snap.Snaplet.Common.BarSnaplet
import Snap.Snaplet.Common.EmbeddedSnaplet
import Snap.Snaplet.Common.FooSnaplet
import Snap.Snaplet.Common.Handlers
import Snap.Snaplet.Common.Types
import Snap.Snaplet.Config                           (AppConfig,
                                                      commandLineAppConfig)
import Snap.Snaplet.Heist                            (Heist, HasHeist,
                                                      addConfig,
                                                      addTemplates,
                                                      addTemplatesAt,
                                                      heistInit', heistLens,
                                                      heistServe,
                                                      modifyHeistState)
import Heist.Interpreted                             (addTemplate, textSplice)
import Snap.Snaplet.HeistNoClass                     (setInterpreted)
import Snap.Snaplet.Session                          (SessionManager)
import Snap.Snaplet.Session.Backends.CookieSession   (initCookieSessionManager)
import Snap.TestCommon                               (shConfigSplice)
import Snap.Util.FileServe                           (serveDirectory)

------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = appInit' False False


------------------------------------------------------------------------------
appInit' :: Bool -> Bool -> SnapletInit App App
appInit' hInterp authConfigFile =
  makeSnaplet "app" "Test application" Nothing $ do

  ------------------------------
  -- Initial subSnaplet setup --
  ------------------------------
  
  hs <- nestSnaplet "heist"   heist   $
        heistInit'
        "templates"
        (mempty {hcCompiledSplices = compiledSplices})

  sm <- nestSnaplet "session" session $
        initCookieSessionManager "sitekey.txt" "_session" (Just (30 * 60))
  au <- nestSnaplet "auth" auth authInit
  fs <- nestSnaplet "foo"     foo     $ fooInit hs
  bs <- nestSnaplet ""        bar     $ nameSnaplet "baz" $ barInit hs foo
  ns <- embedSnaplet "embed" embedded embeddedInit

  --------------------------------
  -- Exercise the Heist snaplet --
  --------------------------------
  
  addAuthSplices hs auth -- TODO/NOTE: probably not necessary (?)
  addTemplates hs "extraTemplates"

  sPath    <- getSnapletFilePath

  let extraTemplatesPath = sPath </> "test" </> "evenMoreTemplates"   -- TODO/NODE is this right?
  addTemplatesAt hs "evenMoreTemplates" extraTemplatesPath  -- TODO/NODE is this right?
  
  when hInterp $ do
    modifyHeistState (addTemplate "smallTemplate" aTestTemplate Nothing)
    setInterpreted hs

  ---------------------------
  -- Exercise Auth snaplet --
  ---------------------------

  authSettings <- if authConfigFile
                    then authSettingsFromConfig
                    else return defAuthSettings

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
  return $ App hs (over snapletValue fooMod fs) au bs sm ns


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings session "users.json"


{-
------------------------------------------------------------------------------
-- Alternative authInit for tunable settings
authInit' :: AuthSettings -> SnapletInit App (AuthManager App)
authInit' settings = initJsonFileAuthManager settings session "users.json"
-}

------------------------------------------------------------------------------
compiledSplices :: Splices (Splice (Handler App App))
compiledSplices = do
  "userSplice" #! withSplices runChildren userCSplices $
    lift $ maybe pass return =<< with auth currentUser

------------------------------------------------------------------------------
fooMod :: FooSnaplet -> FooSnaplet
fooMod f = f { fooField = fooField f ++ "z" }


------------------------------------------------------------------------------
aTestTemplate :: Template
aTestTemplate =  [TextNode "littleTemplateNode"]

