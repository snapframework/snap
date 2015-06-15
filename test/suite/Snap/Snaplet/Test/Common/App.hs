{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Test.Common.App (
  App,
  appInit,
  appInit',
  auth,
  failingAppInit,
  heist,
  session,
  embedded,
  foo,
  bar
  )where

------------------------------------------------------------------------------
import           Control.Lens                                (over)
import           Control.Monad                               (when)
import           Control.Monad.Trans                         (lift)
import           Data.Monoid                                 (mempty)
------------------------------------------------------------------------------
import           Control.Applicative                         ((<|>))
import           Data.Map.Syntax                             (( #! ), ( ## ))
import           Heist                                       (Splices, Template)
import           Heist.Compiled                              (Splice, runChildren, withSplices)
import           Heist.Internal.Types                        (HeistConfig (..), SpliceConfig (..))
import           Heist.Interpreted                           (addTemplate, textSplice)
import           Snap.Core                                   (pass, writeText)
import           Snap.Snaplet                                (Handler, SnapletInit, addRoutes, embedSnaplet, getLens, getSnapletFilePath, makeSnaplet, nameSnaplet, nestSnaplet, snapletValue, with, wrapSite)
import           Snap.Snaplet.Auth                           (AuthManager, AuthSettings, addAuthSplices, authSettingsFromConfig, currentUser, defAuthSettings, userCSplices)
import           Snap.Snaplet.Auth.Backends.JsonFile         (initJsonFileAuthManager)
import           Snap.Snaplet.Heist                          (addConfig, addTemplates, heistInit', heistServe, modifyHeistState)
import           Snap.Snaplet.HeistNoClass                   (setInterpreted)
import           Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import           Snap.Snaplet.Test.Common.BarSnaplet
import           Snap.Snaplet.Test.Common.EmbeddedSnaplet
import           Snap.Snaplet.Test.Common.FooSnaplet
import           Snap.Snaplet.Test.Common.Handlers
import           Snap.Snaplet.Test.Common.Types
import           Snap.TestCommon                             (shConfigSplice)
import           Snap.Util.FileServe                         (serveDirectory)
import           Text.XmlHtml                                (Node (TextNode))

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
        (HeistConfig (mempty {_scCompiledSplices = compiledSplices}) "" True)

  sm <- nestSnaplet "session" session $
        initCookieSessionManager "sitekey.txt" "_session" Nothing (Just (30 * 60))
  fs <- nestSnaplet "foo"     foo     $ fooInit hs
  bs <- nestSnaplet ""        bar     $ nameSnaplet "baz" $ barInit hs foo
  ns <- embedSnaplet "embed" embedded embeddedInit

  --------------------------------
  -- Exercise the Heist snaplet --
  --------------------------------

  addTemplates hs "extraTemplates"

  when hInterp $ do
    modifyHeistState (addTemplate "smallTemplate" aTestTemplate Nothing)
    setInterpreted hs

  _lens <- getLens
  addConfig hs $
    mempty { _scInterpretedSplices = do
                "appsplice" ## textSplice "contents of the app splice"
                "appconfig" ## shConfigSplice _lens
           }

  ---------------------------
  -- Exercise Auth snaplet --
  ---------------------------

  authSettings <- if authConfigFile
                    then authSettingsFromConfig
                    else return defAuthSettings

  au <- nestSnaplet "auth" auth $ authInit authSettings

  addAuthSplices hs auth -- TODO/NOTE: probably not necessary (?)


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
-- Alternative authInit for tunable settings
authInit :: AuthSettings -> SnapletInit App (AuthManager App)
authInit settings = initJsonFileAuthManager settings session "users.json"


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


------------------------------------------------------------------------------
failingAppInit :: SnapletInit App App
failingAppInit = makeSnaplet "app" "Test application" Nothing $ do
   _ <- error "Error"
   return undefined
