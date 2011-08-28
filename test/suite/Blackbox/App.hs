{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blackbox.App where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.Trans
import Data.Lens.Lazy
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Configurator
import Snap.Core
import Snap.Util.FileServe

import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Snap.Snaplet.HeistNoClass as HNC
import Text.Templating.Heist

import Blackbox.Common
import Blackbox.BarSnaplet
import Blackbox.FooSnaplet
import Blackbox.Types
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession


routeWithSplice :: Handler App App ()
routeWithSplice = do
    str <- with foo getFooField
    writeText $ T.pack $ "routeWithSplice: "++str

routeWithConfig :: Handler App App ()
routeWithConfig = do
    cfg <- getSnapletUserConfig
    val <- liftIO $ lookup cfg "topConfigField"
    writeText $ "routeWithConfig: " `T.append` fromJust val


sessionDemo :: Handler App App ()
sessionDemo = withSession session $ do
  with session $ do
    curVal <- getFromSession "foo"
    case curVal of
      Nothing -> do
        setInSession "foo" "bar"
      Just _ -> return ()
  list <- with session $ (T.pack . show) `fmap` sessionToList
  csrf <- with session $ (T.pack . show) `fmap` csrfToken
  HNC.renderWithSplices heist "session"
    [ ("session", liftHeist $ textSplice list)
    , ("csrf", liftHeist $ textSplice csrf) ]

sessionTest :: Handler App App ()
sessionTest = withSession session $ do
  q <- getParam "q"
  val <- case q of
    Just x -> do
      let x' = T.decodeUtf8 x
      with session $ setInSession "test" x'
      return x'
    Nothing -> maybe "" id `fmap` with session (getFromSession "test")
  writeText val

fooMod :: FooSnaplet -> FooSnaplet
fooMod f = f { fooField = fooField f ++ "z" }

app :: SnapletInit App App
app = makeSnaplet "app" "Test application" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    fs <- nestSnaplet "foo" foo fooInit
    bs <- nestSnaplet "" bar $ nameSnaplet "baz" $ barInit foo
    sm <- nestSnaplet "session" session $ 
          initCookieSessionManager "sitekey.txt" "_session" (Just (30 * 60))
    addSplices
        [("appsplice", liftHeist $ textSplice "contents of the app splice")]
    HNC.addSplices heist
        [("appconfig", shConfigSplice)]
    addRoutes [ ("/hello", writeText "hello world")
              , ("/routeWithSplice", routeWithSplice)
              , ("/routeWithConfig", routeWithConfig)
              , ("/public", serveDirectory "public")
              , ("/sessionDemo", sessionDemo)
              , ("/sessionTest", sessionTest)
              , ("/admin/reload", reloadSite)
              ]
    wrapHandlers (<|> heistServe)
    return $ App hs (modL snapletValue fooMod fs) bs sm



