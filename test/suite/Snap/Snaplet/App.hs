{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Snaplet.App where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.Trans
import Data.Maybe
import Data.Record.Label
import qualified Data.Text as T
import Data.Configurator
import Snap.Types
import Snap.Util.FileServe

import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Templating.Heist

import Snap.Snaplet.FooSnaplet
import Snap.Snaplet.BarSnaplet

data App = App
    { _heist :: Snaplet (Heist App)
    , _foo :: Snaplet FooSnaplet
    , _bar :: Snaplet BarSnaplet
    }

mkLabels [''App]

instance HasHeist App App where heistLens = subSnaplet heist

routeWithSplice :: Handler App App ()
routeWithSplice = do
    str <- withChild foo getFooField
    writeText $ T.pack $ "routeWithSplice: "++str

routeWithConfig :: Handler App App ()
routeWithConfig = do
    cfg <- getSnapletConfig
    val <- liftIO $ lookup cfg "topConfigField"
    writeText $ "routeWithConfig: " `T.append` fromJust val

app :: Initializer App App (Snaplet App)
app = makeSnaplet "app" "Test application" Nothing $ do
    hs <- nestSnaplet "heist" $ heistInit "templates"
    fs <- nestSnaplet "foo" $ fooInit
    bs <- nestSnaplet "" $ nameSnaplet "baz" $ barInit
    addSplices
        [("appsplice", liftHeist $ textSplice "contents of the app splice")]
    addRoutes [ ("/hello", writeText "hello world")
              , ("/routeWithSplice", routeWithSplice)
              , ("/routeWithConfig", routeWithConfig)
              , ("/public", serveDirectory "public")
              , ("/admin/reload", reloadSite)
              ]
    wrapHandlers (<|> heistServe)
    return $ App hs fs bs


