{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Blackbox.FooSnaplet where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Configurator
import Data.Maybe
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Core
import Text.Templating.Heist

import Blackbox.Common

data FooSnaplet = FooSnaplet { fooField :: String }

fooInit :: HasHeist b => SnapletInit b FooSnaplet
fooInit = makeSnaplet "foosnaplet" "A demonstration snaplet called foo."
    (Just $ return "../foosnaplet") $ do
    config <- getSnapletUserConfig
    addTemplates "foo"
    addSplices
        [("foosplice", liftHeist $ textSplice "contents of the foo splice")]
    rootUrl <- getSnapletRootURL
    fp <- getSnapletFilePath
    name <- getSnapletName
    addSplices [("fooconfig", shConfigSplice)]
    addRoutes [("fooConfig", liftIO (lookup config "fooSnapletField") >>= writeLBS . fromJust)
              ,("fooRootUrl", writeBS rootUrl)
              ,("fooSnapletName", writeText $ fromMaybe "empty snaplet name" name)
              ,("fooFilePath", writeText $ T.pack fp)
              ,("handlerConfig", handlerConfig)
              ]
    return $ FooSnaplet "foo snaplet data string"

getFooField :: Handler b FooSnaplet String
getFooField = getsSnapletState fooField

