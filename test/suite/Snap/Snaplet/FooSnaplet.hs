{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Snap.Snaplet.FooSnaplet where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Configurator
import Data.Maybe
import Data.Record.Label
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Types
import Text.Templating.Heist

data FooSnaplet = FooSnaplet { fooField :: String }

fooInit :: HasHeist b => SnapletInit b FooSnaplet
fooInit = makeSnaplet "foosnaplet" "A demonstration snaplet called foo." Nothing $ do
    config <- getSnapletConfig
    addTemplates "foo"
    addSplices
        [("foosplice", liftHeist $ textSplice "contents of the foo splice")]
    rootUrl <- getSnapletRootURL
    fp <- getSnapletFilePath
    name <- getSnapletName
    addRoutes [("fooConfig", liftIO (lookup config "fooSnapletField") >>= writeLBS . fromJust)
              ,("fooRootUrl", writeBS rootUrl)
              ,("fooSnapletName", writeText $ fromMaybe "empty snaplet name" name)
              ,("fooFilePath", writeText $ T.pack fp)
              ]
    return $ FooSnaplet "foo snaplet data string"

getFooField :: Handler b FooSnaplet String
getFooField = gets fooField

