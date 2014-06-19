{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Blackbox.FooSnaplet where

------------------------------------------------------------------------------
import Prelude hiding (lookup)
import Control.Monad.State
import Data.Configurator
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Core
import Heist
import Heist.Interpreted

import Blackbox.Common
import Data.Map.Syntax ((##))


------------------------------------------------------------------------------
data FooSnaplet = FooSnaplet { fooField :: String }

fooInit :: HasHeist b => Snaplet (Heist b) -> SnapletInit b FooSnaplet
fooInit h = makeSnaplet "foosnaplet" "A demonstration snaplet called foo."
    (Just $ return "../foosnaplet") $ do
    config <- getSnapletUserConfig
    addTemplates h ""
    rootUrl <- getSnapletRootURL
    fp <- getSnapletFilePath
    name <- getSnapletName
    _lens <- getLens
    addConfig h $ mempty
        { hcInterpretedSplices = do
            "foosplice" ## textSplice "contents of the foo splice"
            "fooconfig" ## shConfigSplice _lens
        }
    addRoutes [("fooConfig", liftIO (lookup config "fooSnapletField") >>= writeLBS . fromJust)
              ,("fooRootUrl", writeBS rootUrl)
              ,("fooSnapletName", writeText $ fromMaybe "empty snaplet name" name)
              ,("fooFilePath", writeText $ T.pack fp)
              ,("handlerConfig", handlerConfig)
              ]
    return $ FooSnaplet "foo snaplet data string"

getFooField :: Handler b FooSnaplet String
getFooField = gets fooField

