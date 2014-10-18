{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Snap.Snaplet.Test.Common.FooSnaplet where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import           Data.Configurator
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import           Prelude             hiding (lookup)
------------------------------------------------------------------------------
import           Data.Map.Syntax     (( ## ))
import           Heist
import           Heist.Interpreted
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.TestCommon     (handlerConfig, shConfigSplice)

------------------------------------------------------------------------------
data FooSnaplet = FooSnaplet { fooField :: String }

fooInit :: HasHeist b => Snaplet (Heist b) -> SnapletInit b FooSnaplet
fooInit h = makeSnaplet "foosnaplet" "A demonstration snaplet called foo."
    (Just $ return "foosnaplet") $ do
    config <- getSnapletUserConfig
    addTemplates h ""
    rootUrl <- getSnapletRootURL
    fp <- getSnapletFilePath
    name <- getSnapletName
    _lens <- getLens
    let splices = do
            "foosplice" ## textSplice "contents of the foo splice"
            "fooconfig" ## shConfigSplice _lens
    addConfig h $ mempty & scInterpretedSplices .~ splices
    addRoutes [("fooConfig", liftIO (lookup config "fooSnapletField") >>= writeLBS . fromJust)
              ,("fooRootUrl", writeBS rootUrl)
              ,("fooSnapletName", writeText $ fromMaybe "empty snaplet name" name)
              ,("fooFilePath", writeText $ T.pack fp)
              ,("handlerConfig", handlerConfig)
              ]
    return $ FooSnaplet "foo snaplet data string"

getFooField :: Handler b FooSnaplet String
getFooField = gets fooField

