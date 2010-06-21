{-# LANGUAGE OverloadedStrings #-}
module Site where

import           Config

import           Control.Monad (msum)
import           Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as S
import           Data.Time.Clock

import           Snap.Util.FileServe (fileServe)
import           Snap.Types

import           Text.Templating.Heist


frontPage :: Config -> Snap ()
frontPage config = ifTop $ do
    modifyResponse $ setContentType "text/html; charset=utf-8"

    time <- liftIO getCurrentTime

    let [loadS, renderS] = map (S.pack . show) [loadTime config, time]
        ts = templateState config
        ts' = bindStrings [ ("loadTime", loadS)
                          , ("renderTime", renderS)
                          ] ts

    Just rendered <- renderTemplate ts' "index"
    writeBS rendered
    let len = fromIntegral . S.length $ rendered
    modifyResponse . setContentLength $ len


staticResources :: Snap ()
staticResources = fileServe "resources/static"


site :: Config -> Snap ()
site config = msum [ frontPage config
                   , staticResources
                   ]
