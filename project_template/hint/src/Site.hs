{-# LANGUAGE OverloadedStrings #-}
module Site where

import           AppState

import           Control.Arrow ((&&&))
import           Control.Monad (msum)
import           Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as S
import           Data.Time.Clock

import           Snap.Heist

import           Snap.Util.FileServe (fileServe)
import           Snap.Types

import           Text.Templating.Heist


frontPage :: StateSnap ()
frontPage = ifTop $ do
    time <- liftIO getCurrentTime
    (ts, lt) <- asks (templateState &&& loadTime)

    let [loadS, renderS] = map (S.pack . show) [lt, time]
        ts' = bindStrings [ ("loadTime", loadS)
                          , ("renderTime", renderS)
                          ] ts
    renderHtml ts' "index"


staticResources :: StateSnap ()
staticResources = fileServe "resources/static"


site :: AppState -> Snap ()
site = runStateSnap $ do
    msum [ frontPage
         , staticResources
         ]
