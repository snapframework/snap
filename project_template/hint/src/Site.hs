{-# LANGUAGE OverloadedStrings #-}
module Site where

------------------------------------------------------------------------------
import           AppState

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans

import qualified Data.ByteString.Char8 as S
import           Data.Maybe
import           Data.Time.Clock

import           Snap.Heist

import           Snap.Util.FileServe
import           Snap.Types

import           Text.Templating.Heist


------------------------------------------------------------------------------
-- Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
frontPage :: StateSnap ()
frontPage = ifTop $ do
    time <- liftIO getCurrentTime
    (ts, lt) <- asks (templateState &&& loadTime)

    let [loadS, renderS] = map (S.pack . show) [lt, time]
        ts' = bindStrings [ ("loadTime", loadS)
                          , ("renderTime", renderS)
                          ] ts
    renderHtml ts' "index"


------------------------------------------------------------------------------
-- Renders the echo page
echo :: StateSnap ()
echo = do
    message <- fromMaybe "" <$> getParam "stuff"
    ts <- asks templateState
    let message' = fromMaybe message $ urlDecode message
        ts' = bindStrings [ ("message", message') ] ts
    renderHtml ts' "echo"


------------------------------------------------------------------------------
-- serves static resources
staticResources :: StateSnap ()
staticResources = fileServe "resources/static"


------------------------------------------------------------------------------
-- contains the routing table.  Be aware that routes are prefix
-- matches.  The 'frontPage' action contains logic to only render if
-- it's served from exactly "/"
routes :: StateSnap ()
routes = route
         [ ("/", frontPage)
         , ("/echo/:stuff", echo)
         ]


------------------------------------------------------------------------------
-- The main entry point handler.  It converts from StateSnap to Snap,
-- and sets the first-pass resolution rules.  In particular, the logic
-- that static resources take priority over routes comes from this
-- function.
site :: AppState -> Snap ()
site = runStateSnap $ staticResources <|> routes
