{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Snap.Snaplet.BarSnaplet where

import Prelude hiding (lookup)

import Control.Monad.State
import qualified Data.ByteString as B
import Data.Maybe
import Data.Configurator
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Types
import Text.Templating.Heist

data BarSnaplet = BarSnaplet { barField :: String }

barInit :: HasHeist b
        => SnapletInit b BarSnaplet
barInit = makeSnaplet "barsnaplet" "An example snaplet called bar." Nothing $ do
    config <- getSnapletConfig
    addTemplates ""
    let barsplice = [("barsplice", liftHeist $ textSplice "contents of the bar splice")]
    rootUrl <- getSnapletRootURL
    addRoutes [("barconfig", liftIO (lookup config "barSnapletField") >>= writeLBS . fromJust)
              ,("barrooturl", writeBS $ "url" `B.append` rootUrl)
              ,("bazpage2",   renderWithSplices "bazpage" barsplice)
              ,("bazpage3",   heistServeSingle "bazpage")
              ,("bazpage4",   renderAs "text/html" "bazpage")
              ,("bazbadpage", heistServeSingle "cpyga")
              ]
    return $ BarSnaplet "bar snaplet data string"

getBarField :: Handler b BarSnaplet String
getBarField = gets barField

