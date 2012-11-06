{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Blackbox.BarSnaplet where

import Prelude hiding (lookup)

import Control.Monad.State
import qualified Data.ByteString as B
import Data.Configurator
import Data.Lens.Lazy
import Data.Lens.Template
import Data.Maybe
import Data.Text (Text)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Core
import Heist.Interpreted

import Blackbox.Common
import Blackbox.FooSnaplet

data BarSnaplet b = BarSnaplet
    { _barField :: String
    , fooLens  :: Lens b (Snaplet FooSnaplet)
    }

makeLens ''BarSnaplet

barsplice :: [(Text, SnapletISplice b)]
barsplice = [("barsplice", textSplice "contents of the bar splice")]

barInit :: HasHeist b
        => Snaplet (Heist b)
        -> Lens b (Snaplet FooSnaplet)
        -> SnapletInit b (BarSnaplet b)
barInit h l = makeSnaplet "barsnaplet" "An example snaplet called bar." Nothing $ do
    config <- getSnapletUserConfig
    addTemplates h ""
    rootUrl <- getSnapletRootURL
    _lens <- getLens
    addRoutes [("barconfig", liftIO (lookup config "barSnapletField") >>= writeLBS . fromJust)
              ,("barrooturl", writeBS $ "url" `B.append` rootUrl)
              ,("bazpage2",   renderWithSplices "bazpage" barsplice)
              ,("bazpage3",   heistServeSingle "bazpage")
              ,("bazpage4",   renderAs "text/html" "bazpage")
              ,("bazpage5",   renderWithSplices "bazpage"
                                [("barsplice", shConfigSplice _lens)])
              ,("bazbadpage", heistServeSingle "cpyga")
              ,("bar/handlerConfig", handlerConfig)
              ]
    return $ BarSnaplet "bar snaplet data string" l

