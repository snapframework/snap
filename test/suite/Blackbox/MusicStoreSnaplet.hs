{-# LANGUAGE OverloadedStrings #-}

module Blackbox.MusicStoreSnaplet where


------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.Lens.Lazy
import           Data.Lens.Template
import           Snap.Core

------------------------------------------------------------------------------
import           Heist
import           Heist.Interpreted
import           Snap.Snaplet
import           Snap.Snaplet.Heist

------------------------------------------------------------------------------


data MusicStoreApp = MusicStoreApp
     { _heist :: Snaplet (Heist MusicStoreApp)
     }

makeLenses [''MusicStoreApp]

musicStoreInit :: SnapletInit MusicStoreApp MusicStoreApp
musicStoreInit = makeSnaplet "musicstore" "Best shop in town" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes [ ("/about", writeText "We sell music online") ]
    return $ MusicStoreApp hs
