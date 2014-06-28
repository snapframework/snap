{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Common.Types where

------------------------------------------------------------------------------
import Snap.Snaplet                     (Handler, Initializer, Snaplet,
                                         SnapletInit, makeSnaplet,
                                         nestSnaplet, subSnaplet)
import Snap.Snaplet.Auth                (AuthManager, defAuthSettings)
import Snap.Snaplet.Common.FooSnaplet
import Snap.Snaplet.Common.BarSnaplet

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _foo :: Snaplet FooSnaplet
    , _bar :: Snaplet (BarSnaplet App)
    , _session :: Snaplet SessionManager
    , _embedded :: Snaplet EmbeddedSnaplet
    }

$(makeLenses ''App)

instance HasHeist App where
  heistLens = subSnaplet heist
