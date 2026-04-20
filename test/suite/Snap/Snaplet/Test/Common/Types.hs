{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Test.Common.Types where

------------------------------------------------------------------------------
import Control.Lens
------------------------------------------------------------------------------
import Snap.Snaplet                             (Snaplet, subSnaplet)
import Snap.Snaplet.Auth                        (AuthManager)
import Snap.Snaplet.Test.Common.BarSnaplet
import Snap.Snaplet.Test.Common.EmbeddedSnaplet
import Snap.Snaplet.Test.Common.FooSnaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist    :: Snaplet (Heist App)
    , _foo      :: Snaplet FooSnaplet
    , _auth     :: Snaplet (AuthManager App)
    , _bar      :: Snaplet (BarSnaplet App)
    , _session  :: Snaplet (SessionManager App)
    , _embedded :: Snaplet EmbeddedSnaplet
    }

$(makeLenses ''App)

instance HasHeist App where
  heistLens = subSnaplet heist
