{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blackbox.Types where


import Control.Lens

import Snap.Snaplet
import Snap.Snaplet.Heist

import Blackbox.FooSnaplet
import Blackbox.BarSnaplet
import Blackbox.EmbeddedSnaplet
import Snap.Snaplet.Session


data App = App
    { _heist :: Snaplet (Heist App)
    , _foo :: Snaplet FooSnaplet
    , _bar :: Snaplet (BarSnaplet App)
    , _session :: Snaplet SessionManager
    , _embedded :: Snaplet EmbeddedSnaplet
    }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

