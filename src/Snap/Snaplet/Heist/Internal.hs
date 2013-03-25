module Snap.Snaplet.Heist.Internal where

import           Prelude hiding ((.), id)
import           Control.Lens
import           Data.IORef
import           Heist
import           Heist.Splices.Cache

import           Snap.Snaplet


data DefaultMode = Compiled | Interpreted


------------------------------------------------------------------------------
-- | The state for the Heist snaplet.  To use the Heist snaplet in your app
-- include this in your application state and use 'heistInit' to initialize
-- it.  The type parameter b will typically be the base state type for your
-- application.
data Heist b = Configuring
                 { _heistConfig :: IORef (HeistConfig (Handler b b), DefaultMode)
                 }
             | Running
                 { _masterConfig :: HeistConfig (Handler b b)
                 , _heistState   :: HeistState (Handler b b)
                 , _heistCTS     :: CacheTagState
                 , _defMode      :: DefaultMode
                 }

makeLenses ''Heist
