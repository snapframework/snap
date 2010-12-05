{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application
  ( Application
  , applicationRunner
  ) where

import           Snap.Extension
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Text.Templating.Heist


------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.
data ApplicationState = ApplicationState
    { templateState :: HeistState Application
    , timerState    :: TimerState
    }


------------------------------------------------------------------------------
instance HasHeistState ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }


------------------------------------------------------------------------------
instance HasTimerState ApplicationState where
    getTimerState     = timerState
    setTimerState s a = a { timerState = s }


------------------------------------------------------------------------------
-- | The 'Runner' for ApplicationState. For more on 'Runner's, see the README
-- from the snap-extensions package. Briefly, this is used to generate the
-- 'ApplicationState' needed for our application and will automatically
-- generate reload\/cleanup actions for us which we don't need to worry about.
applicationRunner :: Runner ApplicationState
applicationRunner = do
    heist <- heistRunner "resources/templates" emptyTemplateState
    timer <- timerRunner
    return $ ApplicationState heist timer
