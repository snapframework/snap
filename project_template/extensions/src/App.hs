{-

This module defines our application's monad and any application-specific
information it requires.

-}

module App
  ( App
  , appRunner
  ) where

import           Snap.Extension
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Text.Templating.Heist


------------------------------------------------------------------------------
-- | 'App' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type App = SnapExtend AppState


------------------------------------------------------------------------------
-- | 'AppState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.
data AppState = AppState
    { heistState :: HeistState App
    , timerState :: TimerState
    }


------------------------------------------------------------------------------
instance HasHeistState AppState where
    getHeistState = heistState
    setHeistState s a = a { heistState = s }


------------------------------------------------------------------------------
instance HasTimerState AppState where
    getTimerState = timerState
    setTimerState s a = a { timerState = s }


------------------------------------------------------------------------------
-- | The 'Runner' for AppState. For more on 'Runner's, see the README from
-- the snap-extensions package. Briefly, this is used to generate the
-- 'AppState' needed for our application and will automatically generate
-- reload\/cleanup actions for us which we don't need to worry about.
appRunner :: Runner AppState
appRunner = do
    heist <- heistRunner "resources/templates" emptyTemplateState
    timer <- timerRunner
    return $ AppState heist timer
