{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Timer.Impl' is an implementation of the 'MonadTimer'
interface defined in 'Snap.Extension.Timer'.

As always, to use, add 'TimerState' to your application's state, along with an
instance of 'HasTimerState' for your application's state, making sure to use a
'timerInitializer' in your application's 'Initializer', and then you're ready to go.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Snap.Extension.Timer.Impl
  ( TimerState
  , HasTimerState(..)
  , timerInitializer
  , module Snap.Extension.Timer
  ) where

import           Control.Monad.Reader
import           Data.Time.Clock
import           Snap.Extension
import           Snap.Extension.Timer
import           Snap.Types

------------------------------------------------------------------------------
-- | Your application's state must include a 'TimerState' in order for your
-- application to be a 'MonadTimer'.
newtype TimerState = TimerState
    { _startTime :: UTCTime
    }


------------------------------------------------------------------------------
-- | For your application's monad to be a 'MonadTimer', your application's
-- state needs to be an instance of 'HasTimerState'. Minimal complete
-- definition: 'getTimerState', 'setTimerState'.
class HasTimerState s where
    getTimerState :: s -> TimerState
    setTimerState :: TimerState -> s -> s


------------------------------------------------------------------------------
-- | The Initializer for 'TimerState'. No arguments are required.
timerInitializer :: Initializer TimerState
timerInitializer = liftIO getCurrentTime >>= mkInitializer . TimerState


------------------------------------------------------------------------------
instance InitializerState TimerState where
    extensionId = const "Timer/Timer"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()


------------------------------------------------------------------------------
instance HasTimerState s => MonadTimer (SnapExtend s) where
    startTime = fmap _startTime $ asks getTimerState


------------------------------------------------------------------------------
instance (MonadSnap m, HasTimerState s) => MonadTimer (ReaderT s m) where
    startTime = fmap _startTime $ asks getTimerState
