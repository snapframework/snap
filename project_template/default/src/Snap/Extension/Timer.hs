{-|

'Snap.Extension.Timer' exports the 'MonadTimer' interface which allows you to
keep track of the time at which your application was started. The interface's
only operation is 'startTime'.

Two splices, 'startTimeSplice' and 'currentTimeSplice' are also provided, for
your convenience.

'Snap.Extension.Timer.Timer' contains the only implementation of this
interface and can be used to turn your application's monad into a
'MonadTimer'.

More than anything else, this is intended to serve as an example Snap
Extension to any developer wishing to write their own Snap Extension.

-}

module Snap.Extension.Timer
  ( MonadTimer(..)
  , startTimeSplice
  , currentTimeSplice
  ) where

import           Control.Monad.Trans
import           Data.Time.Clock
import qualified Data.Text as T
import           Snap.Types
import           Text.Templating.Heist
import           Text.XmlHtml


------------------------------------------------------------------------------
-- | The 'MonadTimer' type class. Minimal complete definition: 'startTime'.
class MonadSnap m => MonadTimer m where
    -- | The time at which your application was last loaded.
    startTime :: m UTCTime


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: MonadTimer m => Splice m
startTimeSplice = do
    time <- lift startTime
    return $ [TextNode $ T.pack $ show $ time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: MonadTimer m => Splice m
currentTimeSplice = do
    time <- lift $ liftIO getCurrentTime
    return $ [TextNode $ T.pack $ show $ time]
