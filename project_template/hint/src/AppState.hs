{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This module contains site-specific state information.
module AppState (
    AppState(..)
  , loadAppState
  , cleanupAppState
  , StateSnap
  , runStateSnap
  , ask  -- these functions are re-exported
  , asks -- from Control.Monad.Reader
)
where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Reader

import Data.Time.Clock
import Snap.Types
import Text.Templating.Heist


-- This contains the site configuration.  Being a boring sample site,
-- this is just a boring sample configuration.  It has the load time
-- (to help illustrate config loading differences between development
-- and production modes) and the TemplateState used for rendering
-- Heist templates.
data AppState = AppState {
      loadTime :: UTCTime
    , templateState :: TemplateState StateSnap
    }


-- An example of creating an application-specific MonadSnap instance.
-- This instance uses the GeneralizedNewtypeDeriving extension to
-- derive all the necessary instances from the underlying
-- representation.
newtype StateSnap a = AS (ReaderT AppState Snap a)
    deriving ( Monad
             , MonadReader AppState
             , MonadSnap
             , MonadPlus
             , MonadCatchIO
             , MonadIO
             , Applicative
             , Alternative
             , Functor
             )


-- Convert the application-specific MonadSnap instance into a Snap
-- type.
runStateSnap :: StateSnap a -> AppState -> Snap a
runStateSnap (AS rt) st = runReaderT rt st


-- loads the heist TemplateState, and gets the current time.
loadAppState :: IO AppState
loadAppState = do
    time <- getCurrentTime
    let ets = loadTemplates "resources/templates" emptyTemplateState
    either error (AppState time) <$> ets


-- Doesn't actually do anything.  This is a placeholder for tasks like
-- releasing database connections, or cleaning up anything else that
-- might have been included in the config.
cleanupAppState :: AppState -> IO ()
cleanupAppState _ = return ()
