-- This module contains site-specific configuration information.
module Config where

import Control.Applicative ((<$>))
import Data.Time.Clock
import Snap.Types
import Text.Templating.Heist


-- This contains the site configuration.  Being a boring sample site,
-- this is just a boring sample configuration.  It has the load time
-- (to help illustrate config loading differences between development
-- and production modes) and the TemplateState used for rendering
-- Heist templates.
data Config = Config {
      loadTime :: UTCTime
    , templateState :: TemplateState Snap
    }


-- loads the heist TemplateState, and gets the current time.
getConfig :: IO Config
getConfig = do
    time <- getCurrentTime
    let ets = loadTemplates "resources/templates" emptyTemplateState
    either error (Config time) <$> ets


-- Doesn't actually do anything.  This is a placeholder for tasks like
-- releasing database connections, or cleaning up anything else that
-- might have been included in the config.
cleanupConfig :: Config -> IO ()
cleanupConfig _ = return ()
