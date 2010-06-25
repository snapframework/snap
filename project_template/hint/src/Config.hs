module Config where

import Control.Applicative ((<$>))
import Data.Time.Clock
import Snap.Types
import Text.Templating.Heist

data Config = Config {
      loadTime :: UTCTime
    , templateState :: TemplateState Snap
    }

getConfig :: IO Config
getConfig = do
    time <- getCurrentTime
    let ets = loadTemplates "resources/templates" emptyTemplateState
    either error (Config time) <$> ets


cleanupConfig :: Config -> IO ()
cleanupConfig _ = return ()
