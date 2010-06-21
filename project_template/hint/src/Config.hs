module Config where

import Control.Applicative ((<$>))
import Snap.Types
import Text.Templating.Heist

data Config = Config {
      templateState :: TemplateState Snap
    }


getConfig :: IO Config
getConfig = do
    let ets = loadTemplates "resources/templates" emptyTemplateState
    either error Config <$> ets
