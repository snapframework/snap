{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Config where

import Data.Function
import Data.Maybe
import Data.Monoid
import Snap.Core
import Snap.Http.Server.Config
import System.Console.GetOpt

------------------------------------------------------------------------------
-- | AppConfig contains the config options for command line arguments in
-- snaplet-based apps.
newtype AppConfig = AppConfig { appEnvironment :: Maybe String }


------------------------------------------------------------------------------
instance Monoid AppConfig where
    mempty = AppConfig Nothing
    mappend a b = AppConfig
        { appEnvironment = ov appEnvironment a b
        }
      where
        ov f x y = getLast $! (mappend `on` (Last . f)) x y


appOpts :: AppConfig -> [OptDescr (Maybe (Config m AppConfig))]
appOpts defaults = map (fmapOpt $ fmap (flip setOther mempty))
    [ Option ['e'] ["environment"]
             (ReqArg (\s -> Just $ mempty { appEnvironment = Just s}) "ENVIRONMENT")
             $ "runtime environment to use" ++ defaultC appEnvironment
    ]
  where
    defaultC f = maybe "" ((", default " ++) . show) $ f defaults


commandLineAppConfig :: MonadSnap m
                     => Config m AppConfig
                     -> IO (Config m AppConfig)
commandLineAppConfig defaults =
    extendedCommandLineConfig (appOpts appDefaults ++ optDescrs defaults)
                              combine defaults
  where
    appDefaults = fromMaybe (AppConfig Nothing) $ getOther defaults
    combine :: AppConfig -> AppConfig -> AppConfig
    combine a b = AppConfig $ (mappend `on` appEnvironment) a b

