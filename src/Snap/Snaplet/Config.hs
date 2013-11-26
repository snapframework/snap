{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Snap.Snaplet.Config where

import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Snap.Core
import Snap.Http.Server.Config
import System.Console.GetOpt

------------------------------------------------------------------------------
-- | AppConfig contains the config options for command line arguments in
-- snaplet-based apps.
#if MIN_VERSION_base(4,7,0)
newtype AppConfig = AppConfig { appEnvironment :: Maybe String }
#else
newtype AppConfig = AppConfig { appEnvironment :: Maybe String } deriving (Typeable)
#endif



------------------------------------------------------------------------------
-- | AppConfig has a manual instance of Typeable due to limitations in the
-- tools available before GHC 7.4, and the need to make dynamic loading
-- tractable.  When support for earlier versions of GHC is dropped, the
-- dynamic loader package can be updated so that manual Typeable instances
-- are no longer needed.
appConfigTyCon :: TyCon
#if MIN_VERSION_base(4,7,0)
appConfigTyCon = mkTyCon3 "snap" "Snap.Snaplet.Config" "AppConfig"
#else
appConfigTyCon = mkTyCon "Snap.Snaplet.Config.AppConfig"
#endif
{-# NOINLINE appConfigTyCon #-}

#if !MIN_VERSION_base(4,7,0)
instance Typeable AppConfig where
    typeOf _ = mkTyConApp appConfigTyCon []
#endif


------------------------------------------------------------------------------
instance Monoid AppConfig where
    mempty = AppConfig Nothing
    mappend a b = AppConfig
        { appEnvironment = ov appEnvironment a b
        }
      where
        ov f x y = getLast $! (mappend `on` (Last . f)) x y


------------------------------------------------------------------------------
-- | Command line options for snaplet applications.
appOpts :: AppConfig -> [OptDescr (Maybe (Config m AppConfig))]
appOpts defaults = map (fmapOpt $ fmap (`setOther` mempty))
    [ Option "e" ["environment"]
             (ReqArg setter "ENVIRONMENT")
             $ "runtime environment to use" ++ defaultC appEnvironment
    ]
  where
    setter s = Just $ mempty { appEnvironment = Just s}
    defaultC f = maybe "" ((", default " ++) . show) $ f defaults


------------------------------------------------------------------------------
-- | Calls snap-server's extendedCommandLineConfig to add snaplet options to
-- the built-in server command line options.
commandLineAppConfig :: MonadSnap m
                     => Config m AppConfig
                     -> IO (Config m AppConfig)
commandLineAppConfig defaults =
    extendedCommandLineConfig (appOpts appDefaults ++ optDescrs defaults)
                              mappend defaults
  where
    appDefaults = fromMaybe mempty $ getOther defaults

