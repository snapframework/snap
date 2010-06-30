{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Data.Monoid (mappend, mempty)

import Config (getConfig, cleanupConfig)
import Site (site)

import Snap.Http.Server
import Snap.Http.Server.Config

#ifdef PRODUCTION
import Snap.Loader.Static (loadSnapTH)
#else
import Snap.Loader.Hint (loadSnapTH)
#endif

main :: IO ()
main = do
    (cleanup, snap) <- $(loadSnapTH 'getConfig 'cleanupConfig 'site)

    let defaultFlags = mempty { flagVerbose = True
                              , flagAccessLog = Just "log/access.log"
                              , flagErrorLog = Just "log/error.log"
                              }

    cmdLineFlags <- readFlagsFromCmdLineArgs
    let conf = flagsToConfig $ defaultFlags `mappend` cmdLineFlags

    httpServeConfig conf snap
    cleanup
