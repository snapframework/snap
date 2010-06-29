{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Data.Monoid (mappend, mconcat)

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

    let defaultFlags = mconcat [ flagV -- verbose
                               , flagAL "log/access.log"
                               , flagEL "log/error.log"
                               ]

    cmdLineFlags <- readFlagsFromCmdLineArgs
    let conf = flagsToConfig $ defaultFlags `mappend` cmdLineFlags

    httpServeConfig conf snap
    cleanup
