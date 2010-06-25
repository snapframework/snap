{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Config (getConfig, cleanupConfig)
import Site (site)
import Server (quickServer)

#ifdef PRODUCTION
import Snap.Loader.Static (loadSnapTH)
#else
import Snap.Loader.Hint (loadSnapTH)
#endif

main :: IO ()
main = do
    (cleanup, snap) <- $(loadSnapTH 'getConfig 'cleanupConfig 'site)
    quickServer snap
    cleanup
