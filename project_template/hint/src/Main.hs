{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Config (getConfig)
import Site (site)
import Server (quickServer)

#ifdef PRODUCTION
import Snap.Loader.Static (loadSnapTH)
#else
import Snap.Loader.Hint (loadSnapTH)
#endif

main :: IO ()
main = do
  snap <- $(loadSnapTH 'getConfig 'site)
  quickServer snap
