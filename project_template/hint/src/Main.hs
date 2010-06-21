{-# LANGUAGE OverloadedStrings, CPP, TemplateHaskell #-}
module Main where

import Config (getConfig)
import Site (site)

import Snap.Http.Server (httpServe)

#ifdef PRODUCTION
import Snap.Loader.Static (loadSnapTH)
#else
import Snap.Loader.Hint (loadSnapTH)
#endif

import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let port = case args of
               []  -> 8000
               p:_ -> read p
      aLog = Just "log/access.log"
      eLog = Just "log/error.log"

  snap <- $(loadSnapTH 'getConfig 'site)

  httpServe "*" port "localhost" aLog eLog snap
