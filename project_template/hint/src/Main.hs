{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Data.Monoid        (mappend, mempty)

import Config             (getConfig, cleanupConfig)
import Site               (site)

import Snap.Http.Server
import Snap.Http.Server.Config

#ifdef PRODUCTION
import Snap.Loader.Static (loadSnapTH)
#else
import Snap.Loader.Hint   (loadSnapTH)
#endif

-- This is the entry point for this web server application.  It
-- supports easily switching between interpreting source and running
-- statically compiled code.
main :: IO ()
main = do
    -- override the some of the defaults from Snap.Http.Server.Config
    let defaultFlags = mempty { flagVerbose = True
                              , flagAccessLog = Just "log/access.log"
                              , flagErrorLog = Just "log/error.log"
                              }

    -- read command line args, and merge them with the defaults above
    cmdLineFlags <- readFlagsFromCmdLineArgs
    let conf = flagsToConfig $ defaultFlags `mappend` cmdLineFlags

    -- This is just about the same as calling a function:
    --
    -- loadSnap :: IO a
    --          -> (a -> IO ())
    --          -> (a -> Snap ())
    --          -> IO (IO (), Snap ())
    --
    -- The important parts are that it gives you back a cleanup action
    -- and a Snap handler.  The specific behavior of each depends on
    -- whether the Hint loader or the Static loader is imported.  This
    -- interface abstracts across the differences between them.
    (cleanup, snap) <- $(loadSnapTH 'getConfig 'cleanupConfig 'site)

    -- Run the server
    httpServeConfig conf snap

    -- Run the cleanup action before exiting
    cleanup
