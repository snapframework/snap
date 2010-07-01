{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Data.Monoid        (mappend, mempty)

import AppState           (cleanupAppState, loadAppState)
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
--
-- In either mode, the generated program should be run from the root
-- of the project tree.  It locates its templates, static content, and
-- source files in development mode, relative to the current working
-- directory when it is run.
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
    --
    -- The most significant behavioral differences between the two
    -- loaders are how the action is determined, and when the
    -- loadAppState and cleanupAppState functions are executed.
    --
    -- The Hint loader uses the ghc api to interpret the sources when
    -- pages are loaded.  It also runs loadAppState and
    -- cleanupAppState for each request it handles.
    --
    -- The Static loader compiles all the actions when the app is
    -- compiled.  It runs loadAppState once, at the start of the
    -- program, and cleanupAppState once, at the end of the program.
    (cleanup, snap) <- $(loadSnapTH 'loadAppState 'cleanupAppState 'site)

    -- Run the server
    httpServeConfig conf snap

    -- Run the cleanup action before exiting
    cleanup
