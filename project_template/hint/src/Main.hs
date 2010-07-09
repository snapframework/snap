{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import           AppState
import           Site
import           Snap.Http.Server
#ifdef PRODUCTION
import           Snap.Loader.Static
#else
import           Snap.Loader.Hint
#endif

-- This is the entry point for this web server application.  It
-- supports easily switching between interpreting source and running
-- statically compiled code.
--
-- In either mode, the generated program should be run from the root
-- of the project tree.  It locates its templates, static content, and
-- source files in development mode, relative to the current working
-- directory when it is run.
--
-- When compiled without the production flag, only changes to the
-- libraries, your cabal file, or this file should require a recompile
-- to be picked up.  Everything else is interpreted at runtime.  There
-- are a few consequences of this.
--
-- First, this is much slower.  Running the interpreter seems to take
-- about 300ms on my system, regardless of the simplicity of the
-- loaded code.  The results of the interpreter process are cached for
-- a few seconds, to hopefully ensure that the the interpreter is only
-- invoked once for each load of a page and the resources it depends
-- on.
--
-- Second, the generated server binary is MUCH larger, since it links
-- in the GHC API (via the hint library).
--
-- Third, it results in loadAppState/cleanupAppState being called for
-- each request.  This is to ensure that the current state is
-- compatible with the running action.  If your application state
-- takes a long time to load or clean up, the penalty will be visible.
--
-- Fourth, and the reason you would ever want to actually compile
-- without production mode, is that it enables a *much* faster
-- development cycle.  You can simply edit a file, save your changes,
-- and hit reload to see your changes reflected immediately.
--
-- When this is compiled with the production flag, all the actions are
-- statically compiled in.  This results in much faster execution, a
-- smaller binary size, only running load and cleanup once per
-- application run, and having to recompile the server for any code
-- change.
main :: IO ()
main = do
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
    quickHttpServe snap

    -- Run the cleanup action before exiting
    cleanup
