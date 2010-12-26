{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

This is the entry point for this web server application.  It supports
easily switching between interpreting source and running statically
compiled code.

In either mode, the generated program should be run from the root of
the project tree.  When it is run, it locates its templates, static
content, and source files in development mode, relative to the current
working directory.

When compiled with the development flag, only changes to the
libraries, your cabal file, or this file should require a recompile to
be picked up.  Everything else is interpreted at runtime.  There are a
few consequences of this.

First, this is much slower.  Running the interpreter takes a
significant chunk of time (a couple tenths of a second on the author's
machine, at this time), regardless of the simplicity of the loaded
code.  In order to recompile and re-load server state as infrequently
as possible, the source directories are watched for updates, as are
any extra directories specified below.

Second, the generated server binary is MUCH larger, since it links in
the GHC API (via the hint library).

Third, and the reason you would ever want to actually compile with
development mode, is that it enables a faster development cycle. You
can simply edit a file, save your changes, and hit reload to see your
changes reflected immediately.

When this is compiled without the development flag, all the actions
are statically compiled in.  This results in faster execution, a
smaller binary size, and having to recompile the server for any code
change.

-}

module Main where

#ifdef DEVELOPMENT
import           Snap.Extension.Loader.Devel
import           Snap.Http.Server (quickHttpServe)
#else
import           Snap.Extension.Server
#endif

import           Application
import           Site

main :: IO ()
#ifdef DEVELOPMENT
main = do
    -- All source directories will be watched for updates
    -- automatically.  If any extra directories should be watched for
    -- updates, include them here.
    snap <- $(let extraWatcheDirs = ["resources/templates"]
              in loadSnapTH 'applicationInitializer 'site extraWatcheDirs)
    quickHttpServe snap
#else
main = quickHttpServe applicationInitializer site
#endif
