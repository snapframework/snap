{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

This is the entry point for this web server application.  It supports easily
switching between interpreting source and running statically compiled code.

In either mode, the generated program should be run from the root of the
project tree.  It locates its templates, static content, and source files in 
development mode, relative to the current working directory when it is run.

When compiled without the production flag, only changes to the libraries, your
cabal file, or this file should require a recompile to be picked up.
Everything else is interpreted at runtime.  There are a few consequences of
this.

First, this is much slower.  Running the interpreter seems to take about
300ms, regardless of the simplicity of the loaded code.  The results of the
interpreter process are cached for a few seconds, to hopefully ensure that
the the interpreter is only invoked once for each load of a page and the
resources it depends on.

Second, the generated server binary is MUCH larger, since it links in the GHC
API (via the hint library).

Third, it results in initialization\/cleanup code defined by the @Initializer@
being called for each request.  This is to ensure that the current state is
compatible with the running action.  If your application state takes a long
time to load or clean up, the penalty will be visible.

Fourth, and the reason you would ever want to actually compile without
production mode, is that it enables a *much* faster development cycle. You can
simply edit a file, save your changes, and hit reload to see your changes
reflected immediately.

When this is compiled with the production flag, all the actions are statically
compiled in.  This results in much faster execution, a smaller binary size,
only running initialization and cleanup once per application run, and having
to recompile the server for any code change.

-}

module Main where

#ifdef PRODUCTION
import           Snap.Extension.Server
#else
import           Snap.Extension.Loader.Devel
import           Snap.Http.Server (quickHttpServe)
#endif

import           Application
import           Site

main :: IO ()
#ifdef PRODUCTION
main = quickHttpServe applicationInitializer site
#else
main = do
    snap <- $(loadSnapTH 'applicationInitializer 'site)
    quickHttpServe snap
#endif
