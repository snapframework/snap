{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-

NOTE: Don't modify this file unless you know what you are doing.  If you are
new to snap, start with Site.hs and Application.hs.  This file contains
boilerplate needed for dynamic reloading and is not meant for general
consumption.

Occasionally if we modify the way the dynamic reloader works and you want to
upgrade, you might have to swap out this file for a newer version.  But in
most cases you'll never need to modify this code.

-}
module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import qualified Data.Text as T
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Core
import           System.IO
import           Site

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif


------------------------------------------------------------------------------
-- | This is the entry point for this web server application. It supports
-- easily switching between interpreting source and running statically compiled
-- code.
--
-- In either mode, the generated program should be run from the root of the
-- project tree. When it is run, it locates its templates, static content, and
-- source files in development mode, relative to the current working directory.
--
-- When compiled with the development flag, only changes to the libraries, your
-- cabal file, or this file should require a recompile to be picked up.
-- Everything else is interpreted at runtime. There are a few consequences of
-- this.
--
-- First, this is much slower. Running the interpreter takes a significant
-- chunk of time (a couple tenths of a second on the author's machine, at this
-- time), regardless of the simplicity of the loaded code. In order to
-- recompile and re-load server state as infrequently as possible, the source
-- directories are watched for updates, as are any extra directories specified
-- below.
--
-- Second, the generated server binary is MUCH larger, since it links in the
-- GHC API (via the hint library).
--
-- Third, and the reason you would ever want to actually compile with
-- development mode, is that it enables a faster development cycle. You can
-- simply edit a file, save your changes, and hit reload to see your changes
-- reflected immediately.
--
-- When this is compiled without the development flag, all the actions are
-- statically compiled in. This results in faster execution, a smaller binary
-- size, and having to recompile the server for any code change.
--
main :: IO ()
main = do
    -- Depending on the version of loadSnapTH in scope, this either enables
    -- dynamic reloading, or compiles it without. The last argument to
    -- loadSnapTH is a list of additional directories to watch for changes to
    -- trigger reloads in development mode. It doesn't need to include source
    -- directories, those are picked up automatically by the splice.
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup


------------------------------------------------------------------------------
-- | This action loads the config used by this application. The loaded config
-- is returned as the first element of the tuple produced by the loadSnapTH
-- Splice. The type is not solidly fixed, though it must be an IO action that
-- produces the same type as 'getActions' takes. It also must be an instance of
-- Typeable. If the type of this is changed, a full recompile will be needed to
-- pick up the change, even in development mode.
--
-- This action is only run once, regardless of whether development or
-- production mode is in use.
getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig


------------------------------------------------------------------------------
-- | This function generates the the site handler and cleanup action from the
-- configuration. In production mode, this action is only run once. In
-- development mode, this action is run whenever the application is reloaded.
--
-- Development mode also makes sure that the cleanup actions are run
-- appropriately before shutdown. The cleanup action returned from loadSnapTH
-- should still be used after the server has stopped handling requests, as the
-- cleanup actions are only automatically run when a reload is triggered.
--
-- This sample doesn't actually use the config passed in, but more
-- sophisticated code might.
getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
