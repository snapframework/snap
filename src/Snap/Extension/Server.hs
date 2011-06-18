{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|

This module provides replacements for the 'httpServe' and 'quickHttpServe'
functions exported by 'Snap.Http.Server'. By taking a 'Initializer' as an
argument, these functions simplify the glue code that is needed to use Snap
Extensions.

-}

module Snap.Extension.Server
  ( ConfigExtend
  , httpServe
  , quickHttpServe
  , defaultConfig
  , getReloadHandler
  , setReloadHandler
  , module Snap.Http.Server.Config
  ) where

import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Prelude hiding (catch)
import           Snap.Extension
import qualified Snap.Http.Server as SS
import qualified Snap.Http.Server.Config as C
import           Snap.Http.Server.Config hiding ( defaultConfig
                                                , completeConfig
                                                , getOther
                                                , setOther
                                                )
import           Snap.Util.GZip
import           Snap.Types
import           System.IO


------------------------------------------------------------------------------
-- | 'ConfigExtend' is similar to the 'Config' exported by 'Snap.Http.Server',
-- but is augmented with a @reloadHandler@ field which can be accessed using
-- 'getReloadHandler' and 'setReloadHandler'.
type ConfigExtend s = Config
    (SnapExtend s) (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())


------------------------------------------------------------------------------
getReloadHandler :: ConfigExtend s -> Maybe
                      (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
getReloadHandler = C.getOther


------------------------------------------------------------------------------
setReloadHandler :: (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
                 -> ConfigExtend s
                 -> ConfigExtend s
setReloadHandler = C.setOther




------------------------------------------------------------------------------
-- | These are the default values for all the fields in 'ConfigExtend'.
--
-- > hostname      = "localhost"
-- > address       = "0.0.0.0"
-- > port          = 8000
-- > accessLog     = "log/access.log"
-- > errorLog      = "log/error.log"
-- > locale        = "en_US"
-- > compression   = True
-- > verbose       = True
-- > errorHandler  = prints the error message
-- > reloadHandler = prints the result of each reload handler (error/success)
--
defaultConfig :: ConfigExtend s
defaultConfig = setReloadHandler handler C.defaultConfig
  where
    handler = path "admin/reload" . defaultReloadHandler


------------------------------------------------------------------------------
-- | Completes a partial 'Config' by filling in the unspecified values with
-- the default values from 'defaultConfig'.
completeConfig :: ConfigExtend s -> IO (ConfigExtend s)
completeConfig = C.completeConfig . (mappend defaultConfig)


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'ConfigExtend' passed in. This function never returns; to shut down
-- the HTTP server, kill the controlling thread.
httpServe :: ConfigExtend s
          -- ^ Any configuration options which override the defaults
          -> Initializer s
          -- ^ The 'Initializer' function for the application's monad
          -> SnapExtend s ()
          -- ^ The application to be served
          -> IO ()
httpServe config initializer handler = do
    conf <- completeConfig config
    let !verbose  = fromJust $ getVerbose conf
    let !reloader = fromJust $ getReloadHandler conf
    let !compress = if fromJust $ getCompression conf then withCompression else id
    let !catch500 = flip catch $ fromJust $ getErrorHandler conf
    let !serve    = SS.simpleHttpServe config
    (site, cleanup) <- runInitializerWithReloadAction
                         verbose
                         initializer
                         (catch500 $ compress handler)
                         reloader
    _   <- try $ serve $ site :: IO (Either SomeException ())
    putStr "\n"
    cleanup


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by
-- 'commandLineConfig'.
quickHttpServe :: Initializer s
               -- ^ The 'Initializer' function for the application's monad
               -> SnapExtend s ()
               -- ^ The application to be served
               -> IO ()
quickHttpServe r m = commandLineConfig emptyConfig >>= \c -> httpServe c r m

------------------------------------------------------------------------------
fromUTF8 :: ByteString -> String
fromUTF8 = T.unpack . T.decodeUtf8
