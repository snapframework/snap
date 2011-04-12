{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes are defined for your site. The
'router' function combines everything together and is exported by this module.

-}

module Router
  ( router
  ) where

import           Control.Applicative ((<|>))
import           Snap.Types (route)

import           Application (Application)
import           Snap.Util.FileServe (serveDirectory)
import           Handlers

------------------------------------------------------------------------------
-- | The main entry point handler.
router :: Application ()
router = route [ ("/",            index)
               , ("/echo/:stuff", echo)
               ]
         <|> serveDirectory "resources/static"
