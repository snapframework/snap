{-|

A module exporting only functions for using interpreted templates.  If
you import the main Snap.Snaplet.Heist module, it's easy to accidentally
use the compiled render function even when you're using interpreted Heist.
Importing only this module will make it harder to make mistakes like that.

-}
module Snap.Snaplet.Heist.Interpreted
  ( Heist
  , HasHeist(..)
  , SnapletHeist
  , SnapletISplice

  -- * Initializer Functions
  -- $initializerSection
  , heistInit
  , heistInit'
  , addTemplates
  , addTemplatesAt
  , addConfig
  , modifyHeistState
  , withHeistState

  -- * Handler Functions
  -- $handlerSection
  , render
  , renderAs
  , heistServe
  , heistServeSingle
  , heistLocal
  , withSplices
  , renderWithSplices

  , clearHeistCache
  ) where

import Snap.Snaplet.Heist

