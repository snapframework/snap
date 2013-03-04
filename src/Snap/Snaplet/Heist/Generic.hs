{-|

A module exporting only generic functions that choose between compiled and
interpreted mode based on the setting specified in the initializer.  This
module is most useful for writitng general snaplets that use Heist and are
meant to be used in applications that might use either interpreted or compiled
templates.

-}
module Snap.Snaplet.Heist.Generic
  ( Heist
  , HasHeist(..)
  , SnapletHeist
  , SnapletCSplice

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
  , gRender
  , gRenderAs
  , gHeistServe
  , gHeistServeSingle
  , chooseMode

  , clearHeistCache
  ) where

import Snap.Snaplet.Heist
