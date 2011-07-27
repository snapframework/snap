{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

The Heist snaplet makes it easy to add Heist to your application and use it in
other snaplets.

-}

module Snap.Snaplet.Heist
  (
  -- * Heist and its type class
    Heist
  , HasHeist(..)

  -- * Initializer Functions
  -- $initializerSection
  , heistInit
  , addTemplates
  , addTemplatesAt
  , addSplices

  -- * Handler Functions
  -- $handlerSection
  , render
  , renderAs
  , heistServe
  , heistServeSingle
  , heistLocal
  , withSplices
  , renderWithSplices

  -- * Writing Splices
  -- $spliceSection
  , Unclassed.SnapletHeist
  , Unclassed.SnapletSplice
  , Unclassed.liftHeist
  , Unclassed.liftHandler
  , Unclassed.liftWith
  , Unclassed.bindSnapletSplices

  , clearHeistCache
  ) where

import           Prelude hiding (id, (.))
import           Control.Category
import           Data.ByteString (ByteString)
import           Data.Record.Label
import           Data.Text (Text)
import           Text.Templating.Heist

import           Snap.Snaplet

import qualified Snap.Snaplet.HeistNoClass as Unclassed
import           Snap.Snaplet.HeistNoClass (Heist, heistInit, clearHeistCache)


------------------------------------------------------------------------------
-- | A single snaplet should never need more than one instance of Heist as a
-- subsnaplet.  This type class allows you to make it easy for other snaplets
-- to get the lens that identifies the heist snaplet.  Here's an example of
-- how the heist snaplet might be declared:
--
-- > data App = App { _heist :: Snaplet (Heist App) }
-- > mkLabels [''App]
-- >
-- > instance HasHeist App App where heistLens = subSnaplet heist
-- >
-- > appInit = makeSnaplet "app" "" Nothing $ do
-- >     h <- nestSnaplet "heist" $ heistInit "templates"
-- >     addSplices myAppSplices
-- >     return $ App h
class HasHeist b e where
    -- | A lens from e to the Heist snaplet.  The b parameter to Heist will
    -- typically be the base state of your application.  Most of the time
    -- 'b' and 'e' will be the same.
    heistLens :: Snaplet e :-> Snaplet (Heist b)


------------------------------------------------------------------------------
-- | This default instance allows you to avoid writing a HasHeist instance and
-- instead access the heist snaplet via the 'with' or 'withTop'
-- functions.  This might be preferrable in situations where Heist usage is
-- very simple.
instance HasHeist b (Heist b) where heistLens = id


-- $initializerSection
-- This section contains functions for use in setting up your Heist state
-- during initialization.


------------------------------------------------------------------------------
-- | Adds templates to the Heist TemplateState.  Other snaplets should use
-- this function to add their own templates.  The templates are automatically
-- read from the templates directory in the current snaplet's filesystem root.
addTemplates :: HasHeist b e => ByteString -> Initializer b e ()
addTemplates pfx = with' heistLens (Unclassed.addTemplates pfx)


------------------------------------------------------------------------------
-- | Adds templates to the Heist TemplateState, and lets you specify where
-- they are fonud in the filesystem.
addTemplatesAt :: HasHeist b e => ByteString -> FilePath -> Initializer b e ()
addTemplatesAt pfx p = with' heistLens (Unclassed.addTemplatesAt pfx p)


------------------------------------------------------------------------------
-- | Allows snaplets to add splices.
addSplices :: (HasHeist b e)
           => [(Text, Unclassed.SnapletSplice b e)] -> Initializer b e ()
addSplices = Unclassed.addSplices' heistLens


-- $handlerSection
-- This section contains functions in the 'Handler' monad that you'll use in
-- processing requests.


------------------------------------------------------------------------------
-- | Renders a template as text\/html. If the given template is not found,
-- this returns 'empty'.
render :: HasHeist b e => ByteString -> Handler b e ()
render t = with' heistLens (Unclassed.render t)


------------------------------------------------------------------------------
-- | Renders a template as the given content type.  If the given template
-- is not found, this returns 'empty'.
renderAs :: HasHeist b e => ByteString -> ByteString -> Handler b e ()
renderAs ct t = with' heistLens (Unclassed.renderAs ct t)


------------------------------------------------------------------------------
-- | Analogous to 'fileServe'. If the template specified in the request path
-- is not found, it returns 'empty'.
heistServe :: HasHeist b e => Handler b e ()
heistServe = with' heistLens Unclassed.heistServe


------------------------------------------------------------------------------
-- | Analogous to 'fileServeSingle'. If the given template is not found,
-- this throws an error.
heistServeSingle :: HasHeist b e => ByteString -> Handler b e ()
heistServeSingle t = with' heistLens (Unclassed.heistServeSingle t)


------------------------------------------------------------------------------
-- | Renders a template with a given set of splices.  This is syntax sugar for
-- a common combination of heistLocal, bindSplices, and render.
renderWithSplices :: (HasHeist b e)
                  => ByteString
                  -> [(Text, Unclassed.SnapletSplice b e)]
                  -> Handler b e ()
renderWithSplices = Unclassed.renderWithSplices' heistLens


------------------------------------------------------------------------------
-- | Runs an action with additional splices bound into the Heist
-- 'TemplateState'.
withSplices :: HasHeist b e
            => [(Text, Unclassed.SnapletSplice b e)]
            -> Handler b e a
            -> Handler b e a
withSplices = Unclassed.withSplices' heistLens


------------------------------------------------------------------------------
-- | Runs a handler with a modified 'TemplateState'.  You might want to use
-- this if you had a set of splices which were customised for a specific
-- action.  To do that you would do:
--
-- > heistLocal (bindSplices mySplices) handlerThatNeedsSplices
heistLocal :: HasHeist b e
           => (TemplateState (Handler b b) -> TemplateState (Handler b b))
           -> Handler b e a
           -> Handler b e a
heistLocal = Unclassed.heistLocal' heistLens


-- $spliceSection
-- As can be seen in the type signature of heistLocal, the internal
-- TemplateState used by the heist snaplet is parameterized by (Handler b b).
-- The reasons for this are beyond the scope of this discussion, but the
-- result is that 'lift' inside a splice only works with @Handler b b@
-- actions.  When you're writing your own snaplets you obviously would rather
-- work with @Handler b e@ so your local snaplet's state is available.  We
-- provide the SnapletHeist monad to make this possible.  The general rule is
-- that when you're using Snaplets and Heist, use SnapletHeist instead of
-- HeistT (previously called TemplateMonad) and use SnapletSplice instead of
-- Splice.

