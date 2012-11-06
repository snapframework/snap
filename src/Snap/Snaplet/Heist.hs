------------------------------------------------------------------------------
-- | The Heist snaplet makes it easy to add Heist to your application and use
-- it in other snaplets.
--

module Snap.Snaplet.Heist
  (
  -- * Heist and its type class
    Heist
  , HasHeist(..)

  -- * Initializer Functions
  -- $initializerSection
  , heistInit
  , heistInit'
  , addTemplates
  , addTemplatesAt
  , Unclassed.addConfig
  , modifyHeistState
  , withHeistState
  , addSplices

  -- * Handler Functions
  -- $handlerSection
  , cRender
  , cRenderAs
  , cHeistServe
  , cHeistServeSingle

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
  , Unclassed.SnapletCSplice
  , Unclassed.SnapletISplice

  , clearHeistCache
  ) where

------------------------------------------------------------------------------
import           Prelude hiding (id, (.))
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Heist
------------------------------------------------------------------------------
import           Snap.Snaplet
import qualified Snap.Snaplet.HeistNoClass as Unclassed
import           Snap.Snaplet.HeistNoClass ( Heist
                                           , heistInit
                                           , heistInit'
                                           , clearHeistCache
                                           )


------------------------------------------------------------------------------
-- | A single snaplet should never need more than one instance of Heist as a
-- subsnaplet.  This type class allows you to make it easy for other snaplets
-- to get the lens that identifies the heist snaplet.  Here's an example of
-- how the heist snaplet might be declared:
--
-- > data App = App { _heist :: Snaplet (Heist App) }
-- > mkLabels [''App]
-- >
-- > instance HasHeist App where heistLens = subSnaplet heist
-- >
-- > appInit = makeSnaplet "app" "" Nothing $ do
-- >     h <- nestSnaplet "heist" heist $ heistInit "templates"
-- >     addSplices myAppSplices
-- >     return $ App h
class HasHeist b where
    -- | A lens to the Heist snaplet.  The b parameter to Heist will
    -- typically be the base state of your application.
    heistLens :: SnapletLens (Snaplet b) (Heist b)


-- $initializerSection
-- This section contains functions for use in setting up your Heist state
-- during initialization.


------------------------------------------------------------------------------
-- | Adds templates to the Heist HeistState.  Other snaplets should use
-- this function to add their own templates.  The templates are automatically
-- read from the templates directory in the current snaplet's filesystem root.
addTemplates :: HasHeist b
             => Snaplet (Heist b)
             -> ByteString
                 -- ^ The url prefix for the template routes
             -> Initializer b v ()
addTemplates h pfx = withTop' heistLens (Unclassed.addTemplates h pfx)


------------------------------------------------------------------------------
-- | Adds templates to the Heist HeistState, and lets you specify where
-- they are found in the filesystem.  Note that the path to the template
-- directory is an absolute path.  This allows you more flexibility in where
-- your templates are located, but means that you have to explicitly call
-- getSnapletFilePath if you want your snaplet to use templates within its
-- normal directory structure.
addTemplatesAt :: HasHeist b
               => Snaplet (Heist b)
               -> ByteString
                   -- ^ URL prefix for template routes
               -> FilePath
                   -- ^ Path to templates
               -> Initializer b v ()
addTemplatesAt h pfx p =
    withTop' heistLens (Unclassed.addTemplatesAt h pfx p)


------------------------------------------------------------------------------
-- | Allows snaplets to add interpreted splices.
--
-- NOTE: The splices added with this function will not work if you render your
-- templates with cRender.  To add splices that work with cRender, you have to
-- use the addConfig function to add compiled splices or load time splices.
addSplices :: (HasHeist b)
           => [(Text, Unclassed.SnapletISplice b)]
               -- ^ Splices to bind
           -> Initializer b v ()
addSplices = Unclassed.addSplices' heistLens


------------------------------------------------------------------------------
-- | More general function allowing arbitrary HeistState modification.
modifyHeistState :: (HasHeist b)
                 => (HeistState (Handler b b) -> HeistState (Handler b b))
                     -- ^ HeistState modifying function
                 -> Initializer b v ()
modifyHeistState = Unclassed.modifyHeistState' heistLens


------------------------------------------------------------------------------
-- | Runs a function on with the Heist snaplet's 'HeistState'.
withHeistState :: (HasHeist b)
               => (HeistState (Handler b b) -> a)
                   -- ^ HeistState function to run
               -> Handler b v a
withHeistState = Unclassed.withHeistState' heistLens


-- $handlerSection
-- This section contains functions in the 'Handler' monad that you'll use in
-- processing requests.  Functions beginning with a 'c' prefix use compiled
-- template rendering.  The other functions use the older interpreted
-- rendering.  Splices added with addSplices will only work if you use
-- interpreted rendering.


------------------------------------------------------------------------------
-- | Renders a template as text\/html. If the given template is not found,
-- this returns 'empty'.
render :: HasHeist b
       => ByteString
           -- ^ Template name
       -> Handler b v ()
render t = withTop' heistLens (Unclassed.render t)


------------------------------------------------------------------------------
-- | Renders a template as the given content type.  If the given template
-- is not found, this returns 'empty'.
renderAs :: HasHeist b
         => ByteString
             -- ^ Content type to render with
         -> ByteString
             -- ^ Template name
         -> Handler b v ()
renderAs ct t = withTop' heistLens (Unclassed.renderAs ct t)


------------------------------------------------------------------------------
-- | Renders a compiled template as text\/html. If the given template is not
-- found, this returns 'empty'.
cRender :: HasHeist b
        => ByteString
            -- ^ Template name
        -> Handler b v ()
cRender t = withTop' heistLens (Unclassed.cRender t)


------------------------------------------------------------------------------
-- | Renders a compiled template as the given content type.  If the given
-- template is not found, this returns 'empty'.
cRenderAs :: HasHeist b
          => ByteString
              -- ^ Content type to render with
          -> ByteString
              -- ^ Template name
          -> Handler b v ()
cRenderAs ct t = withTop' heistLens (Unclassed.cRenderAs ct t)


------------------------------------------------------------------------------
-- | Analogous to 'fileServe'. If the template specified in the request path
-- is not found, it returns 'empty'.  Also, this function does not serve any
-- templates beginning with an underscore.  This gives you a way to prevent
-- some templates from being served.  For example, you might have a template
-- that contains only the navbar of your pages, and you wouldn't want that
-- template to be visible to the user as a standalone template.
heistServe :: HasHeist b => Handler b v ()
heistServe = withTop' heistLens Unclassed.heistServe


------------------------------------------------------------------------------
-- | Analogous to 'fileServeSingle'. If the given template is not found,
-- this throws an error.
heistServeSingle :: HasHeist b
                 => ByteString
                     -- ^ Template name
                 -> Handler b v ()
heistServeSingle t = withTop' heistLens (Unclassed.heistServeSingle t)


------------------------------------------------------------------------------
-- | A compiled version of 'heistServe'.
cHeistServe :: HasHeist b => Handler b v ()
cHeistServe = withTop' heistLens Unclassed.cHeistServe


------------------------------------------------------------------------------
-- | Analogous to 'fileServeSingle'. If the given template is not found,
-- this throws an error.
cHeistServeSingle :: HasHeist b
                 => ByteString
                     -- ^ Template name
                 -> Handler b v ()
cHeistServeSingle t = withTop' heistLens (Unclassed.cHeistServeSingle t)


------------------------------------------------------------------------------
-- | Renders a template with a given set of splices.  This is syntax sugar for
-- a common combination of heistLocal, bindSplices, and render.
renderWithSplices :: HasHeist b
                  => ByteString
                      -- ^ Template name
                  -> [(Text, Unclassed.SnapletISplice b)]
                      -- ^ Splices to bind
                  -> Handler b v ()
renderWithSplices = Unclassed.renderWithSplices' heistLens


------------------------------------------------------------------------------
-- | Runs an action with additional splices bound into the Heist
-- 'HeistState'.
withSplices :: HasHeist b
            => [(Text, Unclassed.SnapletISplice b)]
                -- ^ Splices to bind
            -> Handler b v a
                -- ^ Handler to run
            -> Handler b v a
withSplices = Unclassed.withSplices' heistLens


------------------------------------------------------------------------------
-- | Runs a handler with a modified 'HeistState'.  You might want to use
-- this if you had a set of splices which were customised for a specific
-- action.  To do that you would do:
--
-- > heistLocal (bindSplices mySplices) handlerThatNeedsSplices
heistLocal :: HasHeist b
           => (HeistState (Handler b b) -> HeistState (Handler b b))
               -- ^ HeistState modifying function
           -> Handler b v a
               -- ^ Handler to run
           -> Handler b v a
heistLocal = Unclassed.heistLocal' heistLens


-- $spliceSection
-- As can be seen in the type signature of heistLocal, the internal
-- HeistState used by the heist snaplet is parameterized by (Handler b b).
-- The reasons for this are beyond the scope of this discussion, but the
-- result is that 'lift' inside a splice only works with @Handler b b@
-- actions.  When you're writing your own snaplets you obviously would rather
-- work with @Handler b v@ so your local snaplet's state is available.  We
-- provide the SnapletHeist monad to make this possible.  The general rule is
-- that when you're using Snaplets and Heist, use SnapletHeist instead of
-- HeistT (previously called TemplateMonad) and use SnapletISplice instead of
-- Splice.

