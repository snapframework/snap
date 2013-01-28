{-|

A module exporting only functions for using compiled templates.  If you
import the main Snap.Snaplet.Heist module, it's easy to accidentally use
the interpreted render function even when you're using compiled Heist.
Importing only this module will make it harder to make mistakes like that.

-}
module Snap.Snaplet.Heist.Compiled
  ( H.Heist
  , H.HasHeist(..)
  , H.SnapletHeist
  , H.SnapletCSplice

  -- * Initializer Functions
  -- $initializerSection
  , H.heistInit
  , H.heistInit'
  , H.addTemplates
  , H.addTemplatesAt
  , H.addConfig
  , H.modifyHeistState
  , H.withHeistState

  -- * Handler Functions
  -- $handlerSection
  , render
  , renderAs
  , heistServe
  , heistServeSingle

  , H.clearHeistCache
  ) where

import           Data.ByteString (ByteString)
import           Snap.Snaplet
import qualified Snap.Snaplet.Heist as H


------------------------------------------------------------------------------
-- | Renders a compiled template as text\/html. If the given template is not
-- found, this returns 'empty'.
render :: H.HasHeist b
       => ByteString
           -- ^ Template name
       -> Handler b v ()
render = H.cRender


------------------------------------------------------------------------------
-- | Renders a compiled template as the given content type.  If the given
-- template is not found, this returns 'empty'.
renderAs :: H.HasHeist b
         => ByteString
             -- ^ Content type to render with
         -> ByteString
             -- ^ Template name
         -> Handler b v ()
renderAs = H.cRenderAs


------------------------------------------------------------------------------
-- | A handler that serves all the templates (similar to 'serveDirectory').
-- If the template specified in the request path is not found, it returns
-- 'empty'.  Also, this function does not serve any templates beginning with
-- an underscore.  This gives you a way to prevent some templates from being
-- served.  For example, you might have a template that contains only the
-- navbar of your pages, and you probably wouldn't want that template to be
-- visible to the user as a standalone template.  So if you put it in a file
-- called \"_nav.tpl\", this function won't serve it.
heistServe :: H.HasHeist b => Handler b v ()
heistServe = H.cHeistServe


------------------------------------------------------------------------------
-- | Handler for serving a single template (similar to 'fileServeSingle'). If
-- the given template is not found, this throws an error.
heistServeSingle :: H.HasHeist b
                 => ByteString
                     -- ^ Template name
                 -> Handler b v ()
heistServeSingle = H.cHeistServeSingle

