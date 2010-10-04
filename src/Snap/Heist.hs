{-# LANGUAGE OverloadedStrings #-}
-- | This module contains convenience functions for helping render
-- Heist templates from Snap.
module Snap.Heist where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.ByteString.Char8 as S

import           Snap.Error
import           Snap.Types
import           Snap.Util.FileServe

import           Text.Templating.Heist


------------------------------------------------------------------------------
-- | This is a convenience function.  It calls 'render' with the
-- content type set to @text/html; charset=utf-8@.
renderHtml :: (MonadSnap m) => TemplateState m -> S.ByteString -> m ()
renderHtml = render "text/html; charset=utf-8"


------------------------------------------------------------------------------
-- | Renders a template with the provided content type.  If the
-- template cannot be loaded, 'internalError' is called with an error
-- message.
render :: (MonadSnap m)
       => S.ByteString -- ^ the content type to include in the response
       -> TemplateState m -- ^ the TemplateState that contains the template
       -> S.ByteString -- ^ the name of the template
       -> m ()
render contentType ts template = do
    bytes <- renderTemplate ts template
    flip (maybe missingTemplate) bytes $ \x -> do
        modifyResponse $ setContentType contentType
                       . setContentLength (fromIntegral $ S.length x)
        writeBS x
  where
    msg = S.append "Unable to load template: " template
    missingTemplate = internalError msg


------------------------------------------------------------------------------
-- | Handles the rendering of any template in TemplateState.
handleAllTemplates :: (MonadSnap m)
                   => TemplateState m -> m ()
handleAllTemplates ts =
    ifTop (renderHtml ts "index") <|>
    (renderHtml ts . S.pack =<< getSafePath)
    
