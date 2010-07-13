{-# LANGUAGE OverloadedStrings #-}
-- | This module contains convenience functions for helping render
-- Heist templates from Snap.
module Snap.Heist where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import           Snap.Types
import           Text.Templating.Heist


------------------------------------------------------------------------------
-- | This is a convenience function.  It calls 'render' with the
-- content type set to @text/html; charset=utf-8@.
renderHtml :: (MonadSnap m) => TemplateState m -> ByteString -> m ()
renderHtml = render "text/html; charset=utf-8"


------------------------------------------------------------------------------
-- | Renders a template with the provided content type.  If the
-- template cannot be loaded, 'pass' is called and the next handler is tried.
render :: (MonadSnap m)
       => ByteString      -- ^ the content type to include in the response
       -> TemplateState m -- ^ the TemplateState that contains the template
       -> ByteString      -- ^ the name of the template
       -> m ()
render contentType ts template = do
    bytes <- renderTemplate ts template
    flip (maybe pass) bytes $ \x -> do
        modifyResponse $ setContentType contentType
                       . setContentLength (fromIntegral $ S.length x)
        writeBS x
