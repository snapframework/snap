{-# LANGUAGE OverloadedStrings #-}
module Snap.Heist where

import qualified Data.ByteString.Char8 as S

import           Snap.Error
import           Snap.Types

import           Text.Templating.Heist


renderHtml :: (MonadSnap m) => TemplateState m -> S.ByteString -> m ()
renderHtml = render "text/html; charset=utf-8"


render :: (MonadSnap m) =>
          S.ByteString -> TemplateState m -> S.ByteString -> m ()
render contentType ts template = do
    bytes <- renderTemplate ts template
    flip (maybe missingTemplate) bytes $ \x -> do
        modifyResponse $ setContentType contentType
                       . setContentLength (fromIntegral $ S.length x)
        writeBS x
  where
    msg = S.append "Unable to load template: " template
    missingTemplate = internalError msg
