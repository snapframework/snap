{-# LANGUAGE OverloadedStrings #-}
module Glue where

import qualified Data.ByteString.Char8 as S

import           Snap.Types
import           Text.Templating.Heist


render :: TemplateState Snap -> S.ByteString -> Snap ()
render ts template = do
    bytes <- renderTemplate ts template
    flip (maybe missingTemplate) bytes $ \x -> do
        modifyResponse $ setContentType "text/html; charset=utf-8"
                       . setContentLength (fromIntegral $ S.length x)
        writeBS x
  where
    missingTemplate = do
        let msg = S.append "Unable to load template: " template
        modifyResponse $ setContentType "text/plain; charset=utf-8"
                       . setContentLength (fromIntegral $ S.length msg)
                       . setResponseStatus 500 "Internal Server Error"
        writeBS msg


