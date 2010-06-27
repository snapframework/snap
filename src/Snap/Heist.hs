{-# LANGUAGE OverloadedStrings #-}
module Snap.Heist where

import qualified Data.ByteString.Char8 as S

import           Snap.Iteratee
import           Snap.Types

import           Text.Templating.Heist


renderHtml :: TemplateState Snap -> S.ByteString -> Snap ()
renderHtml = render "text/html; charset=utf-8"


render :: S.ByteString -> TemplateState Snap -> S.ByteString -> Snap ()
render contentType ts template = do
    bytes <- renderTemplate ts template
    flip (maybe missingTemplate) bytes $ \x -> do
        modifyResponse $ setContentType contentType
                       . setContentLength (fromIntegral $ S.length x)
        writeBS x
  where
    missingTemplate = do
        let msg = S.append "Unable to load template: " template
            rsp = setContentType "text/plain; charset=utf-8"
                . setContentLength (fromIntegral $ S.length msg)
                . setResponseStatus 500 "Internal Server Error"
                . modifyResponseBody (>. enumBS msg)
                $ emptyResponse

        finishWith rsp
