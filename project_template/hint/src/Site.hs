{-# LANGUAGE OverloadedStrings #-}
module Site where

import           Config

import           Control.Monad (msum)

import qualified Data.ByteString.Char8 as S

import           Snap.Util.FileServe (fileServe)
import           Snap.Types

import           Text.Templating.Heist


frontPage :: TemplateState Snap -> Snap ()
frontPage ts = ifTop $ do
    modifyResponse $ setContentType "text/html; charset=utf-8"

    Just rendered <- renderTemplate ts "index"
    writeBS rendered
    let len = fromIntegral . S.length $ rendered
    modifyResponse . setContentLength $ len


staticResources :: Snap ()
staticResources = fileServe "resources/static"


site :: Config -> Snap ()
site ts = msum [ frontPage $ templateState ts
               , staticResources
               ]
