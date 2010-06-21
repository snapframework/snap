{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory

import           Glue
import           Server


main :: IO ()
main = do
    td <- newTemplateDirectory' "templates" emptyTemplateState
    quickServer $ templateHandler td defaultReloadHandler $ \ts ->
        ifTop (writeBS "hello world") <|>
        route [ ("foo", writeBS "bar")
              , ("echo/:echoparam", echoHandler)
              ] <|>
        templateServe ts <|>
        dir "static" (fileServe ".")


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
