{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe $
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (fileServe ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
