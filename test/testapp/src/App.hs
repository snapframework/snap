{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Record.Label
import qualified Data.Text as T
import Snap.Types

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.SimpleTest
import Text.Templating.Heist

data App = App
    { _heist :: Snaplet (Heist App)
    , _simple :: Snaplet SimpleTest
    }

mkLabels [''App]

type MyApp = Handler App App
type instance Base MyApp = App

instance MonadHeist (Handler App App) where
    withHeist = with heist

hello :: Handler App App ()
hello = do
    str <- with simple getSpecial
    writeText $ T.pack $ "hello world "++str

------------------------------------------------------------------------------
-- | 
app :: Initializer App App App
app = do
    hs <- nestSnaplet "heist" $ heistInit "templates"
    ss <- nestSnaplet "simple" $ simpleTestInit heist
    iWith heist $ addSplices
        [("mysplice", textSplice "woo hoo, it worked")]
    addRoutes [ ("/hello", hello)
              , ("", with heist heistServe)
              ]
    return $ App hs ss

main :: IO ()
main = serveSnaplet app

