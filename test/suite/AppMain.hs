module Main where

import           Snap.Http.Server.Config
import           Snap.Snaplet

import           Snap.Snaplet.App

main :: IO ()
main = serveSnaplet defaultConfig app
