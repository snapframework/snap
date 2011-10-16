module Main where

import           Snap.Http.Server.Config
import           Snap.Snaplet

import           Blackbox.App

main :: IO ()
main = serveSnaplet defaultConfig app
