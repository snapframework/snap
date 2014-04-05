{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}


module Snap.Snaplet.Test.App
 ( App(..)
 , appInit
 , failingAppInit
 ) where


------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans (liftIO)
------------------------------------------------------------------------------
import           Snap.Snaplet


------------------------------------------------------------------------------
data App = App

makeLenses ''App


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do
   return App

failingAppInit :: SnapletInit App App
failingAppInit = makeSnaplet "app" "Test application" Nothing $ do
   error "Error"
   return App
