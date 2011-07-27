{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude hiding ((.))
import Control.Monad.State
import Data.Record.Label
import qualified Data.Text as T
import           Snap.Http.Server.Config
import Snap.Types
import Snap.Util.FileServe

import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Templating.Heist

-- If we universally quantify FooSnaplet to get rid of the type parameter
-- mkLabels throws an error "Can't reify a GADT data constructor"
data FooSnaplet b = FooSnaplet
    { _fooHeist :: Snaplet (Heist b)
    , _fooVal :: Int
    }

mkLabels [''FooSnaplet]

instance HasHeist b (FooSnaplet b) where
    heistLens = subSnaplet fooHeist

fooInit :: Initializer b (FooSnaplet b) (Snaplet (FooSnaplet b))
fooInit = makeSnaplet "foosnaplet" "foo snaplet" Nothing $ do
    hs <- nestSnaplet "heist" $ heistInit "templates"
    addTemplates "foo"
    rootUrl <- getSnapletRootURL
    fooLens <- getLens
    addRoutes [("fooRootUrl", writeBS rootUrl)
              ,("aoeuhtns", renderWithSplices "foo/foopage"
                    [("asplice", fooSplice fooLens)])
              ,("", heistServe)
              ]
    return $ FooSnaplet hs 42


--fooSplice :: (Snaplet b :-> Snaplet (FooSnaplet b))
--          -> SnapletSplice (Handler b b)
fooSplice :: (Snaplet b :-> Snaplet (FooSnaplet b))
          -> SnapletHeist b e Template
fooSplice fooLens = do
    val <- liftWith fooLens $ gets _fooVal
    liftHeist $ textSplice $ T.pack $ "splice value" ++ (show val)

------------------------------------------------------------------------------

data App = App
    { _foo :: Snaplet (FooSnaplet App)
    }

mkLabels [''App]

app :: Initializer App App (Snaplet App)
app = makeSnaplet "app" "nested snaplet application" Nothing $ do
    fs <- with foo $ nestSnaplet "foo" $ fooInit
    addRoutes [ ("/hello", writeText "hello world")
              , ("/public", serveDirectory "public")
              , ("/admin/reload", reloadSite)
              ]
    return $ App fs

main :: IO ()
main = serveSnaplet defaultConfig app

