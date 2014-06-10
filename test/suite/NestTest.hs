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

------------------------------------------------------------------------------
import Prelude hiding ((.))
import Control.Lens
import Control.Monad.State
import qualified Data.Text as T
import           Snap.Http.Server.Config
import Snap.Core
import Snap.Util.FileServe

import Data.Map.Syntax ((##))
import Snap.Snaplet
import Snap.Snaplet.Heist
import Heist.Interpreted


------------------------------------------------------------------------------
-- If we universally quantify FooSnaplet to get rid of the type parameter
-- mkLabels throws an error "Can't reify a GADT data constructor"
data FooSnaplet = FooSnaplet
    { _fooHeist :: Snaplet (Heist FooSnaplet)
    , _fooVal :: Int
    }

makeLenses ''FooSnaplet

instance HasHeist FooSnaplet where
    heistLens = subSnaplet fooHeist

fooInit :: SnapletInit FooSnaplet FooSnaplet
fooInit = makeSnaplet "foosnaplet" "foo snaplet" Nothing $ do
    hs <- nestSnaplet "heist" fooHeist $ heistInit "templates"
    addTemplates hs "foo"
    rootUrl <- getSnapletRootURL
    fooLens <- getLens
    addRoutes [("fooRootUrl", writeBS rootUrl)
              ,("aoeuhtns", renderWithSplices "foo/foopage"
                    ("asplice" ## fooSplice fooLens))
              ,("", heistServe)
              ]
    return $ FooSnaplet hs 42


--fooSplice :: (Lens (Snaplet b) (Snaplet (FooSnaplet b)))
--          -> SnapletSplice (Handler b b)
fooSplice :: (SnapletLens (Snaplet b) FooSnaplet)
          -> SnapletISplice b
fooSplice fooLens = do
    val <- lift $ with' fooLens $ gets _fooVal
    textSplice $ T.pack $ "splice value" ++ (show val)

------------------------------------------------------------------------------

data App = App
    { _foo :: Snaplet (FooSnaplet)
    }

makeLenses ''App

app :: SnapletInit App App
app = makeSnaplet "app" "nested snaplet application" Nothing $ do
    fs <- embedSnaplet "foo" foo fooInit
    addRoutes [ ("/hello", writeText "hello world")
              , ("/public", serveDirectory "public")
              ]
    return $ App fs

main :: IO ()
main = serveSnaplet defaultConfig app

