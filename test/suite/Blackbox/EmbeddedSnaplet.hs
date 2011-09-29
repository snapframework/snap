{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blackbox.EmbeddedSnaplet where

import Prelude hiding ((.))
import Control.Monad.State
import Data.Lens.Lazy
import Data.Lens.Template
import qualified Data.Text as T
import           Snap.Http.Server.Config
import Snap.Core
import Snap.Util.FileServe
import System.FilePath.Posix

import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Templating.Heist

-- If we universally quantify EmbeddedSnaplet to get rid of the type parameter
-- mkLabels throws an error "Can't reify a GADT data constructor"
data EmbeddedSnaplet = EmbeddedSnaplet
    { _embeddedHeist :: Snaplet (Heist EmbeddedSnaplet)
    , _embeddedVal :: Int
    }

makeLenses [''EmbeddedSnaplet]

instance HasHeist EmbeddedSnaplet where
    heistLens = subSnaplet embeddedHeist

embeddedInit :: SnapletInit EmbeddedSnaplet EmbeddedSnaplet
embeddedInit = makeSnaplet "embedded" "embedded snaplet" Nothing $ do
    hs <- nestSnaplet "heist" embeddedHeist $ heistInit "templates"

    -- This is the implementation of addTemplates, but we do it here manually
    -- to test coverage for addTemplatesAt.
    snapletPath <- getSnapletFilePath
    addTemplatesAt "embedded" (snapletPath </> "templates")

    embeddedLens <- getLens
    addRoutes [("aoeuhtns", withSplices
                    [("asplice", embeddedSplice embeddedLens)]
                    (render "embedded/embeddedpage"))
              ,("", heistServe)
              ]
    return $ EmbeddedSnaplet hs 42


embeddedSplice :: (Lens (Snaplet b) (Snaplet EmbeddedSnaplet))
              -> SnapletHeist b v Template
embeddedSplice embeddedLens = do
    val <- liftWith embeddedLens $ gets _embeddedVal
    liftHeist $ textSplice $ T.pack $ "splice value" ++ (show val)

