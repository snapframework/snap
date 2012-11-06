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
import System.FilePath.Posix

import Snap.Snaplet
import Snap.Snaplet.Heist
import Heist.Interpreted

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
    addTemplatesAt hs "onemoredir" (snapletPath </> "extra-templates")

    embeddedLens <- getLens
    addRoutes [("aoeuhtns", withSplices
                    [("asplice", embeddedSplice embeddedLens)]
                    (render "embeddedpage"))
              ]
    return $ EmbeddedSnaplet hs 42


embeddedSplice :: (Lens (Snaplet b) (Snaplet EmbeddedSnaplet))
               -> SnapletISplice b
embeddedSplice embeddedLens = do
    val <- lift $ with' embeddedLens $ gets _embeddedVal
    textSplice $ T.pack $ "splice value" ++ (show val)

