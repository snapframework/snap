{-# LANGUAGE TemplateHaskell #-}

module Snap.Loader.Static where

------------------------------------------------------------------------------
import           Control.Arrow
import           Language.Haskell.TH.Syntax

------------------------------------------------------------------------------
-- | XXX
loadSnapTH :: Name -> Name -> Name -> Q Exp
loadSnapTH initialize cleanup action = do
    funE <- [| \c a -> fmap (c &&& a) |]

    let [initE, cleanE, actE] = map VarE [initialize, cleanup, action]
        simpleLoad = foldl AppE funE [cleanE, actE, initE]

    return simpleLoad
