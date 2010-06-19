{-# LANGUAGE TemplateHaskell #-}

module Snap.Loader.Static where

------------------------------------------------------------------------------
import           Language.Haskell.TH.Syntax


------------------------------------------------------------------------------
-- | XXX

loadSnapTH :: Name -> Name -> Q Exp
loadSnapTH init action = do
    let initE = VarE init
        actE = VarE action
        fmapE = VarE 'fmap
        simpleLoad = foldl AppE fmapE [actE, initE]
    return simpleLoad
