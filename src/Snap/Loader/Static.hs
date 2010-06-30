{-# LANGUAGE TemplateHaskell #-}
-- | This module is a source-compatible replacement for
-- "Snap.Loader.Hint".  It does not import the GHC api, directly or
-- transitively, resulting in a greatly decreased binary size, as
-- compared to the Hint loader.
--
-- This module results in the same code as using the actions in the
-- obvious, straight-forward manner.  It is present *only* as a
-- source-level replacement for the Hint loader, to enable quickly
-- switching the Hint loader off with only a changed import.
module Snap.Loader.Static where

import           Control.Arrow
import           Language.Haskell.TH.Syntax


------------------------------------------------------------------------------
-- | This function is a shim for source compatibility with loadSnapTH
-- in "Snap.Loader.Hint".  This function is a TH wrapper around a
-- hypothetical function:
--
-- > loadSnap :: IO a -> (a -> IO ()) -> (a -> Snap ()) -> IO (IO (), Snap ())
-- > loadSnap initialize cleanup action = do
-- >     i <- initialize
-- >     return (cleanup i, action i)
loadSnapTH :: Name -> Name -> Name -> Q Exp
loadSnapTH initialize cleanup action = do
    funE <- [| \c a -> fmap (c &&& a) |]

    let [initE, cleanE, actE] = map VarE [initialize, cleanup, action]
        simpleLoad = foldl AppE funE [cleanE, actE, initE]

    return simpleLoad
