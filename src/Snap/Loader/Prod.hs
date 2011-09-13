{-# LANGUAGE TemplateHaskell #-}
module Snap.Loader.Prod
  ( loadSnapTH
  ) where

import           Language.Haskell.TH


------------------------------------------------------------------------------
-- | This function provides a non-magical type-compatible loader for
-- the one in Snap.Loader.Devel, allowing switching one import to
-- provide production-mode compilation.
--
-- This could be considered a TH wrapper around a function
--
-- > loadSnap :: Typeable a => IO a -> (a -> IO (Snap (), IO ())) -> IO (a, Snap (), IO ())
loadSnapTH :: Name -> Name -> [String] -> [String] -> Q Exp
loadSnapTH initializer action _additionalImports _additionalWatchDirs =
    [| do value <- $(varE initializer)
          (site, conf) <- $(varE action) value
          return (value, site, conf) |]
