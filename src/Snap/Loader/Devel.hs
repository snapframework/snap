------------------------------------------------------------------------------
-- | This module is a compatibility shim for snap-dynamic that causes
-- compilation to fail with an indication that snap-dynamic was not
-- installed
module Snap.Loader.Devel
  ( loadSnapTH
  ) where

import Language.Haskell.TH (Exp, Name, Q)

------------------------------------------------------------------------------
-- | This shim does nothing except cause compilation to fail
loadSnapTH :: Q Exp -> Name -> [String] -> Q Exp
loadSnapTH _ _ _ = fail . concat $
                   [ "Snap was built without dynamic loading support. "
                   , "Dynamic loading support is necessary for development "
                   , "mode.  Please reinstall snap with dynamic loading "
                   , "support.\n\n  cabal install snap -fdynamic\n\n"
                   ]
