-- | The Snap.Snaplet.Test module contains primitives and combinators for
-- testing Snaplets.
module Snap.Snaplet.Test
  (

   -- ** Types
    SnapletEnvironment

   -- * HUnit Assertions
  , assertInitSuccess
  )
  where

import           Snap.Snaplet.Internal.Test.Types
import           Snap.Snaplet.Internal.Test.Assertions
