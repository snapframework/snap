-- | The Snap.Snaplet.Test module contains primitives and combinators for
-- testing Snaplets.
module Snap.Snaplet.Test
  (
    runHandler
  )
  where


import           Control.Monad.IO.Class
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Internal.Types
import           Snap.Test hiding (runHandler)
import qualified Snap.Test as ST


-- Then what you do is use the initializer (probably a SnapletInit) to construct the b.
-- From there, it should be pretty simple to connect runBase and runHandler from Snap.Test

------------------------------------------------------------------------------
runHandler :: MonadIO m =>
             RequestBuilder m ()
           -> Handler b b a
           -> m Response
runHandler rq h = ST.runHandler rq $ runPureBase h undefined
