-- | The Snap.Snaplet.Test module contains primitives and combinators for
-- testing Snaplets.
module Snap.Snaplet.Test
  (
    runHandler
  )
  where

import           Control.Category
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.Text
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Internal.Types
import           Snap.Test hiding (get, runHandler)
import qualified Snap.Test as ST
import           Snap.Snaplet.Internal.Initializer


-- Then what you do is use the initializer (probably a SnapletInit) to construct the b.
-- From there, it should be pretty simple to connect runBase and runHandler from Snap.Test

------------------------------------------------------------------------------
runHandler :: RequestBuilder IO ()
           -> Handler b b a
           -> SnapletInit b b
           -> IO (Either Text Response)
runHandler rq h (SnapletInit init) = do
        app <- getSnaplet init
        case app of
            (Left e) -> return $ Left e
            (Right (a,_)) -> do
                res <- ST.runHandler rq $ runPureBase h a
                return $ Right res


getSnaplet :: Initializer b b (Snaplet b)
           -> IO (Either Text (Snaplet b, InitializerState b))
getSnaplet init = do
        mvar <- newEmptyMVar
        runInitializer mvar "" init

