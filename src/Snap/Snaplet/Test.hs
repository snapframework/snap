-- | The Snap.Snaplet.Test module contains primitives and combinators for
-- testing Snaplets.
module Snap.Snaplet.Test
  (
    -- ** Testing handlers
    runHandler
  )
  where


import           Control.Concurrent.MVar
import           Data.Text
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Internal.Types
import           Snap.Test hiding (runHandler)
import qualified Snap.Test as ST
import           Snap.Snaplet.Internal.Initializer


------------------------------------------------------------------------------
-- | Given a Snaplet Handler and a 'RequestBuilder' defining
-- a test request, runs the Handler, producing an HTTP 'Response'.
--
-- Note that the output of this function is slightly different from
-- 'runHandler' defined in Snap.Test, because due to the fact an
-- initializer can throw an exception.
runHandler :: RequestBuilder IO ()
           -> Handler b b a
           -> SnapletInit b b
           -> IO (Either Text Response)
runHandler rq h s = do
        app <- getSnaplet s
        case app of
            (Left e) -> return $ Left e
            (Right (a,_)) -> do
                res <- ST.runHandler rq $ runPureBase h a
                return $ Right res


------------------------------------------------------------------------------
-- | Run the given initializer, yielding a tuple where the first element is
-- a @Snaplet b@, or an error message whether the initializer threw an
-- exception.                                                       
getSnaplet :: SnapletInit b b
           -> IO (Either Text (Snaplet b, InitializerState b))
getSnaplet (SnapletInit initializer) = do
        mvar <- newEmptyMVar
        runInitializer mvar "" initializer

