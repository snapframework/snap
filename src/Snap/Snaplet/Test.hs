-- | The Snap.Snaplet.Test module contains primitives and combinators for
-- testing Snaplets.
module Snap.Snaplet.Test
  (
    -- ** Testing handlers
    evalHandler
  , runHandler
  , withTemporaryFile
  )
  where


------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import           Control.Exception.Base (finally)
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Data.Text
import           System.Directory
import           System.IO.Error


------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Internal.Types
import           Snap.Test hiding (evalHandler, runHandler)
import qualified Snap.Test as ST
import           Snap.Snaplet.Internal.Initializer


------------------------------------------------------------------------------
-- | Remove the given file before running an IO computation. Obviously it
-- can be used with 'Assertion'.
withTemporaryFile :: FilePath -> IO () -> IO ()
withTemporaryFile f = finally (removeFileMayNotExist f)


------------------------------------------------------------------------------
-- | Utility function taken from Darcs
removeFileMayNotExist :: FilePath -> IO ()
removeFileMayNotExist f = catchNonExistence (removeFile f) ()
  where
    catchNonExistence :: IO a -> a -> IO a
    catchNonExistence job nonexistval =
        E.catch job $
        \e -> if isDoesNotExistError e then return nonexistval
                                      else ioError e


------------------------------------------------------------------------------
-- | Given a Snaplet Handler and a 'RequestBuilder' defining
-- a test request, runs the Handler, producing an HTTP 'Response'.
--
-- Note that the output of this function is slightly different from
-- 'runHandler' defined in Snap.Test, because due to the fact running
-- the initializer inside 'SnapletInit' can throw an exception.
runHandler :: MonadIO m
           => RequestBuilder m ()
           -> Handler b b a
           -> SnapletInit b b
           -> m (Either Text Response)
runHandler rq h s = do
        app <- getSnaplet s
        case app of
          (Left e) -> return $ Left e
          (Right (a,_)) -> do
              res <- ST.runHandler rq $ runPureBase h a
              return $ Right res


------------------------------------------------------------------------------
-- | Given a Snaplet Handler, a 'SnapletInit' specifying the initial state,
--  and a 'RequestBuilder' defining a test request, runs the handler,
--  returning the monadic value it produces.
--
-- Throws an exception if the 'Snap' handler early-terminates with 'finishWith'
-- or 'mzero'.
--
-- Note that the output of this function is slightly different from
-- 'evalHandler defined in Snap.Test, because due to the fact running
-- the initializer inside 'SnapletInit' can throw an exception.
evalHandler :: MonadIO m
            => RequestBuilder m ()
            -> Handler b b a
            -> SnapletInit b b
            -> m (Either Text a)
evalHandler rq h s = do
    app <- getSnaplet s
    case app of
      (Left e) -> return $ Left e
      (Right (a,_)) -> do
          res <- ST.evalHandler rq $ runPureBase h a
          return $ Right res


------------------------------------------------------------------------------
-- | Run the given initializer, yielding a tuple where the first element is
-- a @Snaplet b@, or an error message whether the initializer threw an
-- exception.                                                       
getSnaplet :: MonadIO m
           => SnapletInit b b
           -> m (Either Text (Snaplet b, InitializerState b))
getSnaplet (SnapletInit initializer) = liftIO $ do
    mvar <- newEmptyMVar
    let resetter f = modifyMVar_ mvar (return . f)
    runInitializer resetter "" initializer

