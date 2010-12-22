{-# LANGUAGE DeriveDataTypeable #-}

module Snap.Extension.Loader.Devel.Evaluator
  ( HintInternals
  , makeHintInternals
  , protectedHintEvaluator
  ) where


import Control.Exception
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.MVar

import Data.Typeable (Typeable)

import GHC.Prim (Any)

import Prelude hiding (catch, init, any)

import Snap.Types (Snap)

import Unsafe.Coerce (unsafeCoerce)


------------------------------------------------------------------------------
-- | A monomorphic type to hide polymorphism.  This allows Hint to
-- load the action, since it requires loading a monomorphic type.  The
-- constructor for this is not exposed because its internals are
-- incredibly far from type safe.
data HintInternals = HintInternals
    { hiInit :: IO Any
    , hiClean :: Any -> IO ()
    , hiExec :: Any -> Snap ()
    } deriving Typeable


------------------------------------------------------------------------------
-- | A smart constructor to hide the incredibly type unsafe internals
-- of HintInternals behind a type safe smart constructor.
makeHintInternals :: IO a -> (a -> IO ()) -> (a -> Snap ()) -> HintInternals
makeHintInternals init clean exec = HintInternals init' clean' exec'
  where
    init' = fmap unsafeCoerce init
    clean' = clean . unsafeCoerce
    exec' = exec . unsafeCoerce


------------------------------------------------------------------------------
-- | Convert an action to generate HintInternals into an action to
-- generate Snap actions.  The resulting action will share initialized
-- state until the next execution of the input action.  At this time,
-- the cleanup action will be executed.
--
-- The first two arguments control when recompiles are done.  The
-- first argument is an action that is executed when compilation
-- starts.  The second is a function from the result of the first
-- action to an action that determines whether the value from the
-- previous compilation is still good.  This abstracts out the
-- strategy for determining when a cached result is no longer valid.
--
-- If an exception is raised during the processing of the action, it
-- will be thrown to all waiting threads, and for all requests made
-- before the delay time has expired after the exception was raised.
protectedHintEvaluator :: IO a
                       -> (a -> IO Bool)
                       -> IO HintInternals
                       -> IO (Snap ())
protectedHintEvaluator start test getInternals = do
    -- The list of requesters waiting for a result.  Contains the
    -- ThreadId in case of exceptions, and an empty MVar awaiting a
    -- successful result.
    --
    -- type: MVar [(ThreadId, MVar (Snap ()))]
    readerContainer <- newMVar []

    -- Contains the previous result and initialization value, and the
    -- time it was stored, if a previous result has been computed.
    -- The result stored is either the actual result and
    -- initialization result, or the exception thrown by the
    -- calculation.
    --
    -- type: MVar (Maybe (Either SomeException (HintInternals, Any), a))
    resultContainer <- newMVar Nothing

    -- The model used for the above MVars in the returned action is
    -- "keep them full, unless updating them."  In every case, when
    -- one of those MVars is emptied, the next action is to fill that
    -- same MVar.  This makes deadlocking on MVar wait impossible.
    return $ do
        let waitForNewResult = do
                -- Need to calculate a new result
                tid <- myThreadId
                reader <- newEmptyMVar

                readers <- takeMVar readerContainer

                -- Some strictness is employed to ensure the MVar
                -- isn't holding on to a chain of unevaluated thunks.
                let pair = (tid, reader)
                    newReaders = readers `seq` pair `seq` (pair : readers)
                putMVar readerContainer $! newReaders

                -- If this is the first reader to queue, clean up the
                -- previous state, if there was any, and then begin
                -- evaluation of the new code and state.
                when (null readers) $ do
                    let runAndFill = block $ do
                            -- run the cleanup action
                            previous <- readMVar resultContainer
                            unblock $ cleanup previous

                            -- compile the new internals and initialize
                            hi <- unblock getInternals
                            any <- unblock $ hiInit hi

                            let a = (hi, any)
                            clearAndNotify (Right a) (flip putMVar a . snd)

                        killWaiting :: SomeException -> IO ()
                        killWaiting e = block $ do
                            clearAndNotify (Left e) (flip throwTo e . fst)
                            throwIO e

                        clearAndNotify r f = do
                            a <- unblock start
                            _ <- swapMVar resultContainer $ Just (r, a)
                            allReaders <- swapMVar readerContainer []
                            mapM_ f allReaders

                    _ <- forkIO $ runAndFill `catch` killWaiting
                    return ()

                -- Wait for the evaluation of the action to complete,
                -- and return its result.
                takeMVar reader

        existingResult <- liftIO $ readMVar resultContainer

        (hi, any) <- liftIO $ case existingResult of
            Just (res, a) -> do
                -- There's an existing result.  Check for validity
                valid <- test a
                case (valid, res) of
                    (True, Right x) -> return x
                    (True, Left  e) -> throwIO e
                    (False, _)      -> do
                        _ <- swapMVar resultContainer Nothing
                        waitForNewResult
            Nothing -> waitForNewResult
        hiExec hi any
  where
    cleanup (Just (Right (hi, any), _)) = hiClean hi any
    cleanup _                           = return ()
