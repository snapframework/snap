{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Loader.Devel.Evaluator
  ( HintLoadable
  , protectedHintEvaluator
  ) where


import Control.Exception
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar

import Prelude hiding (catch, init, any)

import Snap.Core (Snap)


------------------------------------------------------------------------------
-- | A type synonym to simply talking about the type loaded by hint.
type HintLoadable = IO (Snap (), IO ())


------------------------------------------------------------------------------
-- | Convert an action to generate 'HintLoadable's into an action to
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
-- before the recompile condition is reached.
protectedHintEvaluator :: forall a.
                          IO a
                       -> (a -> IO Bool)
                       -> IO HintLoadable
                       -> HintLoadable
protectedHintEvaluator start test getInternals = do
    -- The list of requesters waiting for a result.  Contains the
    -- ThreadId in case of exceptions, and an empty MVar awaiting a
    -- successful result.
    readerContainer <- newReaderContainer

    -- Contains the previous result and initialization value, and the
    -- time it was stored, if a previous result has been computed.
    -- The result stored is either the actual result and
    -- initialization result, or the exception thrown by the
    -- calculation.
    resultContainer <- newResultContainer

    -- The model used for the above MVars in the returned action is
    -- "keep them full, unless updating them."  In every case, when
    -- one of those MVars is emptied, the next action is to fill that
    -- same MVar.  This makes deadlocking on MVar wait impossible.
    let snap = do
            let waitForNewResult :: IO (Snap ())
                waitForNewResult = do
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
                                stateInitializer <- unblock getInternals
                                res <- unblock stateInitializer

                                let a = fst res

                                clearAndNotify (Right res) (flip putMVar a . snd)

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

            getResult <- liftIO $ case existingResult of
                Just (res, a) -> do
                    -- There's an existing result.  Check for validity
                    valid <- test a
                    case (valid, res) of
                        (True, Right (x, _)) -> return x
                        (True, Left e)       -> throwIO e
                        (False, _)           -> waitForNewResult
                Nothing -> waitForNewResult
            getResult

        clean = do
             let msg = "invalid dynamic loader state.  " ++
                       "The cleanup action has been executed"
             contents <- swapMVar resultContainer $ error msg
             cleanup contents

    return (snap, clean)
  where
    newReaderContainer :: IO (MVar [(ThreadId, MVar (Snap ()))])
    newReaderContainer = newMVar []

    newResultContainer :: IO (MVar (Maybe (Either SomeException
                                                  (Snap (), IO ()), a)))
    newResultContainer = newMVar Nothing

    cleanup (Just (Right (_, clean), _)) = clean
    cleanup _                            = return ()
