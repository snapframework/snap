{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module includes the machinery necessary to use hint to load
-- action code dynamically.  It includes a Template Haskell function
-- to gather the necessary compile-time information about code
-- location, compiler arguments, etc, and bind that information into
-- the calls to the dynamic loader.
module Snap.Loader.Hint where

import           Data.List (groupBy, intercalate, isPrefixOf, nub)

import           Control.Concurrent (forkIO, myThreadId)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)

import           Data.Maybe (catMaybes)
import           Data.Time.Clock

import           Language.Haskell.Interpreter hiding (lift, liftIO)
import           Language.Haskell.Interpreter.Unsafe

import           Language.Haskell.TH

import           Prelude hiding (catch)

import           System.Environment (getArgs)

------------------------------------------------------------------------------
import           Snap.Types
import qualified Snap.Loader.Static as Static

------------------------------------------------------------------------------
-- | This function derives all the information necessary to use the
-- interpreter from the compile-time environment, and compiles it in
-- to the generated code.
--
-- This could be considered a TH wrapper around a function
--
-- > loadSnap :: IO a -> (a -> IO ()) -> (a -> Snap ()) -> IO (IO (), Snap ())
--
-- with a magical implementation.
--
-- The returned IO action does nothing.  The returned Snap action does
-- initialization, runs the action, and does the cleanup.  This means
-- that the whole application state will be loaded and unloaded for
-- each request.  To make this worthwhile, those steps should be made
-- quite fast.
--
-- The upshot is that you shouldn't need to recompile your server
-- during development unless your .cabal file changes, or the code
-- that uses this splice changes.
loadSnapTH :: Name -> Name -> Name -> Q Exp
loadSnapTH initialize cleanup action = do
    args <- runIO getArgs

    let initMod = nameModule initialize
        initBase = nameBase initialize
        cleanMod = nameModule cleanup
        cleanBase = nameBase cleanup
        actMod = nameModule action
        actBase = nameBase action

        modules = catMaybes [initMod, cleanMod, actMod]
        opts = getHintOpts args

    let static = Static.loadSnapTH initialize cleanup action

    -- The let in this block causes the static expression to be
    -- pattern-matched, providing an extra check that the types were
    -- correct at compile-time, at least.
    [| do let _ = $static :: IO (IO (), Snap ())
          hint <- hintSnap opts modules initBase cleanBase actBase
          return (return (), hint) |]


------------------------------------------------------------------------------
-- | Convert the command-line arguments passed in to options for the
-- hint interpreter.  This is somewhat brittle code, based on a few
-- experimental datapoints regarding the structure of the command-line
-- arguments cabal produces.
getHintOpts :: [String] -> [String]
getHintOpts args = removeBad opts
  where
    bad = ["-threaded", "-O"]
    removeBad = filter (\x -> not $ any (`isPrefixOf` x) bad)

    hideAll = filter (== "-hide-all-packages") args

    srcOpts = filter (\x -> "-i" `isPrefixOf` x
                            && not ("-idist" `isPrefixOf` x)) args

    toCopy = init' $ dropWhile (not . ("-package" `isPrefixOf`)) args
    copy = map (intercalate " ") . groupBy (\_ s -> not $ "-" `isPrefixOf` s)

    opts = hideAll ++ srcOpts ++ copy toCopy

    init' [] = []
    init' xs = init xs


------------------------------------------------------------------------------
-- | This function creates the Snap handler that actually is
-- responsible for doing the dynamic loading of actions via hint,
-- given all of the configuration information that the interpreter
-- needs.  It also ensures safe concurrent access to the interpreter,
-- and caches the interpreter results for a short time before allowing
-- it to run again.
--
-- This constructs an expression of type Snap (), that is essentially
--
-- > bracketSnap initialization cleanup handler
--
-- for the values of initialization, cleanup, and handler passed in.
--
-- Generally, this won't be called manually.  Instead, loadSnapTH will
-- generate a call to it at compile-time, calculating all the
-- arguments from its environment.
hintSnap :: [String] -- ^ A list of command-line options for the interpreter
         -> [String] -- ^ A list of modules that need to be
                     -- interpreted. This should contain only the
                     -- modules which contain the initialization,
                     -- cleanup, and handler actions.  Everything else
                     -- they require will be loaded transitively.
         -> String   -- ^ The name of the initialization action
         -> String   -- ^ The name of the cleanup action
         -> String   -- ^ The name of the handler action
         -> IO (Snap ())
hintSnap opts modules initialization cleanup handler = do
    let action = intercalate " " [ "bracketSnap"
                                 , initialization
                                 , cleanup
                                 , handler
                                 ]
        interpreter = do
            loadModules . nub $ modules
            let imports = "Prelude" : "Snap.Types" : modules
            setImports . nub $ imports

            interpret action (as :: Snap ())

        loadInterpreter = unsafeRunInterpreterWithArgs opts interpreter

    -- Protect the interpreter from concurrent and high-speed serial
    -- access.
    loadAction <- protectedActionEvaluator 3 loadInterpreter

    return $ do
        interpreterResult <- liftIO loadAction
        case interpreterResult of
            Left err -> error $ format err
            Right handlerAction -> handlerAction


------------------------------------------------------------------------------
-- | Convert an InterpreterError to a String for presentation
format :: InterpreterError -> String
format (UnknownError e)   = "Unknown interpreter error:\r\n\r\n" ++ e
format (NotAllowed e)     = "Interpreter action not allowed:\r\n\r\n" ++ e
format (GhcException e)   = "GHC error:\r\n\r\n" ++ e
format (WontCompile errs) = "Compile errors:\r\n\r\n" ++
    (intercalate "\r\n" $ nub $ map errMsg errs)


------------------------------------------------------------------------------
-- | Create a wrapper for an action that protects the action from
-- concurrent or rapid evaluation.
--
-- There will be at least the passed-in 'NominalDiffTime' delay
-- between the finish of one execution of the action the start of the
-- next.  Concurrent calls to the wrapper, and calls within the delay
-- period, end up with the same calculated value returned.
--
-- If an exception is raised during the processing of the action, it
-- will be thrown to all waiting threads, and for all requests made
-- before the delay time has expired after the exception was raised.
protectedActionEvaluator :: NominalDiffTime -> IO a -> IO (IO a)
protectedActionEvaluator minReEval action = do
    -- The list of requesters waiting for a result.  Contains the
    -- ThreadId in case of exceptions, and an empty MVar awaiting a
    -- successful result.
    --
    -- type: MVar [(ThreadId, MVar a)]
    readerContainer <- newMVar []

    -- Contains the previous result, and the time it was stored, if a
    -- previous result has been computed.  The result stored is either
    -- the actual result, or the exception thrown by the calculation.
    --
    -- type: MVar (Maybe (Either SomeException a, UTCTime))
    resultContainer <- newMVar Nothing

    -- The model used for the above MVars in the returned action is
    -- "keep them full, unless updating them."  In every case, when
    -- one of those MVars is emptied, the next action is to fill that
    -- same MVar.  This makes deadlocking on MVar wait impossible.
    return $ do
        existingResult <- readMVar resultContainer
        now <- getCurrentTime

        case existingResult of
            Just (res, ts) | diffUTCTime now ts < minReEval ->
                -- There's an existing result, and it's still valid
                case res of
                    Right val -> return val
                    Left  e   -> throwIO e
            _ -> do
                -- Need to calculate a new result
                tid <- myThreadId
                reader <- newEmptyMVar

                readers <- takeMVar readerContainer

                -- Some strictness is employed to ensure the MVar
                -- isn't holding on to a chain of unevaluated thunks.
                let pair = (tid, reader)
                    newReaders = readers `seq` pair `seq` (pair : readers)
                putMVar readerContainer $! newReaders

                -- If this is the first reader, kick off evaluation of
                -- the action in a new thread. This is slightly
                -- careful to block asynchronous exceptions to that
                -- thread except when actually running the action.
                when (null readers) $ do
                    let runAndFill = block $ do
                            a <- unblock action
                            clearAndNotify (Right a) (flip putMVar a . snd)

                        killWaiting :: SomeException -> IO ()
                        killWaiting e = block $ do
                            clearAndNotify (Left e) (flip throwTo e . fst)
                            throwIO e

                        clearAndNotify r f = do
                            t <- getCurrentTime
                            _ <- swapMVar resultContainer $ Just (r, t)
                            allReaders <- swapMVar readerContainer []
                            mapM_ f allReaders

                    _ <- forkIO $ runAndFill `catch` killWaiting
                    return ()

                -- Wait for the evaluation of the action to complete,
                -- and return its result.
                takeMVar reader
