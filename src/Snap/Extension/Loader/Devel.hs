{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module includes the machinery necessary to use hint to load
-- action code dynamically.  It includes a Template Haskell function
-- to gather the necessary compile-time information about code
-- location, compiler arguments, etc, and bind that information into
-- the calls to the dynamic loader.
module Snap.Extension.Loader.Devel
  ( loadSnapTH
  ) where

import           Control.Monad (join)

import           Data.List (groupBy, intercalate, isPrefixOf, nub)
import           Data.Maybe (catMaybes)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)

import           Language.Haskell.Interpreter hiding (lift, liftIO)
import           Language.Haskell.Interpreter.Unsafe

import           Language.Haskell.TH

import           System.Environment (getArgs)

------------------------------------------------------------------------------
import           Snap.Types
import           Snap.Extension (getHintInternals)
import           Snap.Extension.Loader.Devel.Signal
import           Snap.Extension.Loader.Devel.Evaluator

------------------------------------------------------------------------------
-- | This function derives all the information necessary to use the
-- interpreter from the compile-time environment, and compiles it in
-- to the generated code.
--
-- This could be considered a TH wrapper around a function
--
-- > loadSnap :: Initializer s -> SnapExtend s () -> IO (Snap ())
--
-- with a magical implementation.
--
-- The returned Snap action runs the 'Initializer', runs the 'Snap' handler,
-- and does the cleanup.  This means that the whole application state will be
-- loaded and unloaded for each request.  To make this worthwhile, those steps
-- should be made quite fast.
--
-- The upshot is that you shouldn't need to recompile your server
-- during development unless your .cabal file changes, or the code
-- that uses this splice changes.
loadSnapTH :: Name -> Name -> Q Exp
loadSnapTH initializer action = do
    args <- runIO getArgs

    let initMod = nameModule initializer
        initBase = nameBase initializer
        actMod = nameModule action
        actBase = nameBase action

        modules = catMaybes [initMod, actMod]
        opts = getHintOpts args

    -- The let in this block causes an extra static type check that the
    -- types of the names passed in were correct at compile time.
    [| let _ = getHintInternals $(varE initializer) $(varE action)
       in hintSnap opts modules initBase actBase |]


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
         -> String   -- ^ The name of the initializer action
         -> String   -- ^ The name of the SnapExtend action
         -> IO (Snap ())
hintSnap opts modules initialization handler = do
    let action = intercalate " " [ "getHintInternals"
                                 , initialization
                                 , handler
                                 ]
        interpreter = do
            loadModules . nub $ modules
            let imports = "Prelude" : "Snap.Extension" :
                          "Snap.Extension.Loader.Devel.Evaluator" :
                          modules
            setImports . nub $ imports

            interpret action (as :: IO HintInternals)

        loadInterpreter = unsafeRunInterpreterWithArgs opts interpreter

        formatError (Left err) = error $ format err
        formatError (Right a) = a

        loader = join $ formatError `fmap` protectHandlers loadInterpreter

        test prevTime = do
            now <- getCurrentTime
            return $ diffUTCTime now prevTime < 4

    protectedHintEvaluator getCurrentTime test loader


------------------------------------------------------------------------------
-- | Convert an InterpreterError to a String for presentation
format :: InterpreterError -> String
format (UnknownError e)   = "Unknown interpreter error:\r\n\r\n" ++ e
format (NotAllowed e)     = "Interpreter action not allowed:\r\n\r\n" ++ e
format (GhcException e)   = "GHC error:\r\n\r\n" ++ e
format (WontCompile errs) = "Compile errors:\r\n\r\n" ++
    (intercalate "\r\n" $ nub $ map errMsg errs)

