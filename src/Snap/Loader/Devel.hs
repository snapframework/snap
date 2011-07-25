{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module includes the machinery necessary to use hint to load
-- action code dynamically.  It includes a Template Haskell function
-- to gather the necessary compile-time information about code
-- location, compiler arguments, etc, and bind that information into
-- the calls to the dynamic loader.
module Snap.Loader.Devel
  ( loadSnapTH
  , loadSnapTH'
  ) where

import           Control.Monad (liftM2)

import           Data.List
import           Data.Maybe (catMaybes)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)

import           Language.Haskell.Interpreter hiding (lift, liftIO)
import           Language.Haskell.Interpreter.Unsafe

import           Language.Haskell.TH

import           System.Environment (getArgs)

------------------------------------------------------------------------------
import           Snap.Types
import           Snap.Loader.Devel.Signal
import           Snap.Loader.Devel.Evaluator
import           Snap.Loader.Devel.TreeWatcher

------------------------------------------------------------------------------
-- | This function derives all the information necessary to use the
-- interpreter from the compile-time environment, and compiles it in
-- to the generated code.
--
-- This could be considered a TH wrapper around a function
--
-- > loadSnap :: Initializer s -> SnapExtend s () -> [String] -> IO (Snap ())
--
-- with a magical implementation.
--
-- The upshot is that you shouldn't need to recompile your server
-- during development unless your .cabal file changes, or the code
-- that uses this splice changes.
loadSnapTH :: Name -> Name -> [String] -> Q Exp
loadSnapTH initializer action additionalWatchDirs =
    loadSnapTH' modules imports additionalWatchDirs loadStr
  where
    initMod = nameModule initializer
    initBase = nameBase initializer
    actMod = nameModule action
    actBase = nameBase action

    modules = catMaybes [initMod, actMod]
    imports = ["Snap.Snaplet"]

    loadStr = intercalate " " [ "runInitializerWithoutReloadAction"
                              , initBase
                              , actBase
                              ]


------------------------------------------------------------------------------
-- | This is the backing implementation for 'loadSnapTH'.  This
-- interface can be used when the types involved don't include a
-- SnapExtend and an Initializer.
loadSnapTH' :: [String] -- ^ the list of modules to interpret
            -> [String] -- ^ the list of modules to import in addition
                        -- to those being interpreted
            -> [String] -- ^ additional directories to watch for
                        -- changes to trigger to recompile/reload
            -> String   -- ^ the expression to interpret in the
                        -- context of the loaded modules and imports.
                        -- It should have the type 'HintLoadable'
            -> Q Exp
loadSnapTH' modules imports additionalWatchDirs loadStr = do
    args <- runIO getArgs

    let opts = getHintOpts args
        srcPaths = additionalWatchDirs ++ getSrcPaths args

    [| hintSnap opts modules imports srcPaths loadStr |]


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

    toCopy = filter (not . isSuffixOf ".hs") $
             dropWhile (not . ("-package" `isPrefixOf`)) args
    copy = map (intercalate " ") . groupBy (\_ s -> not $ "-" `isPrefixOf` s)

    opts = hideAll ++ srcOpts ++ copy toCopy


------------------------------------------------------------------------------
-- | This function extracts the source paths from the compilation args
getSrcPaths :: [String] -> [String]
getSrcPaths = filter (not . null) . map (drop 2) . filter srcArg
  where
    srcArg x = "-i" `isPrefixOf` x && not ("-idist" `isPrefixOf` x)


------------------------------------------------------------------------------
-- | This function creates the Snap handler that actually is
-- responsible for doing the dynamic loading of actions via hint,
-- given all of the configuration information that the interpreter
-- needs.  It also ensures safe concurrent access to the interpreter,
-- and caches the interpreter results for a short time before allowing
-- it to run again.
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
         -> [String] -- ^ A list of modules that need to be be
                     -- imported, in addition to the ones that need to
                     -- be interpreted.  This only needs to contain
                     -- modules that aren't being interpreted, such as
                     -- those from other libraries, that are used in
                     -- the expression passed in.
         -> [String] -- ^ A list of paths to watch for updates
         -> String   -- ^ The string to execute
         -> IO (Snap (), IO ())
hintSnap opts modules imports srcPaths action =
    protectedHintEvaluator initialize test loader
  where
    interpreter = do
        loadModules . nub $ modules
        setImports . nub $ "Prelude" : "Snap.Types" : imports ++ modules

        interpret action (as :: HintLoadable)

    loadInterpreter = unsafeRunInterpreterWithArgs opts interpreter

    formatOnError (Left err) = error $ format err
    formatOnError (Right a) = a

    loader = formatOnError `fmap` protectHandlers loadInterpreter

    initialize = liftM2 (,) getCurrentTime $ getTreeStatus srcPaths

    test (prevTime, ts) = do
        now <- getCurrentTime
        if diffUTCTime now prevTime < 3
            then return True
            else checkTreeStatus ts


------------------------------------------------------------------------------
-- | Convert an InterpreterError to a String for presentation
format :: InterpreterError -> String
format (UnknownError e)   = "Unknown interpreter error:\r\n\r\n" ++ e
format (NotAllowed e)     = "Interpreter action not allowed:\r\n\r\n" ++ e
format (GhcException e)   = "GHC error:\r\n\r\n" ++ e
format (WontCompile errs) = "Compile errors:\r\n\r\n" ++
    (intercalate "\r\n" $ nub $ map errMsg errs)

