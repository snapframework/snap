{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Loader.Hint where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as S

import           Data.List (nub)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)

import           Data.Maybe (catMaybes)
import           Data.Time.Clock

import           Language.Haskell.Interpreter hiding (lift, liftIO)
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)

import           Language.Haskell.TH.Syntax

import           System.Directory (getCurrentDirectory)

------------------------------------------------------------------------------
import           Snap.Types



------------------------------------------------------------------------------
-- | XXX
-- Assumes being spliced into the same source tree as the action to
-- dynamically load is located in
-- Assumes mtl is the only package installed with a conflicting
-- Control.Monad.Trans
loadSnapTH :: Name -> Name -> Q Exp
loadSnapTH initialize action = do
    loc <- location
    cwd <- runIO getCurrentDirectory

    let initMod = nameModule initialize
        initBase = nameBase initialize
        actMod = nameModule action
        actBase = nameBase action

        lf = length . loc_filename $ loc
        lm = length . loc_module $ loc
        src = if lf > lm + 4
              then take (lf - (lm + 4)) $ loc_filename loc
              else "."
        str = "liftIO " ++ initBase ++ " >>= " ++ actBase
        modules = catMaybes [initMod, actMod]
        opts = [ "-hide-package=mtl" ] :: [String]

        hintSnapE = VarE 'hintSnap

    optsE <- lift opts
    srcE <- lift src
    modulesE <- lift modules
    strE <- lift str

    return $ foldl AppE hintSnapE [optsE, srcE, modulesE, strE]


------------------------------------------------------------------------------
-- | XXX
hintSnap :: [String] -> String -> [String] -> String -> IO (Snap ())
hintSnap opts sPath mNames action = do
    let interpreter = do
        mapM_ unsafeSetGhcOption opts
        set [ searchPath := [sPath] ]
        loadModules . nub $ mNames
        let allMods = "Prelude":"Snap.Types":"Control.Monad.Trans":mNames
        setImports . nub $ allMods
        interpret action (as :: Snap ())

    loadAction <- protectedActionEvaluator 3 $ runInterpreter interpreter

    return $ do
        eSnap <- liftIO loadAction
        case eSnap of
            Left err -> do
                let msg = format err
                    len = fromIntegral $ S.length msg
                modifyResponse $ setContentType "text/plain; charset=utf-8"
                               . setResponseStatus 500 "Internal Server Error"
                               . setContentLength len
                writeBS msg

            Right handler -> handler


------------------------------------------------------------------------------
-- |
format :: InterpreterError -> S.ByteString
format (UnknownError e)   =
    S.append "Unknown interpreter error:\r\n\r\n" $ S.pack e

format (NotAllowed e)     =
    S.append "Interpreter action not allowed:\r\n\r\n" $ S.pack e

format (GhcException e)   =
    S.append "GHC error:\r\n\r\n" $ S.pack e

format (WontCompile errs) =
    let formatted = S.intercalate "\r\n" . map S.pack . nub . map errMsg $ errs
    in S.append "Compile errors:\r\n\r\n" formatted


------------------------------------------------------------------------------
-- | XXX
protectedActionEvaluator :: NominalDiffTime -> IO a -> IO (IO a)
protectedActionEvaluator minReEval action = do
    readerContainer <- newMVar []
    resultContainer <- newMVar Nothing
    return $ do
        existingResult <- readMVar resultContainer
        now <- getCurrentTime

        case existingResult of
            Just (val, ts) | diffUTCTime now ts < minReEval -> return val
            _ -> do
                reader <- newEmptyMVar
                readers <- takeMVar readerContainer

                when (null readers) $ do
                    forkIO $ do
                        result <- action
                        allReaders <- takeMVar readerContainer
                        finishTime <- getCurrentTime
                        swapMVar resultContainer $ Just (result, finishTime)
                        putMVar readerContainer []
                        mapM_ (flip putMVar result) allReaders
                    return ()

                putMVar readerContainer $ reader : readers
                takeMVar reader
