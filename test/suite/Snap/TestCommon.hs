{-# LANGUAGE ScopedTypeVariables #-}

module Snap.TestCommon where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_, when)
import Data.Maybe
import Data.Monoid
import Prelude hiding (catch)
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.Process hiding (cwd)

import SafeCWD


testGeneratedProject :: String  -- ^ project name and directory
                     -> String  -- ^ arguments to @snap init@
                     -> String  -- ^ arguments to @cabal install@
                     -> Int     -- ^ port to run http server on
                     -> IO ()   -- ^ action to run when the server goes up
                     -> IO ()
testGeneratedProject projName snapInitArgs cabalInstallArgs httpPort
                     testAction = do
    cwd <- getCurrentDirectory
    let segments = reverse $ splitPath cwd
        projectPath = cwd </> "test-snap-exe" </> projName
        snapRoot = joinPath $ reverse $ drop 1 segments
        snapRepos = joinPath $ reverse $ drop 2 segments

        sandbox = cwd </> "test-cabal-dev"

        cabalDevArgs = "-s " ++ sandbox

        args = cabalDevArgs ++ " " ++ cabalInstallArgs 

        initialize = do
            snapExe <- findSnap
            systemOrDie $ snapExe ++ " init " ++ snapInitArgs

            snapCoreSrc   <- fromEnv "SNAP_CORE_SRC" $ snapRepos </> "snap-core"
            snapServerSrc <- fromEnv "SNAP_SERVER_SRC" $ snapRepos </> "snap-server"
            xmlhtmlSrc    <- fromEnv "XMLHTML_SRC" $ snapRepos </> "xmlhtml"
            heistSrc      <- fromEnv "HEIST_SRC" $ snapRepos </> "heist"
            let snapSrc   =  snapRoot

            forM_ [ "snap-core", "snap-server", "xmlhtml", "heist", "snap" ]
                  (pkgCleanUp sandbox)

            forM_ [ snapCoreSrc, snapServerSrc, xmlhtmlSrc, heistSrc
                  , snapSrc] $ \s ->
                systemOrDie $ "cabal-dev " ++ cabalDevArgs
                                ++ " add-source " ++ s

            systemOrDie $ "cabal-dev install " ++ args
            let cmd = ("." </> "dist" </> "build" </> projName </> projName)
                      ++ " -p " ++ show httpPort
            putStrLn $ "Running \"" ++ cmd ++ "\""
            pHandle <- runCommand cmd
            waitABit
            return pHandle

        findSnap = do
            home <- fromEnv "HOME" "."
            p1 <- gimmeIfExists $ snapRoot </> "dist" </> "build" </> "snap" </> "snap"
            p2 <- gimmeIfExists $ home </> ".cabal" </> "bin" </> "snap"
            p3 <- findExecutable "snap"

            return $ fromMaybe (error "couldn't find snap executable")
                               (getFirst $ mconcat $ map First [p1,p2,p3])

    putStrLn $ "Changing directory to "++projectPath
    inDir True projectPath $ bracket initialize cleanup (const testAction)
    removeDirectoryRecursiveSafe projectPath
  where

    fromEnv name def = do
        r <- getEnv name `catch` \(_::SomeException) -> return ""
        if r == "" then return def else return r

    cleanup pHandle = do
        terminateProcess pHandle
        waitForProcess pHandle

    waitABit = threadDelay $ 2*10^(6::Int)

    pkgCleanUp d pkg = do
        paths <- globDir1 (compile $ "packages*conf/" ++ pkg ++ "-*") d
        forM_ paths
              (\x -> (rm x `catch` \(_::SomeException) -> return ()))
      where
        rm x = do
            putStrLn $ "removing " ++ x
            removeFile x

    gimmeIfExists p = do
        b <- doesFileExist p
        if b then return (Just p) else return Nothing


systemOrDie :: String -> IO ()
systemOrDie s = do
    putStrLn $ "Running \"" ++ s ++ "\""
    system s >>= check
  where
    check ExitSuccess = return ()
    check _ = throwIO $ ErrorCall $ "command failed: '" ++ s ++ "'"
