module Snap.TestCommon where

import Control.Concurrent
import Control.Exception
import Control.Monad     (when)
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.Process

testGeneratedProject :: String  -- ^ project name and directory
                     -> String  -- ^ arguments to @snap init@
                     -> String  -- ^ arguments to @cabal install@
                     -> Int     -- ^ port to run http server on
                     -> IO ()   -- ^ action to run when the server goes up
                     -> IO ()
testGeneratedProject projName snapInitArgs cabalInstallArgs httpPort
                     testAction = bracket initialize cleanup (const testAction)
  where
    initialize = do
        cwd <- getCurrentDirectory
        let projectPath = cwd </> projName
        flip onException (setCurrentDirectory cwd >>
                          removeDirectoryRecursive projectPath) $ do
            makeWorkDirectory projectPath
            setCurrentDirectory projectPath
            systemOrDie $ "snap init " ++ snapInitArgs
            systemOrDie $ "cabal install " ++ cabalInstallArgs
            let cmd = ("." </> "dist" </> "build" </> projName </> projName)
                      ++ " -p " ++ show httpPort
            putStrLn $ "Running \"" ++ cmd ++ "\""
            pHandle <- runCommand cmd
            waitABit
            return (cwd, projectPath, pHandle)

    cleanup (cwd, projectPath, pHandle) = do
        setCurrentDirectory cwd
        terminateProcess pHandle
        waitForProcess pHandle
        removeDirectoryRecursive projectPath

    waitABit = threadDelay $ 2*10^(6::Int)

systemOrDie :: String -> IO ()
systemOrDie s = do
    putStrLn $ "Running \"" ++ s ++ "\""
    system s >>= check
  where
    check ExitSuccess = return ()
    check _ = throwIO $ ErrorCall $ "command failed: '" ++ s ++ "'"


makeWorkDirectory :: FilePath -> IO ()
makeWorkDirectory p = do
    doesDirectoryExist p >>= flip when (removeDirectoryRecursive p)
    createDirectory p
