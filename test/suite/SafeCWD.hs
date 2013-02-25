module SafeCWD
  ( inDir
  , removeDirectoryRecursiveSafe
  ) where

import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import System.Directory
import System.IO.Unsafe

sem :: QSem
sem = unsafePerformIO $ newQSem 1
{-# NOINLINE sem #-}


inDir :: Bool -> FilePath -> IO a -> IO a
inDir startClean dir action = bracket before after (const action)
  where
    before = do
        waitQSem sem
        cwd <- getCurrentDirectory
        when startClean $ removeDirectoryRecursiveSafe dir
        createDirectoryIfMissing True dir
        setCurrentDirectory dir
        return cwd
    after cwd = do
        setCurrentDirectory cwd
        signalQSem sem


removeDirectoryRecursiveSafe p =
    doesDirectoryExist p >>= flip when (removeDirectoryRecursive p)
