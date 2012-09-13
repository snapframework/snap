module Paths_snap (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,10,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/adinapoli/programming/Haskell/snap-dev/.hsenv_snap-dev/cabal/bin"
libdir     = "/home/adinapoli/programming/Haskell/snap-dev/.hsenv_snap-dev/cabal/lib/snap-0.10.0/ghc-7.4.1"
datadir    = "/home/adinapoli/programming/Haskell/snap-dev/.hsenv_snap-dev/cabal/share/snap-0.10.0"
libexecdir = "/home/adinapoli/programming/Haskell/snap-dev/.hsenv_snap-dev/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "snap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snap_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "snap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
