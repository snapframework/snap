module Snap.Extension.Loader.Devel.TreeWatcher
    ( TreeStatus
    , getTreeStatus
    , checkTreeStatus
    ) where

import Control.Applicative

import System.Directory
import System.Directory.Tree

import System.Time


------------------------------------------------------------------------------
-- | An opaque representation of the contents and last modification
-- times of a forest of directory trees.
data TreeStatus = TS [FilePath] [AnchoredDirTree ClockTime]


------------------------------------------------------------------------------
-- | Create a 'TreeStatus' for later checking with 'checkTreeStatus'
getTreeStatus :: [FilePath] -> IO TreeStatus
getTreeStatus = liftA2 (<$>) TS readModificationTimes


------------------------------------------------------------------------------
-- | Checks that all the files present in the initial set of paths are
-- the exact set of files currently present, with unchanged modifcations times
checkTreeStatus :: TreeStatus -> IO Bool
checkTreeStatus (TS paths entries) = check <$> readModificationTimes paths
  where
    check = and . zipWith adtEq entries
    adtEq (n1 :/ dt1) (n2 :/ dt2) = n1 == n2 && dtEq dt1 dt2

    dtEq (Dir n1 d1) (Dir n2 d2) = n1 == n2 && and (zipWith dtEq d1 d2)
    dtEq (File n1 t1) (File n2 t2) = n1 == n2 && t1 == t2
    dtEq _ _ = False


------------------------------------------------------------------------------
-- | This is the core of the functions in this module.  It converts a
-- list of filepaths into a list of 'AnchoredDirTree' annotated with
-- the modification times of the files located in those paths.
readModificationTimes :: [FilePath] -> IO [AnchoredDirTree ClockTime]
readModificationTimes = mapM $ readDirectoryWith getModificationTime
