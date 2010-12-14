{-# LANGUAGE CPP #-}
module Snap.Loader.Hint.Helper (protectHandlers) where

import Control.Exception (bracket)

#ifdef mingw32_HOST_OS
import GHC.ConsoleHandler as C


saveHandlers :: IO C.Handler
saveHandlers = C.installHandler Ignore


restoreHandlers :: C.Handler -> IO C.Handler
restoreHandlers = C.installHandler


#else
import qualified System.Posix.Signals as S

helper :: S.Handler -> S.Signal -> IO S.Handler
helper handler signal = S.installHandler signal handler Nothing


signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]


saveHandlers :: IO [S.Handler]
saveHandlers = mapM (helper S.Ignore) signals


restoreHandlers :: [S.Handler] -> IO [S.Handler]
restoreHandlers h = sequence $ zipWith helper h signals


#endif
protectHandlers :: IO a -> IO a
protectHandlers a = bracket saveHandlers restoreHandlers $ const a
