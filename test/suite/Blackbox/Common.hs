module Blackbox.Common where

import Control.Lens
import Control.Monad.Trans
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Heist.Interpreted

genericConfigString :: (MonadSnaplet m, Monad (m b v)) => m b v T.Text
genericConfigString = do
    a <- getSnapletAncestry
    b <- getSnapletFilePath
    c <- getSnapletName
    d <- getSnapletDescription
    e <- getSnapletRootURL
    return $ T.pack $ show (a,b,c,d,e)

handlerConfig :: Handler b v ()
handlerConfig = writeText =<< genericConfigString

shConfigSplice :: SnapletLens (Snaplet b) v -> SnapletISplice b
shConfigSplice _lens = textSplice =<< lift (with' _lens genericConfigString)

