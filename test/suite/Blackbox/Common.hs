module Blackbox.Common where

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

shConfigSplice :: SnapletISplice b v
shConfigSplice = liftHeist . textSplice =<< genericConfigString

