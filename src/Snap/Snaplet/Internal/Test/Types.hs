module Snap.Snaplet.Internal.Test.Types where

------------------------------------------------------------------------------
import          Snap.Core
import          Data.Text (Text)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A SnapletEnvironment is what you get back after calling 'runSnaplet'.
data SnapletEnvironment = SnapletEnvironment
    { senvMessages    :: Text
    , senvHandler     :: Snap ()
    , senvCleanAction :: IO ()
    }
