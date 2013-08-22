{-# LANGUAGE TemplateHaskell #-}
module Snap.Snaplet.Heist.Internal where

import           Prelude
import           Control.Error
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Heist
import           Heist.Splices.Cache
import           System.FilePath.Posix

import           Snap.Core
import           Snap.Snaplet


data DefaultMode = Compiled | Interpreted


------------------------------------------------------------------------------
-- | The state for the Heist snaplet.  To use the Heist snaplet in your app
-- include this in your application state and use 'heistInit' to initialize
-- it.  The type parameter b will typically be the base state type for your
-- application.
data Heist b = Configuring
                 { _heistConfig :: IORef (HeistConfig (Handler b b), DefaultMode)
                 }
             | Running
                 { _masterConfig :: HeistConfig (Handler b b)
                 , _heistState   :: HeistState (Handler b b)
                 , _heistCTS     :: CacheTagState
                 , _defMode      :: DefaultMode
                 }

makeLenses ''Heist


------------------------------------------------------------------------------
-- | Generic initializer function that allows compiled/interpreted template
-- serving to be specified by the caller.
gHeistInit :: Handler b (Heist b) ()
           -> FilePath
           -> SnapletInit b (Heist b)
gHeistInit serve templateDir = do
    makeSnaplet "heist" "" Nothing $ do
        hs <- heistInitWorker templateDir defaultConfig
        addRoutes [ ("", serve)
                  , ("heistReload", failIfNotLocal heistReloader)
                  ]
        return hs
  where
    defaultConfig = mempty { hcLoadTimeSplices = defaultLoadTimeSplices }


------------------------------------------------------------------------------
-- | Internal worker function used by variants of heistInit.  This is
-- necessary because of the divide between SnapletInit and Initializer.
heistInitWorker :: FilePath
                -> HeistConfig (Handler b b)
                -> Initializer b (Heist b) (Heist b)
heistInitWorker templateDir initialConfig = do
    snapletPath <- getSnapletFilePath
    let tDir = snapletPath </> templateDir
    templates <- liftIO $ runEitherT (loadTemplates tDir) >>=
                          either (error . concat) return
    printInfo $ T.pack $ unwords
        [ "...loaded"
        , (show $ Map.size templates)
        , "templates from"
        , tDir
        ]
    let config = initialConfig `mappend`
                 mempty { hcTemplateLocations = [loadTemplates tDir] }
    ref <- liftIO $ newIORef (config, Compiled)

    -- FIXME This runs after all the initializers, but before post init
    -- hooks registered by other snaplets.
    addPostInitHook finalLoadHook
    return $ Configuring ref


------------------------------------------------------------------------------
-- | Hook that converts the Heist type from Configuring to Running at the end
-- of initialization.
finalLoadHook :: Heist b -> EitherT Text IO (Heist b)
finalLoadHook (Configuring ref) = do
    (hc,dm) <- lift $ readIORef ref
    (hs,cts) <- toTextErrors $ initHeistWithCacheTag hc
    return $ Running hc hs cts dm
  where
    toTextErrors = bimapEitherT (T.pack . intercalate "\n") id
finalLoadHook (Running _ _ _ _) = left "finalLoadHook called while running"


------------------------------------------------------------------------------
-- | Handler that triggers a template reload.  For large sites, this can be
-- desireable because it may be much quicker than the full site reload
-- provided at the /admin/reload route.  This allows you to reload only the
-- heist templates  This handler is automatically set up by heistInit, but if
-- you use heistInit', then you can create your own route with it.
heistReloader :: Handler b (Heist b) ()
heistReloader = do
    h <- get
    ehs <- liftIO $ runEitherT $ initHeist $ _masterConfig h
    either (writeText . T.pack . unlines)
           (\hs -> do writeText "Heist reloaded."
                      modifyMaster $ set heistState hs h)
           ehs


