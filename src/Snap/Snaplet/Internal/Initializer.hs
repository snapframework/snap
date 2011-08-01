{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Snap.Snaplet.Internal.Initializer
  ( addPostInitHook
  , addPostInitHookBase
  , bracketInit
  , modifyCfg
  , nestSnaplet
  , embedSnaplet
  , makeSnaplet
  , nameSnaplet
  , onUnload
  , addRoutes
  , wrapHandlers
  , runEverything
  , serveSnaplet
  , printInfo
  ) where

import           Prelude hiding ((.), id, catch)
import           Control.Category
import           Control.Concurrent.MVar
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO hiding (Handler)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Writer hiding (pass)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Configurator
import           Data.IORef
import           Data.Maybe
import           Data.Record.Label
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.GZip
import           System.Directory
import           System.Directory.Tree
import           System.FilePath.Posix
import           System.IO

import           Snap.Snaplet.Internal.Lens
import           Snap.Snaplet.Internal.Types


------------------------------------------------------------------------------
-- | 'get' for InitializerState.
iGet :: Initializer b v (InitializerState b)
iGet = Initializer $ getBase


------------------------------------------------------------------------------
-- | 'get' for InitializerState.
iPut :: InitializerState b -> Initializer b v ()
iPut s = Initializer $ putBase s


------------------------------------------------------------------------------
-- | 'modify' for InitializerState.
iModify :: (InitializerState b -> InitializerState b) -> Initializer b v ()
iModify f = Initializer $ do
    b <- getBase
    putBase $ f b


------------------------------------------------------------------------------
-- | 'gets' for InitializerState.
iGets :: (InitializerState b -> a) -> Initializer b v a
iGets f = Initializer $ do
    b <- getBase
    return $ f b


------------------------------------------------------------------------------
-- | Converts a plain hook into a Snaplet hook.
toSnapletHook :: (v -> IO v) -> (Snaplet v -> IO (Snaplet v))
toSnapletHook f (Snaplet cfg val) = do
    val' <- f val
    return $! Snaplet cfg val'


------------------------------------------------------------------------------
-- | Adds an IO action that modifies the current snaplet state to be run at
-- the end of initialization on the state that was created.  This makes it
-- easier to allow one snaplet's state to be modified by another snaplet's
-- initializer.  A good example of this is when a snaplet has templates that
-- define its views.  The Heist snaplet provides the 'addTemplates' function
-- which allows other snaplets to set up their own templates.  'addTemplates'
-- is implemented using this function.
addPostInitHook :: (v -> IO v) -> Initializer b v ()
addPostInitHook h = do
    h' <- upHook $ toSnapletHook h
    addPostInitHookBase' h'


------------------------------------------------------------------------------
-- | Adds an IO action that modifies the application state to be run at the
-- end of initialization.
addPostInitHookBase :: (b -> IO b) -> Initializer b v ()
addPostInitHookBase = Initializer . lift . tell . Hook . toSnapletHook


------------------------------------------------------------------------------
addPostInitHookBase' :: (Snaplet b -> IO (Snaplet b))
                     -> Initializer b v ()
addPostInitHookBase' = Initializer . lift . tell . Hook


------------------------------------------------------------------------------
-- | Helper function for transforming hooks.
upHook :: (Snaplet v -> IO (Snaplet v))
       -> Initializer b v (Snaplet b -> IO (Snaplet b))
upHook h = Initializer $ do
    l <- ask
    return $ (\b -> do v <- h (getL l b)
                       return $ setL l v b)


------------------------------------------------------------------------------
-- | Modifies the Initializer's SnapletConfig.
modifyCfg :: (SnapletConfig -> SnapletConfig) -> Initializer b v ()
modifyCfg f = iModify $ modL curConfig $ \c -> f c


------------------------------------------------------------------------------
-- | If a snaplet has a filesystem presence, this function creates and copies
-- the files if they dont' already exist.
setupFilesystem :: Maybe (IO FilePath)
                -- ^ The directory where the snaplet's reference files are
                -- stored.  Nothing if the snaplet doesn't come with any files
                -- that need to be installed.
                -> FilePath
                -- ^ Directory where the files should be copied.
                -> Initializer b v ()
setupFilesystem Nothing _ = return ()
setupFilesystem (Just getSnapletDataDir) targetDir = do
    exists <- liftIO $ doesDirectoryExist targetDir
    unless exists $ do
        printInfo "...setting up filesystem"
        liftIO $ createDirectoryIfMissing True targetDir
        srcDir <- liftIO getSnapletDataDir
        (_ :/ dTree) <- liftIO $ readDirectoryWith B.readFile srcDir
        let (topDir,snapletId) = splitFileName targetDir
        _ <- liftIO $ writeDirectoryWith B.writeFile
               (topDir :/ dTree { name = snapletId })
        return ()


------------------------------------------------------------------------------
-- | Designed to be called by snaplet initializers to handle standardized
-- housekeeping common to all snaplets.  All snaplets must use this function
-- to construct their initializers.  Common usage will look something like
-- this:
--
-- @
-- fooInit :: Initializer b v (Snaplet Foo)
-- fooInit = makeSnaplet \"foo\" Nothing $ do
--     -- Your initializer code here
--     return $ Foo 42
-- @
makeSnaplet :: Text
       -- ^ A default id for this snaplet set by the snaplet itself.  This id
       -- is only used when the end-user has not already set an id using the
       -- nameSnaplet function.
       -> Text
       -- ^ A human readable description of this snaplet.
       -> Maybe (IO FilePath)
       -- ^ The path to the directory holding the snaplet's reference
       -- filesystem content.  This will almost always be the directory
       -- returned by Cabal's getDataDir command, but it has to be passed in
       -- because it is defined in a package-specific import.  Setting this
       -- value to Nothing doesn't preclude the snaplet from having files in
       -- in the filesystem, it just means that they won't be copied there
       -- automatically.
       -> Initializer b v v
       -- ^ Snaplet initializer.
       -> SnapletInit b v
makeSnaplet snapletId desc getSnapletDataDir m = SnapletInit $ do
    modifyCfg $ \c -> if isNothing $ _scId c
        then setL scId (Just snapletId) c else c
    sid <- iGets (T.unpack . fromJust . _scId . _curConfig)
    topLevel <- iGets _isTopLevel
    unless topLevel $ modifyCfg $ \c -> setL scFilePath
        (_scFilePath c </> "snaplets" </> sid) c
    iModify (setL isTopLevel False)
    modifyCfg $ modL scUserConfig (subconfig (T.pack sid))
    modifyCfg $ setL scDescription desc
    cfg <- iGets _curConfig
    printInfo $ T.pack $ concat
      ["Initializing "
      ,sid
      ," @ /"
      ,B.unpack $ buildPath $ _scRouteContext cfg
      ]

    -- This has to happen here because it needs to be after scFilePath is set
    -- up but before snaplet.cfg is read.
    setupFilesystem getSnapletDataDir (_scFilePath cfg)

    liftIO $ addToConfig [Optional (_scFilePath cfg </> "snaplet.cfg")]
                         (_scUserConfig cfg)
    mkSnaplet m


------------------------------------------------------------------------------
-- | Internal function that gets the SnapletConfig out of the initializer
-- state and uses it to create a (Snaplet a).
mkSnaplet :: Initializer b v a -> Initializer b v (Snaplet a)
mkSnaplet m = do
    res <- m
    cfg <- iGets _curConfig
    return $ Snaplet cfg res


------------------------------------------------------------------------------
-- | Brackets an initializer computation, restoring curConfig after the
-- computation returns.
bracketInit :: Initializer b v a -> Initializer b v a
bracketInit m = do
    s <- iGet
    res <- m
    iModify (setL curConfig (_curConfig s))
    return res


------------------------------------------------------------------------------
-- | Handles modifications to InitializerState that need to happen before a
-- snaplet is called with either nestSnaplet or embedSnaplet.
setupSnapletCall rte = do
    curId <- iGets (_scId . _curConfig)
    modifyCfg (modL scAncestry (fromJust curId:))
    modifyCfg (modL scId (const Nothing))
    unless (B.null rte) $ modifyCfg (modL scRouteContext (rte:))


------------------------------------------------------------------------------
-- | This function handles modifications to the initializer state that must
-- happen before each subsnaplet initializer runs.
nestSnaplet :: ByteString
            -- ^ The root url for all the snaplet's routes.  An empty string
            -- gives the routes the same root as the parent snaplet's routes.
            -> (v :-> Snaplet v1)
            -- ^ Lens identifying the snaplet
            -> SnapletInit b v1
            -- ^ The initializer function for the subsnaplet.
            -> Initializer b v (Snaplet v1)
nestSnaplet rte l (SnapletInit snaplet) = with l $ bracketInit $ do
    setupSnapletCall rte
    snaplet


------------------------------------------------------------------------------
-- | This function handles modifications to the initializer state that must
-- happen before each subsnaplet initializer runs.  
embedSnaplet :: ByteString
             -- ^ The root url for all the snaplet's routes.  An empty string
             -- gives the routes the same root as the parent snaplet's routes.
             -> (v :-> Snaplet v1)
             -- ^ Lens identifying the snaplet
             -> SnapletInit v1 v1
             -- ^ The initializer function for the subsnaplet.
             -> Initializer b v (Snaplet v1)
embedSnaplet rte l (SnapletInit snaplet) = do
    curLens <- getLens
    setupSnapletCall rte
    chroot rte (subSnaplet l . curLens) snaplet


------------------------------------------------------------------------------
-- | Changes the base state of an initializer.
--
-- NOTE: You shouldn't use bracketInit with this function as in nestSnaplet
-- because that is handled by the implementation.
chroot :: ByteString
       -> (Snaplet b :-> Snaplet v1)
       -> Initializer v1 v1 a
       -> Initializer b v a
chroot rte l (Initializer m) = do
    curState <- iGet
    ((a,s), (Hook hook)) <- liftIO $ runWriterT $ runLensT m id $
        curState {
          _handlers = [],
          _hFilter = id
        }
    let handler = chrootHandler l $ _hFilter s $ route $ _handlers s
    iModify $ modL handlers (++[(rte,handler)])
            . setL cleanup (_cleanup s)
    return a


------------------------------------------------------------------------------
-- | Changes the base state of a handler.
chrootHandler :: (Snaplet v :-> Snaplet b')
              -> Handler b' b' a -> Handler b v a
chrootHandler l (Handler h) = Handler $ do
    s <- get
    (a, s') <- liftSnap $ runLensT h id (getL l s)
    modify $ setL l s'
    return a


------------------------------------------------------------------------------
-- | Sets a snaplet's name.  All snaplets have a default name set by the
-- snaplet author.  This function allows you to override that name.  You will
-- have to do this if you have more than one instance of the same kind of
-- snaplet because snaplet names must be unique.  This function must
-- immediately surround the snaplet's initializer.  For example:
--
-- @fooState <- nestSnaplet \"fooA\" $ nameSnaplet \"myFoo\" $ fooInit@
nameSnaplet :: Text
            -- ^ The snaplet name
            -> SnapletInit b v
            -- ^ The snaplet initializer function
            -> SnapletInit b v
nameSnaplet nm (SnapletInit m) = SnapletInit $
    modifyCfg (setL scId (Just nm)) >> m


------------------------------------------------------------------------------
-- | Adds routing to the current 'Handler'.  The new routes are merged with the
-- main routing section and take precedence over existing routing that was
-- previously defined.
addRoutes :: [(ByteString, Handler b v ())]
           -> Initializer b v ()
addRoutes rs = do
    l <- getLens
    ctx <- iGets (_scRouteContext . _curConfig)
    let rs' = map (\(r,h) -> (buildPath (r:ctx), withTop' l h)) rs
    iModify (\v -> modL handlers (++rs') v)


------------------------------------------------------------------------------
-- | Wraps the snaplet's routing.  This can be used to provide a snaplet that
-- does per-request setup and cleanup, but then dispatches to the rest of the
-- application.
wrapHandlers :: (Handler b v () -> Handler b v ()) -> Initializer b v ()
wrapHandlers f0 = do
    f <- mungeFilter f0
    iModify (\v -> modL hFilter (f.) v)


------------------------------------------------------------------------------
mungeFilter :: (Handler b v () -> Handler b v ())
            -> Initializer b v (Handler b b () -> Handler b b ())
mungeFilter f = do
    myLens <- Initializer ask
    return $ \m -> b myLens $ f' m

  where
    f' (Handler m)       = f $ Handler $ withLensT (const id) m
    b myLens (Handler m) = Handler $ withLensT ((myLens .)) m


------------------------------------------------------------------------------
-- | Attaches an unload handler to the snaplet.  The unload handler will be
-- called when the server shuts down, or is reloaded.
onUnload :: IO () -> Initializer b v ()
onUnload m = iModify (\v -> modL cleanup (m>>) v)


------------------------------------------------------------------------------
-- | 
logInitMsg :: IORef Text -> Text -> IO ()
logInitMsg ref msg = atomicModifyIORef ref (\cur -> (cur `T.append` msg, ()))


------------------------------------------------------------------------------
-- | Initializers should use this function for all informational or error
-- messages to be displayed to the user.  On application startup they will be
-- sent to the console.  When executed from the reloader, they will be sent
-- back to the user in the HTTP response.
printInfo :: Text -> Initializer b v ()
printInfo msg = do
    logRef <- iGets _initMessages
    liftIO $ logInitMsg logRef (msg `T.append` "\n")


------------------------------------------------------------------------------
-- | Builds an IO reload action for storage in the SnapletState.
mkReloader :: MVar (Snaplet b)
           -> Initializer b b (Snaplet b)
           -> IO (Either String String)
mkReloader mvar i = do
    !res <- try $ runEverything mvar i
    either bad good res
  where
    bad e = do
        return $ Left $ show (e :: SomeException)
    good (b,is) = do
        _ <- swapMVar mvar b
        msgs <- readIORef $ _initMessages is
        return $ Right $ T.unpack msgs


------------------------------------------------------------------------------
-- | Runs a top-level snaplet in the Snap monad.
runBase :: Handler b b a
        -> MVar (Snaplet b)
        -> Snap a
runBase (Handler m) mvar = do
    !b <- liftIO (readMVar mvar)
    (!a, _) <- runLensT m id b
    return $! a


------------------------------------------------------------------------------
-- | 
runEverything :: MVar (Snaplet b)
              -> Initializer b b (Snaplet b)
              -> IO (Snaplet b, InitializerState b)
runEverything mvar b@(Initializer i) = do
    userConfig <- load [Optional "snaplet.cfg"]
    let cfg = SnapletConfig [] "" Nothing "" userConfig [] (mkReloader mvar b)
    logRef <- newIORef ""
    ((res, s), (Hook hook)) <- runWriterT $ runLensT i id $
        InitializerState True (return ()) [] id cfg logRef
    res' <- hook res
    return (res', s) 


------------------------------------------------------------------------------
-- | Serves a top-level snaplet as a web application.
serveSnaplet :: Config Snap a -> SnapletInit b b -> IO ()
serveSnaplet cfg (SnapletInit b) = do
    snapletMVar <- newEmptyMVar
    (siteSnaplet, is) <- runEverything snapletMVar b
    putMVar snapletMVar siteSnaplet

    config <- commandLineConfig cfg
    conf <- completeConfig config
    let site     = compress $ _hFilter is $ route $ _handlers is
        compress = if fromJust $ getCompression conf then withCompression else id
        catch500 = (flip catch $ fromJust $ getErrorHandler conf) :: Snap () -> Snap ()
        serve    = simpleHttpServe config

    msgs <- liftIO $ readIORef $ _initMessages is
    liftIO $ hPutStrLn stderr $ T.unpack msgs
    _ <- try $ serve $ catch500 $ runBase site snapletMVar
         :: IO (Either SomeException ())
    _cleanup is


