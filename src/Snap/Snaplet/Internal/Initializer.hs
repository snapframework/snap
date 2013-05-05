{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Snap.Snaplet.Internal.Initializer
  ( addPostInitHook
  , addPostInitHookBase
  , toSnapletHook
  , bracketInit
  , modifyCfg
  , nestSnaplet
  , embedSnaplet
  , makeSnaplet
  , nameSnaplet
  , onUnload
  , addRoutes
  , wrapSite
  , runInitializer
  , runSnaplet
  , combineConfig
  , serveSnaplet
  , loadAppConfig
  , printInfo
  , getRoutes
  , modifyMaster
  ) where

import           Prelude hiding (catch)
import           Control.Concurrent.MVar
import           Control.Error
import           Control.Exception (SomeException)
import           Control.Lens
import           Control.Monad
import           Control.Monad.CatchIO hiding (Handler)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Writer hiding (pass)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Configurator
import qualified Data.Configurator.Types as C
import           Data.IORef
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Http.Server
import           Snap.Core
import           Snap.Util.GZip
import           System.Directory
import           System.Directory.Tree
import           System.FilePath.Posix
import           System.IO

import           Snap.Snaplet.Config
import qualified Snap.Snaplet.Internal.LensT as LT
import qualified Snap.Snaplet.Internal.Lensed as L
import           Snap.Snaplet.Internal.Types


------------------------------------------------------------------------------
-- | 'get' for InitializerState.
iGet :: Initializer b v (InitializerState b)
iGet = Initializer $ LT.getBase


------------------------------------------------------------------------------
-- | 'modify' for InitializerState.
iModify :: (InitializerState b -> InitializerState b) -> Initializer b v ()
iModify f = Initializer $ do
    b <- LT.getBase
    LT.putBase $ f b


------------------------------------------------------------------------------
-- | 'gets' for InitializerState.
iGets :: (InitializerState b -> a) -> Initializer b v a
iGets f = Initializer $ do
    b <- LT.getBase
    return $ f b


------------------------------------------------------------------------------
-- | Lets you retrieve the list of routes currently set up by an Initializer.
-- This can be useful in debugging.
getRoutes :: Initializer b v [ByteString]
getRoutes = liftM (map fst) $ iGets _handlers


------------------------------------------------------------------------------
-- | Converts a plain hook into a Snaplet hook.
toSnapletHook :: (v -> EitherT Text IO v)
              -> (Snaplet v -> EitherT Text IO (Snaplet v))
toSnapletHook f (Snaplet cfg  reset val) = do
    val' <- f val
    return $! Snaplet cfg reset val'


------------------------------------------------------------------------------
-- | Adds an IO action that modifies the current snaplet state to be run at
-- the end of initialization on the state that was created.  This makes it
-- easier to allow one snaplet's state to be modified by another snaplet's
-- initializer.  A good example of this is when a snaplet has templates that
-- define its views.  The Heist snaplet provides the 'addTemplates' function
-- which allows other snaplets to set up their own templates.  'addTemplates'
-- is implemented using this function.
addPostInitHook :: (v -> EitherT Text IO v)
                -> Initializer b v ()
addPostInitHook = addPostInitHook' . toSnapletHook


addPostInitHook' :: (Snaplet v -> EitherT Text IO (Snaplet v))
                 -> Initializer b v ()
addPostInitHook' h = do
    h' <- upHook h
    addPostInitHookBase h'


------------------------------------------------------------------------------
-- | Variant of addPostInitHook for when you have things wrapped in a Snaplet.
addPostInitHookBase :: (Snaplet b -> EitherT Text IO (Snaplet b))
                    -> Initializer b v ()
addPostInitHookBase = Initializer . lift . tell . Hook


------------------------------------------------------------------------------
-- | Helper function for transforming hooks.
upHook :: (Snaplet v -> EitherT Text IO (Snaplet v))
       -> Initializer b v (Snaplet b -> EitherT Text IO (Snaplet b))
upHook h = Initializer $ do
    l <- ask
    return $ upHook' l h


------------------------------------------------------------------------------
-- | Helper function for transforming hooks.
upHook' :: Monad m => ALens' b a -> (a -> m a) -> b -> m b
upHook' l h b = do
    v <- h (b ^# l)
    return $ storing l v b


------------------------------------------------------------------------------
-- | Modifies the Initializer's SnapletConfig.
modifyCfg :: (SnapletConfig -> SnapletConfig) -> Initializer b v ()
modifyCfg f = iModify $ over curConfig $ \c -> f c


------------------------------------------------------------------------------
-- | If a snaplet has a filesystem presence, this function creates and copies
-- the files if they dont' already exist.
setupFilesystem :: Maybe (IO FilePath)
                    -- ^ The directory where the snaplet's reference files are
                    -- stored.  Nothing if the snaplet doesn't come with any
                    -- files that need to be installed.
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
        liftIO $ readDirectoryWith (doCopy srcDir targetDir) srcDir
        return ()
  where
    doCopy srcRoot targetRoot filename = do
        createDirectoryIfMissing True directory
        copyFile filename toDir
      where
        toDir = targetRoot </> makeRelative srcRoot filename
        directory = dropFileName toDir


------------------------------------------------------------------------------
-- | All snaplet initializers must be wrapped in a call to @makeSnaplet@,
-- which handles standardized housekeeping common to all snaplets.
-- Common usage will look something like
-- this:
--
-- @
-- fooInit :: SnapletInit b Foo
-- fooInit = makeSnaplet \"foo\" \"An example snaplet\" Nothing $ do
--     -- Your initializer code here
--     return $ Foo 42
-- @
--
-- Note that you're writing your initializer code in the Initializer monad,
-- and makeSnaplet converts it into an opaque SnapletInit type.  This allows
-- us to use the type system to ensure that the API is used correctly.
makeSnaplet :: Text
                -- ^ A default id for this snaplet.  This is only used when
                -- the end-user has not already set an id using the
                -- nameSnaplet function.
            -> Text
                -- ^ A human readable description of this snaplet.
            -> Maybe (IO FilePath)
                -- ^ The path to the directory holding the snaplet's reference
                -- filesystem content.  This will almost always be the
                -- directory returned by Cabal's getDataDir command, but it
                -- has to be passed in because it is defined in a
                -- package-specific import.  Setting this value to Nothing
                -- doesn't preclude the snaplet from having files in in the
                -- filesystem, it just means that they won't be copied there
                -- automatically.
            -> Initializer b v v
                -- ^ Snaplet initializer.
            -> SnapletInit b v
makeSnaplet snapletId desc getSnapletDataDir m = SnapletInit $ do
    modifyCfg $ \c -> if isNothing $ _scId c
        then set scId (Just snapletId) c else c
    sid <- iGets (T.unpack . fromJust . _scId . _curConfig)
    topLevel <- iGets _isTopLevel
    unless topLevel $ do
        modifyCfg $ over scUserConfig (subconfig (T.pack sid))
        modifyCfg $ \c -> set scFilePath
          (_scFilePath c </> "snaplets" </> sid) c
    iModify (set isTopLevel False)
    modifyCfg $ set scDescription desc
    cfg <- iGets _curConfig
    printInfo $ T.pack $ concat
      ["Initializing "
      ,sid
      ," @ /"
      ,B.unpack $ buildPath $ _scRouteContext cfg
      ]

    -- This has to happen here because it needs to be after scFilePath is set
    -- up but before the config file is read.
    setupFilesystem getSnapletDataDir (_scFilePath cfg)

    env <- iGets _environment
    let configLocation = _scFilePath cfg </> (env ++ ".cfg")
    liftIO $ addToConfig [Optional configLocation]
                         (_scUserConfig cfg)
    mkSnaplet m


------------------------------------------------------------------------------
-- | Internal function that gets the SnapletConfig out of the initializer
-- state and uses it to create a (Snaplet a).
mkSnaplet :: Initializer b v v -> Initializer b v (Snaplet v)
mkSnaplet m = do
    res <- m
    cfg <- iGets _curConfig
    setInTop <- iGets masterReloader
    l <- getLens
    let modifier = setInTop  . set (cloneLens l . snapletValue)
    return $ Snaplet cfg modifier res
    


------------------------------------------------------------------------------
-- | Brackets an initializer computation, restoring curConfig after the
-- computation returns.
bracketInit :: Initializer b v a -> Initializer b v a
bracketInit m = do
    s <- iGet
    res <- m
    iModify (set curConfig (_curConfig s))
    return res


------------------------------------------------------------------------------
-- | Handles modifications to InitializerState that need to happen before a
-- snaplet is called with either nestSnaplet or embedSnaplet.
setupSnapletCall :: ByteString -> Initializer b v ()
setupSnapletCall rte = do
    curId <- iGets (fromJust . _scId . _curConfig)
    modifyCfg (over scAncestry (curId:))
    modifyCfg (over scId (const Nothing))
    unless (B.null rte) $ modifyCfg (over scRouteContext (rte:))


------------------------------------------------------------------------------
-- | Runs another snaplet's initializer and returns the initialized Snaplet
-- value.  Calling an initializer with nestSnaplet gives the nested snaplet
-- access to the same base state that the current snaplet has.  This makes it
-- possible for the child snaplet to make use of functionality provided by
-- sibling snaplets.
nestSnaplet :: ByteString
                -- ^ The root url for all the snaplet's routes.  An empty
                -- string gives the routes the same root as the parent
                -- snaplet's routes.
            -> SnapletLens v v1
                -- ^ Lens identifying the snaplet
            -> SnapletInit b v1
                -- ^ The initializer function for the subsnaplet.
            -> Initializer b v (Snaplet v1)
nestSnaplet rte l (SnapletInit snaplet) =
    with l $ bracketInit $ do
        setupSnapletCall rte
        snaplet


------------------------------------------------------------------------------
-- | Runs another snaplet's initializer and returns the initialized Snaplet
-- value.  The difference between this and nestSnaplet is the first type
-- parameter in the third argument.  The \"v1 v1\" makes the child snaplet
-- think that it is top-level, which means that it will not be able to use
-- functionality provided by snaplets included above it in the snaplet tree.
-- This strongly isolates the child snaplet, and allows you to eliminate the b
-- type variable.  The embedded snaplet can still get functionality from other
-- snaplets, but only if it nests or embeds the snaplet itself.
embedSnaplet :: ByteString
                 -- ^ The root url for all the snaplet's routes.  An empty
                 -- string gives the routes the same root as the parent
                 -- snaplet's routes.
                 --
                 -- NOTE: Because of the stronger isolation provided by
                 -- embedSnaplet, you should be more careful about using an
                 -- empty string here.
             -> SnapletLens v v1
                -- ^ Lens identifying the snaplet
             -> SnapletInit v1 v1
                -- ^ The initializer function for the subsnaplet.
             -> Initializer b v (Snaplet v1)
embedSnaplet rte l (SnapletInit snaplet) = bracketInit $ do
    curLens <- getLens
    setupSnapletCall ""
    chroot rte (cloneLens curLens . subSnaplet l) snaplet


------------------------------------------------------------------------------
-- | Changes the base state of an initializer.
chroot :: ByteString
       -> SnapletLens (Snaplet b) v1
       -> Initializer v1 v1 a
       -> Initializer b v a
chroot rte l (Initializer m) = do
    curState <- iGet
    let newSetter f = masterReloader curState (over (cloneLens l) f)
    ((a,s), (Hook hook)) <- liftIO $ runWriterT $ LT.runLensT m id $
        curState {
          _handlers = [],
          _hFilter = id,
          masterReloader = newSetter
        }
    let handler = chrootHandler l $ _hFilter s $ route $ _handlers s
    iModify $ over handlers (++[(rte,handler)])
            . set cleanup (_cleanup s)
    addPostInitHookBase $ upHook' l hook
    return a


------------------------------------------------------------------------------
-- | Changes the base state of a handler.
chrootHandler :: SnapletLens (Snaplet v) b'
              -> Handler b' b' a -> Handler b v a
chrootHandler l (Handler h) = Handler $ do
    s <- get
    (a, s') <- liftSnap $ L.runLensed h id (s ^# l)
    modify $ storing l s'
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
    modifyCfg (set scId (Just nm)) >> m


------------------------------------------------------------------------------
-- | Adds routing to the current 'Handler'.  The new routes are merged with
-- the main routing section and take precedence over existing routing that was
-- previously defined.
addRoutes :: [(ByteString, Handler b v ())]
           -> Initializer b v ()
addRoutes rs = do
    l <- getLens
    ctx <- iGets (_scRouteContext . _curConfig)
    let modRoute (r,h) = ( buildPath (r:ctx)
                         , setPattern r >> withTop' l h)
    let rs' = map modRoute rs
    iModify (\v -> over handlers (++rs') v)
  where
    setPattern r = do
      p <- getRoutePattern
      when (isNothing p) $ setRoutePattern r


------------------------------------------------------------------------------
-- | Wraps the /base/ snaplet's routing in another handler, allowing you to run
-- code before and after all routes in an application.
--
-- Here are some examples of things you might do:
--
-- > wrapSite (\site -> logHandlerStart >> site >> logHandlerFinished)
-- > wrapSite (\site -> ensureAdminUser >> site)
--
wrapSite :: (Handler b v () -> Handler b v ())
             -- ^ Handler modifier function
         -> Initializer b v ()
wrapSite f0 = do
    f <- mungeFilter f0
    iModify (\v -> over hFilter (f.) v)


------------------------------------------------------------------------------
mungeFilter :: (Handler b v () -> Handler b v ())
            -> Initializer b v (Handler b b () -> Handler b b ())
mungeFilter f = do
    myLens <- Initializer ask
    return $ \m -> with' myLens $ f' m
  where
    f' (Handler m)       = f $ Handler $ L.withTop id m


------------------------------------------------------------------------------
-- | Attaches an unload handler to the snaplet.  The unload handler will be
-- called when the server shuts down, or is reloaded.
onUnload :: IO () -> Initializer b v ()
onUnload m = do
    cleanupRef <- iGets _cleanup
    liftIO $ atomicModifyIORef cleanupRef f
  where
    f curCleanup = (curCleanup >> m, ())


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
mkReloader :: FilePath
           -> String
           -> ((Snaplet b -> Snaplet b) -> IO ())
           -> IORef (IO ())
           -> Initializer b b (Snaplet b)
           -> IO (Either Text Text)
mkReloader cwd env resetter cleanupRef i = do
    join $ readIORef cleanupRef
    !res <- runInitializer' resetter env i cwd
    either (return . Left) good res
  where
    good (b,is) = do
        _ <- resetter (const b)
        msgs <- readIORef $ _initMessages is
        return $ Right msgs


------------------------------------------------------------------------------
-- | Runs a top-level snaplet in the Snap monad.
runBase :: Handler b b a
        -> MVar (Snaplet b)
        -> Snap a
runBase (Handler m) mvar = do
    !b <- liftIO (readMVar mvar)
    (!a, _) <- L.runLensed m id b
    return $! a


------------------------------------------------------------------------------
-- | Lets you change a snaplet's initial state.  It's alomst like a reload,
-- except that it doesn't run the initializer.  It just modifies the result of
-- the initializer.  This can be used to let you define actions for reloading
-- individual snaplets.
modifyMaster :: v -> Handler b v ()
modifyMaster v = do
    modifier <- getsSnapletState _snapletModifier
    liftIO $ modifier v


------------------------------------------------------------------------------
-- | Internal function for running Initializers.  If any exceptions were
-- thrown by the initializer, this function catches them, runs any cleanup
-- actions that had been registered, and returns an expanded error message
-- containing the exception details as well as all messages generated by the
-- initializer before the exception was thrown.
runInitializer :: ((Snaplet b -> Snaplet b) -> IO ())
               -> String
               -> Initializer b b (Snaplet b)
               -> IO (Either Text (Snaplet b, InitializerState b))
runInitializer resetter env b =
    getCurrentDirectory >>= runInitializer' resetter env b


------------------------------------------------------------------------------
runInitializer' :: ((Snaplet b -> Snaplet b) -> IO ())
                -> String
                -> Initializer b b (Snaplet b)
                -> FilePath
                -> IO (Either Text (Snaplet b, InitializerState b))
runInitializer' resetter env b@(Initializer i) cwd = do
    cleanupRef <- newIORef (return ())
    let reloader_ = mkReloader cwd env resetter cleanupRef b
    let builtinHandlers = [("/admin/reload", reloadSite)]
    let cfg = SnapletConfig [] cwd Nothing "" empty [] Nothing reloader_
    logRef <- newIORef ""

    let body = runEitherT $ do
            ((res, s), (Hook hook)) <- lift $ runWriterT $ LT.runLensT i id $
                InitializerState True cleanupRef builtinHandlers id cfg logRef
                                 env resetter
            res' <- hook res
            right (res', s)

        handler e = do
            join $ readIORef cleanupRef
            logMessages <- readIORef logRef

            return $ Left $ T.unlines
                [ "Initializer threw an exception..."
                , T.pack $ show (e :: SomeException)
                , ""
                , "...but before it died it generated the following output:"
                , logMessages
                ]

    catch body handler


------------------------------------------------------------------------------
-- | Given an environment and a Snaplet initializer, produce a concatenated log
-- of all messages generated during initialization, a snap handler, and a
-- cleanup action.  The environment is an arbitrary string such as \"devel\" or
-- \"production\".  This string is used to determine the name of the
-- configuration files used by each snaplet.  If an environment of Nothing is
-- used, then runSnaplet defaults to \"devel\".
runSnaplet :: Maybe String -> SnapletInit b b -> IO (Text, Snap (), IO ())
runSnaplet env (SnapletInit b) = do
    snapletMVar <- newEmptyMVar
    let resetter f = modifyMVar_ snapletMVar (return . f)
    eRes <- runInitializer resetter (fromMaybe "devel" env) b
    let go (siteSnaplet,is) = do
        putMVar snapletMVar siteSnaplet
        msgs <- liftIO $ readIORef $ _initMessages is
        let handler = runBase (_hFilter is $ route $ _handlers is) snapletMVar
        cleanupAction <- readIORef $ _cleanup is
        return (msgs, handler, cleanupAction)
    either (error . ('\n':) . T.unpack) go eRes


------------------------------------------------------------------------------
-- | Given a configuration and a snap handler, complete it and produce the
-- completed configuration as well as a new toplevel handler with things like
-- compression and a 500 handler set up.
combineConfig :: Config Snap a -> Snap () -> IO (Config Snap a, Snap ())
combineConfig config handler = do
    conf <- completeConfig config

    let catch500 = (flip catch $ fromJust $ getErrorHandler conf)
    let compress = if fromJust (getCompression conf)
                     then withCompression else id
    let site     = compress $ catch500 handler

    return (conf, site)


------------------------------------------------------------------------------
-- | Initialize and run a Snaplet. This function parses command-line arguments,
-- runs the given Snaplet initializer, and starts an HTTP server running the
-- Snaplet's toplevel 'Handler'.
serveSnaplet :: Config Snap AppConfig
                 -- ^ The configuration of the server - you can usually pass a
                 -- default 'Config' via
                 -- 'Snap.Http.Server.Config.defaultConfig'.
             -> SnapletInit b b
                 -- ^ The snaplet initializer function.
             -> IO ()
serveSnaplet startConfig initializer = do
    config       <- commandLineAppConfig startConfig
    let env = appEnvironment =<< getOther config
    (msgs, handler, doCleanup) <- runSnaplet env initializer

    (conf, site) <- combineConfig config handler
    createDirectoryIfMissing False "log"
    let serve = simpleHttpServe conf

    when (loggingEnabled conf) $ liftIO $ hPutStrLn stderr $ T.unpack msgs
    _ <- try $ serve $ site
         :: IO (Either SomeException ())
    doCleanup
  where
    loggingEnabled = not . (== Just False) . getVerbose


------------------------------------------------------------------------------
-- | Allows you to get all of your app's config data in the IO monad without
-- the web server infrastructure.
loadAppConfig :: FileName
              -- ^ The name of the config file to look for.  In snap
              -- applications, this is something based on the
              -- environment...i.e. @devel.cfg@.
              -> FilePath
              -- ^ Path to the root directory of your project.
              -> IO C.Config
loadAppConfig cfg root = do
    tree <- buildL root
    let groups = loadAppConfig' cfg "" $ dirTree tree
    loadGroups groups


------------------------------------------------------------------------------
-- | Recursive worker for loadAppConfig.
loadAppConfig' :: FileName -> Text -> DirTree a -> [(Text, Worth a)]
loadAppConfig' cfg _prefix d@(Dir _ c) =
    (map ((_prefix,) . Required) $ getCfg cfg d) ++
    concatMap (\a -> loadAppConfig' cfg (nextPrefix $ name a) a) snaplets
  where
    nextPrefix p = T.concat [_prefix, T.pack p, "."]
    snapletsDirs = filter isSnapletsDir c
    snaplets = concatMap (filter isDir . contents) snapletsDirs
loadAppConfig' _ _ _ = []


isSnapletsDir :: DirTree t -> Bool
isSnapletsDir (Dir "snaplets" _) = True
isSnapletsDir _ = False


isDir :: DirTree t -> Bool
isDir (Dir _ _) = True
isDir _ = False


isCfg :: FileName -> DirTree t -> Bool
isCfg cfg (File n _) = cfg == n
isCfg _ _ = False


getCfg :: FileName -> DirTree b -> [b]
getCfg cfg (Dir _ c) = map file $ filter (isCfg cfg) c
getCfg _ _ = []

    

