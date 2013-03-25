{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|

This module implements the Heist snaplet without using type classes.  It is
provided mainly as an example of how snaplets can be written with and without
a type class for convenience.

-}
module Snap.Snaplet.HeistNoClass
  ( Heist
  , DefaultMode(..)
  , heistInit
  , heistInit'
  , heistReloader
  , setInterpreted
  , getCurHeistConfig 
  , clearHeistCache

  , addTemplates
  , addTemplatesAt
  , modifyHeistState
  , modifyHeistState'
  , withHeistState
  , withHeistState'

  , gRender
  , gRenderAs
  , gHeistServe
  , gHeistServeSingle
  , chooseMode

  , addConfig
  , cRender
  , cRenderAs
  , cHeistServe
  , cHeistServeSingle

  , render
  , renderAs
  , heistServe
  , heistServeSingle
  , heistLocal
  , withSplices
  , renderWithSplices
  , heistLocal'
  , withSplices'
  , renderWithSplices'

  , SnapletHeist
  , SnapletISplice
  , SnapletCSplice
  ) where

import           Prelude hiding ((.), id)
import           Control.Applicative
import           Control.Category
import           Control.Error
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.DList (DList)
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           System.FilePath.Posix
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import           Heist.Splices.Cache

import           Snap.Snaplet
import           Snap.Snaplet.Heist.Internal
import           Snap.Core
import           Snap.Util.FileServe


------------------------------------------------------------------------------
changeState :: (HeistState (Handler a a) -> HeistState (Handler a a))
            -> Heist a
            -> Heist a
changeState _ (Configuring _)  =
    error "changeState: HeistState has not been initialized"
changeState f (Running hc hs cts dm) = Running hc (f hs) cts dm


------------------------------------------------------------------------------
-- | Clears data stored by the cache tag.  The cache tag automatically reloads
-- its data when the specified TTL expires, but sometimes you may want to
-- trigger a manual reload.  This function lets you do that.
clearHeistCache :: Heist b -> IO ()
clearHeistCache = clearCacheTagState . _heistCTS


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


                         -----------------------------
                         -- SnapletSplice functions --
                         -----------------------------

------------------------------------------------------------------------------
-- | This instance is here because we don't want the heist package to depend
-- on anything from snap packages.
instance MonadSnap m => MonadSnap (HeistT n m) where
    liftSnap = lift . liftSnap


type SnapletHeist b m a = HeistT (Handler b b) m a
type SnapletCSplice b = SnapletHeist b IO (DList (Chunk (Handler b b)))
type SnapletISplice b = SnapletHeist b (Handler b b) Template


                          ---------------------------
                          -- Initializer functions --
                          ---------------------------


------------------------------------------------------------------------------
-- | The 'Initializer' for 'Heist'. This function is a convenience wrapper
-- around `heistInit'` that uses defaultHeistState and sets up routes for all
-- the templates.  It sets up a \"heistReload\" route that reloads the heist
-- templates when you request it from localhost.
heistInit :: FilePath
              -- ^ Path to templates
          -> SnapletInit b (Heist b)
heistInit templateDir = do
    makeSnaplet "heist" "" Nothing $ do
        hs <- heistInitWorker templateDir defaultConfig
        addRoutes [ ("", heistServe)
                  , ("heistReload", failIfNotLocal heistReloader)
                  ]
        return hs
  where
    defaultConfig = mempty { hcLoadTimeSplices = defaultLoadTimeSplices }


------------------------------------------------------------------------------
-- | A lower level 'Initializer' for 'Heist'.  This initializer requires you
-- to specify the initial HeistConfig.  It also does not add any routes for
-- templates, allowing you complete control over which templates get routed.
heistInit' :: FilePath
               -- ^ Path to templates
           -> HeistConfig (Handler b b)
               -- ^ Initial HeistConfig
           -> SnapletInit b (Heist b)
heistInit' templateDir initialConfig =
    makeSnaplet "heist" "" Nothing $ heistInitWorker templateDir initialConfig


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
-- | Sets the snaplet to default to interpreted mode.  Initially, the
-- initializer sets the value to compiled mode.  This function allows you to
-- override that setting.  Note that this is just a default.  It only has an
-- effect if you use one of the generic functions: 'gRender', 'gRenderAs',
-- 'gHeistServe', or 'gHeistServeSingle'.  If you call the non-generic
-- versions directly, then this value will not be checked and you will get the
-- mode implemented by the function you called.
setInterpreted :: Snaplet (Heist b) -> Initializer b v ()
setInterpreted h = 
    liftIO $ atomicModifyIORef (_heistConfig $ view snapletValue h)
        (\(hc,_) -> ((hc,Interpreted),()))


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
-- | Adds templates to the Heist HeistConfig.  Other snaplets should use
-- this function to add their own templates.  The templates are automatically
-- read from the templates directory in the current snaplet's filesystem root.
addTemplates :: Snaplet (Heist b)
             -> ByteString
                 -- ^ The url prefix for the template routes
             -> Initializer b (Heist b) ()
addTemplates h urlPrefix = do
    snapletPath <- getSnapletFilePath
    addTemplatesAt h urlPrefix (snapletPath </> "templates")


------------------------------------------------------------------------------
-- | Adds templates to the Heist HeistConfig, and lets you specify where
-- they are found in the filesystem.  Note that the path to the template
-- directory is an absolute path.  This allows you more flexibility in where
-- your templates are located, but means that you have to explicitly call
-- getSnapletFilePath if you want your snaplet to use templates within its
-- normal directory structure.
addTemplatesAt :: Snaplet (Heist b)
               -> ByteString
                   -- ^ URL prefix for template routes
               -> FilePath
                   -- ^ Path to templates
               -> Initializer b (Heist b) ()
addTemplatesAt h urlPrefix templateDir = do
    rootUrl <- getSnapletRootURL
    let fullPrefix = (T.unpack $ decodeUtf8 rootUrl) </>
                     (T.unpack $ decodeUtf8 urlPrefix)
        addPrefix = addTemplatePathPrefix
                      (encodeUtf8 $ T.pack fullPrefix)
    ts <- liftIO $ runEitherT (loadTemplates templateDir) >>=
                   either (error . concat) return
    printInfo $ T.pack $ unwords
        [ "...adding"
        , (show $ Map.size ts)
        , "templates from"
        , templateDir
        , "with route prefix"
        , fullPrefix ++ "/"
        ]
    let locations = [liftM addPrefix $ loadTemplates templateDir]
        hc' = mempty { hcTemplateLocations = locations }
    liftIO $ atomicModifyIORef (_heistConfig $ view snapletValue h)
        (\(hc,dm) -> ((hc `mappend` hc', dm), ()))


getCurHeistConfig :: Snaplet (Heist b)
                  -> Initializer b v (HeistConfig (Handler b b))
getCurHeistConfig h = case view snapletValue h of
    Configuring ref -> do
        (hc, _) <- liftIO $ readIORef ref
        return hc
    Running _ _ _ _ ->
        error "Can't get HeistConfig after heist is initialized."
    

------------------------------------------------------------------------------
modifyHeistState' :: SnapletLens (Snaplet b) (Heist b)
                  -> (HeistState (Handler b b) -> HeistState (Handler b b))
                  -> Initializer b v ()
modifyHeistState' heist f = do
    withTop' heist $ addPostInitHook $ return . changeState f


------------------------------------------------------------------------------
modifyHeistState :: SnapletLens b (Heist b)
                 -> (HeistState (Handler b b) -> HeistState (Handler b b))
                 -> Initializer b v ()
modifyHeistState heist f = modifyHeistState' (subSnaplet heist) f


------------------------------------------------------------------------------
withHeistState' :: SnapletLens (Snaplet b) (Heist b)
                -> (HeistState (Handler b b) -> a)
                -> Handler b v a
withHeistState' heist f = do
    hs <- withTop' heist $ gets _heistState
    return $ f hs


------------------------------------------------------------------------------
withHeistState :: SnapletLens b (Heist b)
               -> (HeistState (Handler b b) -> a)
               -> Handler b v a
withHeistState heist f = withHeistState' (subSnaplet heist) f


------------------------------------------------------------------------------
-- | Adds more HeistConfig data using mappend with whatever is currently
-- there.  This is the preferred method for adding all four kinds of splices
-- as well as new templates.
addConfig :: Snaplet (Heist b)
          -> HeistConfig (Handler b b)
          -> Initializer b v ()
addConfig h hc = case view snapletValue h of
    Configuring ref ->
        liftIO $ atomicModifyIORef ref
                   (\(hc1,dm) -> ((hc1 `mappend` hc, dm), ()))
    Running _ _ _ _ -> do
        printInfo "finalLoadHook called while running"
        error "this shouldn't happen"


                            -----------------------
                            -- Handler functions --
                            -----------------------

------------------------------------------------------------------------------
-- | Internal helper function for rendering.
iRenderHelper :: Maybe MIMEType
             -> ByteString
             -> Handler b (Heist b) ()
iRenderHelper c t = do
    (Running _ hs _ _) <- get
    withTop' id $ I.renderTemplate hs t >>= maybe pass serve
  where
    serve (b, mime) = do
        modifyResponse $ setContentType $ fromMaybe mime c
        writeBuilder b


------------------------------------------------------------------------------
-- | Internal helper function for rendering.
cRenderHelper :: Maybe MIMEType
              -> ByteString
              -> Handler b (Heist b) ()
cRenderHelper c t = do
    (Running _ hs _ _) <- get
    withTop' id $ maybe pass serve $ C.renderTemplate hs t
  where
    serve (b, mime) = do
        modifyResponse $ setContentType $ fromMaybe mime c
        writeBuilder =<< b


------------------------------------------------------------------------------
serveURI :: Handler b (Heist b) ByteString
serveURI = do
    p <- getSafePath
    -- Allows users to prefix template filenames with an underscore to prevent
    -- the template from being served.
    if take 1 p == "_" then pass else return $ B.pack p


------------------------------------------------------------------------------
render :: ByteString
           -- ^ Name of the template
       -> Handler b (Heist b) ()
render t = iRenderHelper Nothing t


------------------------------------------------------------------------------
renderAs :: ByteString
             -- ^ Content type
         -> ByteString
             -- ^ Name of the template
         -> Handler b (Heist b) ()
renderAs ct t = iRenderHelper (Just ct) t


------------------------------------------------------------------------------
heistServe :: Handler b (Heist b) ()
heistServe =
    ifTop (render "index") <|> (render =<< serveURI)


------------------------------------------------------------------------------
heistServeSingle :: ByteString -> Handler b (Heist b) ()
heistServeSingle t =
    render t <|> error ("Template " ++ show t ++ " not found.")


------------------------------------------------------------------------------
cRender :: ByteString
           -- ^ Name of the template
        -> Handler b (Heist b) ()
cRender t = cRenderHelper Nothing t


------------------------------------------------------------------------------
cRenderAs :: ByteString
             -- ^ Content type
          -> ByteString
             -- ^ Name of the template
          -> Handler b (Heist b) ()
cRenderAs ct t = cRenderHelper (Just ct) t


------------------------------------------------------------------------------
cHeistServe :: Handler b (Heist b) ()
cHeistServe =
    ifTop (cRender "index") <|> (cRender =<< serveURI)


------------------------------------------------------------------------------
cHeistServeSingle :: ByteString -> Handler b (Heist b) ()
cHeistServeSingle t =
    cRender t <|> error ("Template " ++ show t ++ " not found.")


------------------------------------------------------------------------------
-- | Chooses between a compiled action and an interpreted action based on the
-- configured default.
chooseMode :: MonadState (Heist b1) m
           => m b
               -- ^ A compiled action
           -> m b
               -- ^ An interpreted action
           -> m b
chooseMode cAction iAction = do
    mode <- gets _defMode
    case mode of
      Compiled -> cAction
      Interpreted -> iAction


------------------------------------------------------------------------------
-- | Like render/cRender, but chooses between the two appropriately based on
-- the default mode.
gRender :: ByteString
           -- ^ Name of the template
        -> Handler b (Heist b) ()
gRender t = chooseMode (cRender t) (render t)


------------------------------------------------------------------------------
-- | Like renderAs/cRenderAs, but chooses between the two appropriately based
-- on the default mode.
gRenderAs :: ByteString
             -- ^ Content type
          -> ByteString
             -- ^ Name of the template
          -> Handler b (Heist b) ()
gRenderAs ct t = chooseMode (cRenderAs ct t) (renderAs ct t)


------------------------------------------------------------------------------
-- | Like heistServe/cHeistServe, but chooses between the two appropriately
-- based on the default mode.
gHeistServe :: Handler b (Heist b) ()
gHeistServe = chooseMode cHeistServe heistServe


------------------------------------------------------------------------------
-- | Like heistServeSingle/cHeistServeSingle, but chooses between the two
-- appropriately based on the default mode.
gHeistServeSingle :: ByteString -> Handler b (Heist b) ()
gHeistServeSingle t = chooseMode (cHeistServeSingle t) (heistServeSingle t)


------------------------------------------------------------------------------
heistLocal' :: SnapletLens (Snaplet b) (Heist b)
            -> (HeistState (Handler b b) -> HeistState (Handler b b))
            -> Handler b v a
            -> Handler b v a
heistLocal' heist f m = do
    hs  <- withTop' heist get
    withTop' heist $ modify $ changeState f
    res <- m
    withTop' heist $ put hs
    return res


------------------------------------------------------------------------------
heistLocal :: SnapletLens b (Heist b)
           -> (HeistState (Handler b b) -> HeistState (Handler b b))
           -> Handler b v a
           -> Handler b v a
heistLocal heist f m = heistLocal' (subSnaplet heist) f m


------------------------------------------------------------------------------
withSplices' :: SnapletLens (Snaplet b) (Heist b)
             -> [(Text, SnapletISplice b)]
             -> Handler b v a
             -> Handler b v a
withSplices' heist splices m = do
    heistLocal' heist (I.bindSplices splices) m


------------------------------------------------------------------------------
withSplices :: SnapletLens b (Heist b)
            -> [(Text, SnapletISplice b)]
            -> Handler b v a
            -> Handler b v a
withSplices heist splices m = withSplices' (subSnaplet heist) splices m


------------------------------------------------------------------------------
renderWithSplices' :: SnapletLens (Snaplet b) (Heist b)
                   -> ByteString
                   -> [(Text, SnapletISplice b)]
                   -> Handler b v ()
renderWithSplices' heist t splices =
    withSplices' heist splices $ withTop' heist $ render t


------------------------------------------------------------------------------
renderWithSplices :: SnapletLens b (Heist b)
                  -> ByteString
                  -> [(Text, SnapletISplice b)]
                  -> Handler b v ()
renderWithSplices heist t splices =
    renderWithSplices' (subSnaplet heist) t splices


