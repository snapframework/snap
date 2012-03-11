{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Snap.Snaplet.HeistNoClass
  ( Heist
  , heistInit
  , heistInit'
  , clearHeistCache

  , addTemplates
  , addTemplatesAt
  , modifyHeistTS
  , modifyHeistTS'
  , withHeistTS
  , withHeistTS'
  , addSplices
  , addSplices'
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
  , SnapletSplice
  , runSnapletSplice
  , liftHeist
  , liftWith
  , liftHandler
  , liftAppHandler
  , bindSnapletSplices
  ) where

import           Prelude hiding ((.), id)
import           Control.Arrow
import           Control.Applicative
import           Control.Category
import           Control.Monad.CatchIO (MonadCatchIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U
import           Data.Maybe
import           Data.Monoid
import           Data.Lens.Lazy
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath.Posix
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Cache

import           Snap.Snaplet
import           Snap.Core
import           Snap.Util.FileServe


------------------------------------------------------------------------------
-- | The state for the Heist snaplet.  To use the Heist snaplet in your app
-- include this in your application state and use 'heistInit' to initialize
-- it.  The type parameter b will typically be the base state type for your
-- application.
--
data Heist b = Heist
    { _heistTS       :: HeistState (Handler b b)
    , _heistCTS      :: CacheTagState
    }


------------------------------------------------------------------------------
changeTS :: (HeistState (Handler a a) -> HeistState (Handler a a))
         -> Heist a
         -> Heist a
changeTS f (Heist ts cts) = Heist (f ts) cts


------------------------------------------------------------------------------
-- | Clears data stored by the cache tag.  The cache tag automatically reloads
-- its data when the specified TTL expires, but sometimes you may want to
-- trigger a manual reload.  This function lets you do that.
--
clearHeistCache :: Heist b -> IO ()
clearHeistCache = clearCacheTagState . _heistCTS


                         -----------------------------
                         -- SnapletSplice functions --
                         -----------------------------

------------------------------------------------------------------------------
-- | This instance is here because we don't want the heist package to depend
-- on anything from snap packages.
--
instance MonadSnap m => MonadSnap (HeistT m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
-- | Monad for working with Heist's API from within a snaplet.
--
newtype SnapletHeist b v a = SnapletHeist
    (ReaderT (Lens (Snaplet b) (Snaplet v)) (HeistT (Handler b b)) a)
  deriving ( Monad
           , Functor
           , Applicative
           , Alternative
           , MonadIO
           , MonadPlus
           , MonadReader (Lens (Snaplet b) (Snaplet v))
           , MonadCatchIO
           , MonadSnap
           )


------------------------------------------------------------------------------
-- | Type alias for convenience.
--
type SnapletSplice b v = SnapletHeist b v Template


------------------------------------------------------------------------------
-- | Runs the SnapletSplice.
--
runSnapletSplice :: (Lens (Snaplet b) (Snaplet v))
                 -> SnapletHeist b v a
                 -> HeistT (Handler b b) a
runSnapletSplice l (SnapletHeist m) = runReaderT m l


------------------------------------------------------------------------------
withSS :: (Lens (Snaplet b) (Snaplet v) -> Lens (Snaplet b) (Snaplet v'))
       -> SnapletHeist b v' a
       -> SnapletHeist b v a
withSS f (SnapletHeist m) = SnapletHeist $ withReaderT f m


------------------------------------------------------------------------------
-- | Lifts a HeistT action into SnapletHeist.  Use this with all the functions
-- from the Heist API.
--
liftHeist :: HeistT (Handler b b) a -> SnapletHeist b v a
liftHeist = SnapletHeist . lift


------------------------------------------------------------------------------
-- | Common idiom for the combination of liftHandler and withTop.
--
liftWith :: (Lens (Snaplet b) (Snaplet v'))
         -> Handler b v' a
         -> SnapletHeist b v a
liftWith l = liftHeist . lift . withTop' l


------------------------------------------------------------------------------
-- | Lifts a Handler into SnapletHeist.
--
liftHandler :: Handler b v a -> SnapletHeist b v a
liftHandler m = do
    l <- ask
    liftWith l m


------------------------------------------------------------------------------
-- | Lifts a (Handler b b) into SnapletHeist.
--
liftAppHandler :: Handler b b a -> SnapletHeist b v a
liftAppHandler = liftHeist . lift


------------------------------------------------------------------------------
instance MonadState v (SnapletHeist b v) where
    get = do
        l <- ask
        b <- liftAppHandler getSnapletState
        return $ getL (snapletValue . l) b
    put s = do
        l <- ask
        b <- liftAppHandler getSnapletState
        liftAppHandler $ putSnapletState $ setL (snapletValue . l) s b


------------------------------------------------------------------------------
-- | MonadSnaplet instance gives us access to the snaplet infrastructure.
--
instance MonadSnaplet SnapletHeist where
    getLens = ask
    with' l = withSS (l .)
    withTop' l = withSS (const id) . with' l
    getOpaqueConfig = do
        l <- ask
        b <- liftAppHandler getSnapletState
        return $ getL (snapletConfig . l) b


------------------------------------------------------------------------------
-- | SnapletSplices version of bindSplices.
--
bindSnapletSplices :: (Lens (Snaplet b) (Snaplet v))
                   -> [(Text, SnapletSplice b v)]
                   -> HeistState (Handler b b)
                   -> HeistState (Handler b b)
bindSnapletSplices l splices =
    bindSplices $ map (second $ runSnapletSplice l) splices


                          ---------------------------
                          -- Initializer functions --
                          ---------------------------

------------------------------------------------------------------------------
-- | The 'Initializer' for 'Heist'. This function is a convenience wrapper
-- around `heistInit'` that uses the default `mempty` HeistState and sets up
-- routes for all the templates.
--
heistInit :: FilePath                 -- ^ Path to templates
          -> SnapletInit b (Heist b)
heistInit templateDir = do
    makeSnaplet "heist" "" Nothing $ do
        hs <- heistInitWorker templateDir defaultHeistState
        addRoutes [ ("", heistServe) ]
        return hs


------------------------------------------------------------------------------
-- | A lower level 'Initializer' for 'Heist'.  This initializer requires you
-- to specify the initial HeistState.  It also does not add any routes for
-- templates, allowing you complete control over which templates get routed.
--
heistInit' :: FilePath
           -- ^ Path to templates
           -> HeistState (Handler b b)
           -- ^ Initial HeistState
           -> SnapletInit b (Heist b)
heistInit' templateDir initialHeistState =
    makeSnaplet "heist" "" Nothing $
        heistInitWorker templateDir initialHeistState


------------------------------------------------------------------------------
-- | Internal worker function used by variantsof heistInit.  This is necessary
-- because of the divide between SnapletInit and Initializer.
--
heistInitWorker :: FilePath
                -> HeistState (Handler b b)
                -> Initializer b v (Heist b)
heistInitWorker templateDir initialHeistState = do
    (cacheFunc, cts) <- liftIO mkCacheTag
    let origTs = cacheFunc initialHeistState
    snapletPath <- getSnapletFilePath
    let tDir = snapletPath </> templateDir
    ts <- liftIO $ loadTemplates tDir origTs >>=
                   either error return
    printInfo $ T.pack $ unwords
        [ "...loaded"
        , (show $ length $ templateNames ts)
        , "templates from"
        , tDir
        ]

    return $ Heist ts cts


------------------------------------------------------------------------------
-- | Adds templates to the Heist HeistState.  Other snaplets should use
-- this function to add their own templates.  The templates are automatically
-- read from the templates directory in the current snaplet's filesystem root.
addTemplates :: ByteString
             -- ^ The url prefix for the template routes
             -> Initializer b (Heist b) ()
addTemplates urlPrefix = do
    snapletPath <- getSnapletFilePath
    addTemplatesAt urlPrefix (snapletPath </> "templates")


------------------------------------------------------------------------------
-- | Adds templates to the Heist HeistState, and lets you specify where
-- they are found in the filesystem.  Note that the path to the template
-- directory is an absolute path.  This allows you more flexibility in where
-- your templates are located, but means that you have to explicitly call
-- getSnapletFilePath if you want your snaplet to use templates within its
-- normal directory structure.
addTemplatesAt :: ByteString
               -- ^ URL prefix for template routes
               -> FilePath
               -- ^ Path to templates
               -> Initializer b (Heist b) ()
addTemplatesAt urlPrefix templateDir = do
    ts <- liftIO $ loadTemplates templateDir mempty
                   >>= either error return
    rootUrl <- getSnapletRootURL
    let fullPrefix = U.toString rootUrl </> U.toString urlPrefix
    printInfo $ T.pack $ unwords
        [ "...adding"
        , (show $ length $ templateNames ts)
        , "templates from"
        , templateDir
        , "with route prefix"
        , fullPrefix ++ "/"
        ]
    addPostInitHook $ return . changeTS
        (`mappend` addTemplatePathPrefix (U.fromString fullPrefix) ts)


------------------------------------------------------------------------------
modifyHeistTS' :: (Lens (Snaplet b) (Snaplet (Heist b)))
               -> (HeistState (Handler b b) -> HeistState (Handler b b))
               -> Initializer b v ()
modifyHeistTS' heist f = do
    _lens <- getLens
    withTop' heist $ addPostInitHook $ return . changeTS f


------------------------------------------------------------------------------
modifyHeistTS :: (Lens b (Snaplet (Heist b)))
              -> (HeistState (Handler b b) -> HeistState (Handler b b))
              -> Initializer b v ()
modifyHeistTS heist f = modifyHeistTS' (subSnaplet heist) f


------------------------------------------------------------------------------
withHeistTS' :: (Lens (Snaplet b) (Snaplet (Heist b)))
             -> (HeistState (Handler b b) -> a)
             -> Handler b v a
withHeistTS' heist f = withTop' heist $ gets (f . _heistTS)


------------------------------------------------------------------------------
withHeistTS :: (Lens b (Snaplet (Heist b)))
            -> (HeistState (Handler b b) -> a)
            -> Handler b v a
withHeistTS heist f = withHeistTS' (subSnaplet heist) f


------------------------------------------------------------------------------
addSplices' :: (Lens (Snaplet b) (Snaplet (Heist b)))
            -> [(Text, SnapletSplice b v)]
            -> Initializer b v ()
addSplices' heist splices = do
    _lens <- getLens
    withTop' heist $ addPostInitHook $
        return . changeTS (bindSnapletSplices _lens splices)


------------------------------------------------------------------------------
addSplices :: (Lens b (Snaplet (Heist b)))
           -> [(Text, SnapletSplice b v)]
           -> Initializer b v ()
addSplices heist splices = addSplices' (subSnaplet heist) splices


                            -----------------------
                            -- Handler functions --
                            -----------------------

------------------------------------------------------------------------------
-- | Internal helper function for rendering.
renderHelper :: Maybe MIMEType
             -> ByteString
             -> Handler b (Heist b) ()
renderHelper c t = do
    (Heist ts _) <- get
    withTop' id $ renderTemplate ts t >>= maybe pass serve
  where
    serve (b, mime) = do
        modifyResponse $ setContentType $ fromMaybe mime c
        writeBuilder b


------------------------------------------------------------------------------
render :: ByteString
       -- ^ Name of the template
       -> Handler b (Heist b) ()
render t = renderHelper Nothing t


------------------------------------------------------------------------------
renderAs :: ByteString
         -- ^ Content type
         -> ByteString
         -- ^ Name of the template
         -> Handler b (Heist b) ()
renderAs ct t = renderHelper (Just ct) t


------------------------------------------------------------------------------
heistServe :: Handler b (Heist b) ()
heistServe =
    ifTop (render "index") <|> (render . B.pack =<< getSafePath)


------------------------------------------------------------------------------
heistServeSingle :: ByteString
                 -> Handler b (Heist b) ()
heistServeSingle t =
    render t <|> error ("Template " ++ show t ++ " not found.")


------------------------------------------------------------------------------
heistLocal' :: (Lens (Snaplet b) (Snaplet (Heist b)))
            -> (HeistState (Handler b b) -> HeistState (Handler b b))
            -> Handler b v a
            -> Handler b v a
heistLocal' heist f m = do
    hs  <- withTop' heist get
    withTop' heist $ modify $ changeTS f
    res <- m
    withTop' heist $ put hs
    return res


------------------------------------------------------------------------------
heistLocal :: (Lens b (Snaplet (Heist b)))
           -> (HeistState (Handler b b) -> HeistState (Handler b b))
           -> Handler b v a
           -> Handler b v a
heistLocal heist f m = heistLocal' (subSnaplet heist) f m


------------------------------------------------------------------------------
withSplices' :: (Lens (Snaplet b) (Snaplet (Heist b)))
             -> [(Text, SnapletSplice b v)]
             -> Handler b v a
             -> Handler b v a
withSplices' heist splices m = do
    _lens <- getLens
    heistLocal' heist (bindSnapletSplices _lens splices) m


------------------------------------------------------------------------------
withSplices :: (Lens b (Snaplet (Heist b)))
            -> [(Text, SnapletSplice b v)]
            -> Handler b v a
            -> Handler b v a
withSplices heist splices m = withSplices' (subSnaplet heist) splices m


------------------------------------------------------------------------------
renderWithSplices' :: (Lens (Snaplet b) (Snaplet (Heist b)))
                   -> ByteString
                   -> [(Text, SnapletSplice b v)]
                   -> Handler b v ()
renderWithSplices' heist t splices =
    withSplices' heist splices $ withTop' heist $ render t


------------------------------------------------------------------------------
renderWithSplices :: (Lens b (Snaplet (Heist b)))
                  -> ByteString
                  -> [(Text, SnapletSplice b v)]
                  -> Handler b v ()
renderWithSplices heist t splices =
    renderWithSplices' (subSnaplet heist) t splices


