{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Snap.Snaplet.HeistNoClass
  ( Heist
  , heistInit
  , clearHeistCache

  , addTemplates
  , addTemplatesAt
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
  , liftHandler
  , liftWith
  , bindSnapletSplices
  ) where

import           Prelude hiding ((.), id)
import           Control.Arrow
import           Control.Applicative
import           Control.Category
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
data Heist b = Heist
    { _heistTS       :: TemplateState (Handler b b)
    , _heistCTS      :: CacheTagState
    }


------------------------------------------------------------------------------
changeTS :: (TemplateState (Handler a a) -> TemplateState (Handler a a))
         -> Heist a
         -> Heist a
changeTS f (Heist ts cts) = Heist (f ts) cts


------------------------------------------------------------------------------
-- | Clears data stored by the cache tag.  The cache tag automatically reloads
-- its data when the specified TTL expires, but sometimes you may want to
-- trigger a manual reload.  This function lets you do that.
clearHeistCache :: Heist b -> IO ()
clearHeistCache = clearCacheTagState . _heistCTS


------------------------------------------------------------------------------
-- SnapletSplice functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Monad for working with Heist's API from within a snaplet.
newtype SnapletHeist b v a = SnapletHeist
    (ReaderT (Lens (Snaplet b) (Snaplet v)) (HeistT (Handler b b)) a)
  deriving ( Monad
           , Functor
           , Applicative
           , Alternative
           , MonadIO
           , MonadPlus
           , MonadReader (Lens (Snaplet b) (Snaplet v))
           )


------------------------------------------------------------------------------
-- | Type alias for convenience.
type SnapletSplice b v = SnapletHeist b v Template


------------------------------------------------------------------------------
-- | Runs the SnapletSplice.
runSnapletSplice :: (Lens (Snaplet b) (Snaplet v))
                 -> SnapletHeist b v a
                 -> HeistT (Handler b b) a
runSnapletSplice l (SnapletHeist m) = runReaderT m l


withSS :: (Lens (Snaplet b) (Snaplet v) -> Lens (Snaplet b) (Snaplet v'))
       -> SnapletHeist b v' a
       -> SnapletHeist b v a
withSS f (SnapletHeist m) = SnapletHeist $ withReaderT f m


------------------------------------------------------------------------------
-- | Lifts a HeistT action into SnapletHeist.  Use this with all the functions
-- from the Heist API.
liftHeist :: HeistT (Handler b b) a -> SnapletHeist b v a
liftHeist = SnapletHeist . lift


------------------------------------------------------------------------------
-- | Lifts a Handler into SnapletHeist.
liftHandler :: Handler b b a -> SnapletHeist b v a
liftHandler = liftHeist . lift


------------------------------------------------------------------------------
-- | Common idiom for the combination of liftHandler and withTop.
liftWith :: (Lens (Snaplet b) (Snaplet v'))
         -> Handler b v' a
         -> SnapletHeist b v a
liftWith l = liftHandler . withTop' l


instance MonadState (Snaplet v) (SnapletHeist b v) where
    get = do
        l <- ask
        b <- liftHandler get
        return $ getL l b
    put s = do
        l <- ask
        b <- liftHandler get
        liftHandler $ put $ setL l s b


------------------------------------------------------------------------------
-- | MonadSnaplet instance gives us access to the snaplet infrastructure.
instance MonadSnaplet SnapletHeist where
    getLens = ask
    with' l = withSS (l .)
    withTop' l = withSS (const id) . with' l
    getOpaqueConfig = do
        l <- ask
        b <- liftHandler get
        return $ getL (snapletConfig . l) b


------------------------------------------------------------------------------
-- | SnapletSplices version of bindSplices.
bindSnapletSplices :: (Lens (Snaplet b) (Snaplet v))
                   -> [(Text, SnapletSplice b v)]
                   -> TemplateState (Handler b b)
                   -> TemplateState (Handler b b)
bindSnapletSplices l splices =
    bindSplices $ map (second $ runSnapletSplice l) splices


------------------------------------------------------------------------------
-- Initializer functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | The 'Initializer' for 'Heist'.
heistInit :: FilePath
          -> SnapletInit b (Heist b)
heistInit templateDir =
    makeSnaplet "heist" "" Nothing $ liftIO $ do
        (cacheFunc, cts) <- mkCacheTag
        let origTs = cacheFunc emptyTemplateState
        ts <- loadTemplates templateDir origTs >>= either error return
        return $ Heist ts cts


addTemplates :: ByteString -> Initializer b (Heist b) ()
addTemplates urlPrefix = do
    snapletPath <- getSnapletFilePath
    addTemplatesAt urlPrefix (snapletPath </> "templates")


addTemplatesAt :: ByteString
               -> FilePath
               -> Initializer b (Heist b) ()
addTemplatesAt urlPrefix templateDir = do
    printInfo $ T.pack $ "Adding templates from "++ templateDir ++
                        " with route prefix " ++ (U.toString urlPrefix) ++ "/"
    ts <- liftIO $ loadTemplates templateDir emptyTemplateState
                   >>= either error return
    addPostInitHook $ return . changeTS
        (`mappend` addTemplatePathPrefix urlPrefix ts)


addSplices' :: (Lens (Snaplet b) (Snaplet (Heist b)))
            -> [(Text, SnapletSplice b v)]
            -> Initializer b v ()
addSplices' heist splices = do
    _lens <- getLens
    withTop' heist $ addPostInitHook $
        return . changeTS (bindSnapletSplices _lens splices)


addSplices :: (Lens b (Snaplet (Heist b)))
           -> [(Text, SnapletSplice b v)]
           -> Initializer b v ()
addSplices heist splices = addSplices' (subSnaplet heist) splices


------------------------------------------------------------------------------
-- Handler functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Internal helper function for rendering.
renderHelper :: Maybe MIMEType
             -> ByteString
             -> Handler b (Heist b) ()
renderHelper c t = do
    (Heist ts _) <- getSnapletState
    withTop' id $ renderTemplate ts t >>= maybe pass serve
  where
    serve (b, mime) = do
        modifyResponse $ setContentType $ fromMaybe mime c
        writeBuilder b


render :: ByteString
       -- ^ Name of the template
       -> Handler b (Heist b) ()
render t = renderHelper Nothing t


renderAs :: ByteString
         -- ^ Content type
         -> ByteString
         -- ^ Name of the template
         -> Handler b (Heist b) ()
renderAs ct t = renderHelper (Just ct) t


heistServe :: Handler b (Heist b) ()
heistServe =
    ifTop (render "index") <|> (render . B.pack =<< getSafePath)


heistServeSingle :: ByteString
                 -> Handler b (Heist b) ()
heistServeSingle t =
    render t <|> error ("Template " ++ show t ++ " not found.")


heistLocal' :: (Lens (Snaplet b) (Snaplet (Heist b)))
            -> (TemplateState (Handler b b) -> TemplateState (Handler b b))
            -> Handler b v a
            -> Handler b v a
heistLocal' heist f m = do
    hs  <- withTop' heist $ getSnapletState
    withTop' heist $ modifySnapletState $ changeTS f
    res <- m
    withTop' heist $ putSnapletState hs
    return res


heistLocal :: (Lens b (Snaplet (Heist b)))
           -> (TemplateState (Handler b b) -> TemplateState (Handler b b))
           -> Handler b v a
           -> Handler b v a
heistLocal heist f m = heistLocal' (subSnaplet heist) f m


withSplices' :: (Lens (Snaplet b) (Snaplet (Heist b)))
             -> [(Text, SnapletSplice b v)]
             -> Handler b v a
             -> Handler b v a
withSplices' heist splices m = do
    _lens <- getLens
    heistLocal' heist (bindSnapletSplices _lens splices) m


withSplices :: (Lens b (Snaplet (Heist b)))
            -> [(Text, SnapletSplice b v)]
            -> Handler b v a
            -> Handler b v a
withSplices heist splices m = withSplices' (subSnaplet heist) splices m


renderWithSplices' :: (Lens (Snaplet b) (Snaplet (Heist b)))
                   -> ByteString
                   -> [(Text, SnapletSplice b v)]
                   -> Handler b v ()
renderWithSplices' heist t splices =
    withSplices' heist splices $ withTop' heist $ render t


renderWithSplices :: (Lens b (Snaplet (Heist b)))
                  -> ByteString
                  -> [(Text, SnapletSplice b v)]
                  -> Handler b v ()
renderWithSplices heist t splices =
    renderWithSplices' (subSnaplet heist) t splices


