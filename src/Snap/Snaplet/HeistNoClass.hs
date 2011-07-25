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
import           Data.Record.Label
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath.Posix
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Cache

import           Snap.Snaplet
-- TODO: It shouldn't be necessary to import this internal module.
import           Snap.Snaplet.Internal.Types
import           Snap.Types
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
-- | 
newtype SnapletHeist b e a = SnapletHeist
    (ReaderT (Snaplet b :-> Snaplet e) (HeistT (Handler b b)) a)
  deriving ( Monad
           , Functor
           , Applicative
           , Alternative
           , MonadIO
           , MonadPlus
           , MonadReader (Snaplet b :-> Snaplet e)
           )


type SnapletSplice b e = SnapletHeist b e Template


------------------------------------------------------------------------------
-- | Runs the SnapletSplice.
runSnapletSplice :: (Snaplet b :-> Snaplet e)
                 -> SnapletHeist b e a
                 -> HeistT (Handler b b) a
runSnapletSplice l (SnapletHeist m) = runReaderT m l


withSS :: ((Snaplet b :-> Snaplet e) -> (Snaplet b :-> Snaplet e'))
       -> SnapletHeist b e' a
       -> SnapletHeist b e a
withSS f (SnapletHeist m) = SnapletHeist $ withReaderT f m


------------------------------------------------------------------------------
-- | Lifts a HeistT action into SnapletHeist.
liftHeist :: HeistT (Handler b b) a -> SnapletHeist b e a
liftHeist = SnapletHeist . lift


------------------------------------------------------------------------------
-- | Lifts a Handler into SnapletHeist.
liftHandler :: Handler b b a -> SnapletHeist b e a
liftHandler = liftHeist . lift


------------------------------------------------------------------------------
-- | Common idiom for the combination of liftHandler and withSibling.
liftWith :: (Snaplet b :-> Snaplet e')
         -> Handler b e' a
         -> SnapletHeist b e a
liftWith l = liftHandler . withSibling' l


instance MonadState e (SnapletHeist b e) where
    get = do
        l <- ask
        b <- liftHandler lhGet
        return $ _value $ getL l b
    put s = do
        l <- ask
        b <- liftHandler lhGet
        liftHandler $ lhPut $ setL l (b { _value = s}) b


instance MonadSnaplet SnapletHeist where
    getLens = ask
    withBase = withSS (const id)
    withChild' l = withSS (l .)
    getSnapletAncestry = liftHandler getSnapletAncestry
    getSnapletFilePath = liftHandler getSnapletFilePath
    getSnapletName = liftHandler getSnapletName
    getSnapletDescription = liftHandler getSnapletDescription
    getSnapletConfig = liftHandler getSnapletConfig
    getSnapletRootURL = liftHandler getSnapletRootURL


------------------------------------------------------------------------------
-- | SnapletSplices version of bindSplices.
bindSnapletSplices :: (Snaplet b :-> Snaplet e)
                   -> [(Text, SnapletSplice b e)]
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
          -> Initializer b e (Snaplet (Heist b))
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


addSplices' :: (Snaplet e :-> Snaplet (Heist b))
           -> [(Text, SnapletSplice b e)]
           -> Initializer b e ()
addSplices' heist splices = do
    _lens <- getLens
    withChild' heist $ addPostInitHook $
        return . changeTS (bindSnapletSplices _lens splices)


addSplices :: (e :-> Snaplet (Heist b))
           -> [(Text, SnapletSplice b e)]
           -> Initializer b e ()
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
    (Heist ts _) <- get
    withBase $ renderTemplate ts t >>= maybe pass serve
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


heistLocal' :: (Snaplet e :-> Snaplet (Heist b))
            -> (TemplateState (Handler b b) -> TemplateState (Handler b b))
            -> Handler b e a
            -> Handler b e a
heistLocal' heist f m = do
    hs  <- withChild' heist $ get
    withChild' heist $ modify $ changeTS f
    res <- m
    withChild' heist $ put hs
    return res


heistLocal :: (e :-> Snaplet (Heist b))
           -> (TemplateState (Handler b b) -> TemplateState (Handler b b))
           -> Handler b e a
           -> Handler b e a
heistLocal heist f m = heistLocal' (subSnaplet heist) f m


withSplices' :: (Snaplet e :-> Snaplet (Heist b))
             -> [(Text, SnapletSplice b e)]
             -> Handler b e a
             -> Handler b e a
withSplices' heist splices m = do
    _lens <- getLens
    heistLocal' heist (bindSnapletSplices _lens splices) m


withSplices :: (e :-> Snaplet (Heist b))
            -> [(Text, SnapletSplice b e)]
            -> Handler b e a
            -> Handler b e a
withSplices heist splices m = withSplices' (subSnaplet heist) splices m


renderWithSplices' :: (Snaplet e :-> Snaplet (Heist b))
                   -> ByteString
                   -> [(Text, SnapletSplice b e)]
                   -> Handler b e ()
renderWithSplices' heist t splices =
    withSplices' heist splices $ withChild' heist $ render t


renderWithSplices :: (e :-> Snaplet (Heist b))
                  -> ByteString
                  -> [(Text, SnapletSplice b e)]
                  -> Handler b e ()
renderWithSplices heist t splices =
    renderWithSplices' (subSnaplet heist) t splices


