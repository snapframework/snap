{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Heist.Impl' is an implementation of the 'MonadHeist'
interface defined in 'Snap.Extension.Heist'.

As always, to use, add 'HeistState' to your application's state, along with an
instance of 'HasHeistState' for your application's state, making sure to
use a 'heistInitializer' in your application's 'Initializer', and then you're
ready to go.

'Snap.Extension.Heist.Impl' is a little different to other Snap Extensions,
which is unfortunate as it is probably the most widely useful one. As
explained below, 'HeistState' takes your application's monad as a type
argument, and 'HasHeistState' is a multi-parameter type class, the additional
first parameter also being your application's monad.

Two instances of 'MonadHeist' are provided with this module. One is designed
for users wanting to use Heist templates with their application, the other is
designed for users writing Snap Extensions which use their own Heist templates
internally.

The first one of these instances is
@HasHeistState (SnapExtend s) s => MonadHeist (SnapExtend s) (SnapExtend s)@.
This means that any type @s@ which has a 'HeistState', whose
'TemplateState'\'s monad is @SnapExtend s@ forms a 'MonadHeist' whose
'TemplateState'\'s monad is @SnapExtend s@ and whose monad itself is
@SnapExtend s@. The @s@ here is your application's state, and @SnapExtend s@
is your application's monad.

The second one of these instances is
@HasHeistState m s => MonadHeist m (ReaderT s m)@. This means that any type
@s@ which has, for any m, a @HeistState m@, forms a 'MonadHeist', whose
'TemplateState'\'s monad is @m@, when made the environment of
a 'ReaderT' wrapped around @m@. The @s@ here would be the Snap Extension's
internal state, and the @m@ would be 'SnapExtend' wrapped around any @s'@
which was an instance of the Snap Extension's @HasState@ class.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Snap.Extension.Heist.Impl
  ( 
  
    -- * Heist State Definitions
    HeistState
  , HasHeistState(..)
  , heistInitializer

    -- * The MonadHeist Interface
  , MonadHeist(..)

    -- * Convenience Functions
  , registerSplices
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import qualified Data.ByteString as B
import           Snap.Extension
import           Snap.Extension.Heist
import           Snap.Types
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


------------------------------------------------------------------------------
-- | Your application's state must include a 'HeistState' in order for your
-- application to be a 'MonadHeist'.  
--
-- Unlike other @-State@ types, this is of kind @(* -> *) -> *@. Unless you're
-- developing your own Snap Extension which has its own internal 'HeistState',
-- the type argument you want to pass to 'HeistState' is your application's
-- monad, which should be 'SnapExtend' wrapped around your application's
-- state.
data MonadSnap m => HeistState m = HeistState
    { _path     :: FilePath
    , _origTs   :: TemplateState m
    , _tsMVar   :: MVar (TemplateState m)
    , _sts      :: StaticTagState
    , _modifier :: TemplateState m -> TemplateState m
    }


------------------------------------------------------------------------------
-- | For you appliaction's monad to be a 'MonadHeist', your application's
-- state needs to be an instance of 'HasHeistState'. Minimal complete
-- definition: 'getHeistState', 'setHeistState'.
--
-- Unlike other @HasState@ type classes, this is a type class has two
-- parameters. Among other things, this means that you will need to enable the
-- @FlexibleInstances@ and @MultiParameterTypeClasses@ language extensions to
-- be able to create an instance of @HasHeistState@. In most cases, the last
-- parameter will as usual be your application's state, and the additional
-- first one will be the monad formed by wrapping 'SnapExtend' around your
-- application's state.
--
-- However, if you are developing your own Snap Extension which uses its own
-- internal 'HeistState', the last parameter will be your Snap Extension's
-- internal state, and the additional first parameter will be any monad formed
-- by wrapping @SnapExtend@ around a type which has an instance of the
-- @HasState@ class for your monad. These two use cases are subtly different,
-- which is why 'HasHeistState' needs two type parameters.
class MonadSnap m => HasHeistState m s | s -> m where
    getHeistState :: s -> HeistState m
    setHeistState :: HeistState m -> s -> s

    modifyHeistState :: (HeistState m -> HeistState m) -> s -> s
    modifyHeistState f s = setHeistState (f $ getHeistState s) s


------------------------------------------------------------------------------
-- | The 'Initializer' for 'HeistState'. It takes one argument, a path to a
-- template directory containing @.tpl@ files.
heistInitializer :: MonadSnap m => FilePath -> Initializer (HeistState m)
heistInitializer p = do
    heistState <- liftIO $ do
        (origTs,sts) <- bindStaticTag $ emptyTemplateState p
        loadTemplates p origTs >>= either error (\ts -> do
            tsMVar <- newMVar ts
            return $ HeistState p origTs tsMVar sts id)
    mkInitializer heistState


------------------------------------------------------------------------------
instance MonadSnap m => InitializerState (HeistState m) where
    extensionId = const "Heist/Impl"
    mkCleanup   = const $ return ()
    mkReload (HeistState p origTs tsMVar sts _) = do
        clearStaticTagCache $ sts
        either error (modifyMVar_ tsMVar . const . return) =<<
            loadTemplates p origTs


------------------------------------------------------------------------------
instance HasHeistState (SnapExtend s) s => MonadHeist (SnapExtend s) (SnapExtend s) where
    render = renderAs "text/html; charset=utf-8"

    renderAs c t = do
        (HeistState _ _ tsMVar _ modifier) <- asks getHeistState
        ts <- liftIO $ fmap modifier $ readMVar tsMVar
        renderTemplate ts t >>= maybe pass (\ctnt -> do
            modifyResponse $ setContentType c
            modifyResponse $ setContentLength (fromIntegral $ B.length ctnt)
            writeBS ctnt)

    heistLocal f = local $ modifyHeistState $ \s ->
        s { _modifier = f . _modifier s }


------------------------------------------------------------------------------
instance HasHeistState m s => MonadHeist m (ReaderT s m) where
    render = renderAs "text/html; charset=utf-8"

    renderAs c t = ReaderT $ \s -> do
        let (HeistState _ _ tsMVar _ modifier) = getHeistState s
        ts <- liftIO $ fmap modifier $ readMVar tsMVar
        renderTemplate ts t >>= maybe pass (\ctnt -> do
            modifyResponse $ setContentType c
            modifyResponse $ setContentLength (fromIntegral $ B.length ctnt)
            writeBS ctnt)

    heistLocal f = local $ modifyHeistState $ \s ->
        s { _modifier = f . _modifier s }


------------------------------------------------------------------------------
-- | Take your application's state and register these splices in it so
-- that you don't have to re-list them in every handler. Should be called from
-- inside your application's 'Initializer'.
--
-- Typical use cases are dynamically generated components that are present in
-- many of your views. 
--
-- Example Usage:
--
-- @
-- appInit :: Initializer AppState
-- appInit = do
--  hs <- heistInitializer \"templates\"
--  registerSplices hs $ 
--   [ (\"tabs\", tabsSplice)
--   , (\"loginLogout\", loginLogoutSplice) ] 
-- @
registerSplices
  :: (MonadSnap m, MonadIO n) 
  => HeistState m   
  -- ^ Heist state that you are going to embed in your application's state.
  -> [(B.ByteString, Splice m)]   
  -- ^ Your splices.
  -> n ()
registerSplices s sps = liftIO $ do
  let mv = _tsMVar s
  modifyMVar_ mv $ (return . bindSplices sps) 
