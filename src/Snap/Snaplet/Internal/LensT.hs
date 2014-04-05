{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Snap.Snaplet.Internal.LensT where


------------------------------------------------------------------------------
import           Control.Applicative         (Alternative (..),
                                              Applicative (..))
import           Control.Category            ((.))
import           Control.Lens.Loupe          (ALens', cloneLens, storing, (^#))
import           Control.Monad               (MonadPlus (..))
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Reader        (MonadReader (..))
import           Control.Monad.State.Class   (MonadState (..))
import           Control.Monad.Trans         (MonadIO (..), MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultLiftWith, defaultRestoreM,
                                              defaultRestoreT)
import           Prelude                     (Functor (..), Monad (..), const,
                                              seq, ($), ($!))
import           Snap.Core                   (MonadSnap (..))
import           Snap.Snaplet.Internal.RST   (RST (..), runRST, withRST)
------------------------------------------------------------------------------


newtype LensT b v s m a = LensT (RST (ALens' b v) s m a)
  deriving ( Monad
           , MonadTrans
           , Functor
           , Applicative
           , MonadIO
           , MonadPlus
           , Alternative
           , MonadReader (ALens' b v))


------------------------------------------------------------------------------
instance Monad m => MonadState v (LensT b v b m) where
    get = lGet
    put = lPut


instance MonadBase bs m => MonadBase bs (LensT b v s m) where
    liftBase = lift . liftBase


instance MonadBaseControl bs m => MonadBaseControl bs (LensT b v s m) where
     newtype StM (LensT b v s m) a = StMLens {unStMLens :: ComposeSt (LensT b v s) m a}
     liftBaseWith = defaultLiftBaseWith StMLens
     restoreM = defaultRestoreM unStMLens
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}


instance MonadTransControl (LensT b v s) where
    newtype StT (LensT b v s) a = StLensT {unStLensT :: StT (RST (ALens' b v) s) a}
    liftWith = defaultLiftWith LensT (\(LensT rst) -> rst) StLensT
    restoreT = defaultRestoreT LensT unStLensT
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


instance MonadSnap m => MonadSnap (LensT b v s m) where
    liftSnap m = LensT $ liftSnap m


------------------------------------------------------------------------------
getBase :: Monad m => LensT b v s m s
getBase = LensT get
{-# INLINE getBase #-}


------------------------------------------------------------------------------
putBase :: Monad m => s -> LensT b v s m ()
putBase = LensT . put
{-# INLINE putBase #-}


------------------------------------------------------------------------------
lGet :: Monad m => LensT b v b m v
lGet = LensT $ do
           !l <- ask
           !b <- get
           return $! b ^# l
{-# INLINE lGet #-}


------------------------------------------------------------------------------
lPut :: Monad m => v -> LensT b v b m ()
lPut v = LensT $ do
             !l <- ask
             !b <- get
             put $! storing l v b
{-# INLINE lPut #-}


------------------------------------------------------------------------------
runLensT :: Monad m => LensT b v s m a -> ALens' b v -> s -> m (a, s)
runLensT (LensT m) l = runRST m l
{-# INLINE runLensT #-}


------------------------------------------------------------------------------
withLensT :: Monad m
          => (ALens' b' v' -> ALens' b v)
          -> LensT b v s m a
          -> LensT b' v' s m a
withLensT f (LensT m) = LensT $ withRST f m
{-# INLINE withLensT #-}


------------------------------------------------------------------------------
withTop :: Monad m
        => ALens' b v'
        -> LensT b v' s m a
        -> LensT b v  s m a
withTop subLens = withLensT (const subLens)
{-# INLINE withTop #-}


------------------------------------------------------------------------------
with :: Monad m => ALens' v v' -> LensT b v' s m a -> LensT b v s m a
with subLens = withLensT (\l -> cloneLens l . subLens)

