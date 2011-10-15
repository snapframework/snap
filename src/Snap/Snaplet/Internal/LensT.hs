{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Snap.Snaplet.Internal.LensT where

import           Control.Applicative
import           Control.Category
import           Control.Monad.IO.Control
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Data.Lens.Lazy
import           Prelude hiding ((.), id, catch)
import           Snap.Core

import           Snap.Snaplet.Internal.RST


newtype LensT b v s m a = LensT (RST (Lens b v) s m a)
  deriving ( Monad
           , MonadTrans
           , Functor
           , Applicative
           , MonadIO
           , MonadPlus
           , MonadControlIO
           , Alternative
           , MonadReader (Lens b v)
           , MonadSnap )


------------------------------------------------------------------------------
instance (Monad m) => MonadState v (LensT b v b m) where
    get = lGet
    put = lPut


------------------------------------------------------------------------------
getBase :: (Monad m) => LensT b v s m s
getBase = LensT get
{-# INLINE getBase #-}


------------------------------------------------------------------------------
putBase :: (Monad m) => s -> LensT b v s m ()
putBase = LensT . put
{-# INLINE putBase #-}


------------------------------------------------------------------------------
lGet :: (Monad m) => LensT b v b m v
lGet = LensT $ do
           !l <- ask
           !b <- get
           return $! l ^$ b
{-# INLINE lGet #-}


------------------------------------------------------------------------------
lPut :: (Monad m) => v -> LensT b v b m ()
lPut v = LensT $ do
             !l <- ask
             !b <- get
             put $! (l ^!= v) b
{-# INLINE lPut #-}


------------------------------------------------------------------------------
runLensT :: (Monad m) =>
            LensT b v s m a
         -> Lens b v
         -> s
         -> m (a, s)
runLensT (LensT m) = runRST m
{-# INLINE runLensT #-}


------------------------------------------------------------------------------
withLensT :: Monad m =>
             ((Lens b' v') -> (Lens b v))
          -> LensT b v s m a
          -> LensT b' v' s m a
withLensT f (LensT m) = LensT $ withRST f m
{-# INLINE withLensT #-}


------------------------------------------------------------------------------
withTop :: Monad m
        => (Lens b v')
        -> LensT b v' s m a
        -> LensT b v  s m a
withTop !subLens = withLensT (const subLens)
{-# INLINE withTop #-}


------------------------------------------------------------------------------
with :: Monad m
     => (Lens v v')
     -> LensT b v' s m a
     -> LensT b v  s m a
with !subLens = withLensT (subLens .)
{-# INLINE with #-}


