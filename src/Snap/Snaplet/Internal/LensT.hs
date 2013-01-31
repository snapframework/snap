{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Snap.Snaplet.Internal.LensT where

import           Control.Applicative
import           Control.Category
import           Control.Lens.Loupe
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Prelude hiding ((.), id, catch)
import           Snap.Core

import           Snap.Snaplet.Internal.RST


newtype LensT b v s m a = LensT (RST (ALens' b v) s m a)
  deriving ( Monad
           , MonadTrans
           , Functor
           , Applicative
           , MonadIO
           , MonadPlus
           , MonadCatchIO
           , Alternative
           , MonadReader (ALens' b v)
           , MonadSnap )


------------------------------------------------------------------------------
instance Monad m => MonadState v (LensT b v b m) where
    get = lGet
    put = lPut


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

