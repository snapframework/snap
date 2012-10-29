{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Snap.Snaplet.Internal.LensT where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Prelude hiding ((.), id, catch)
import           Snap.Core

import           Snap.Snaplet.Internal.RST


newtype LensT b v s m a = LensT (RST (SimpleReifiedLens b v) s m a)
  deriving ( Monad
           , MonadTrans
           , Functor
           , Applicative
           , MonadIO
           , MonadPlus
           , MonadCatchIO
           , Alternative
           , MonadReader (SimpleReifiedLens b v)
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
           return $! b ^. reflectLens l
{-# INLINE lGet #-}


------------------------------------------------------------------------------
lPut :: (Monad m) => v -> LensT b v b m ()
lPut v = LensT $ do
             !l <- ask
             !b <- get
             put $! set (reflectLens l) v b
{-# INLINE lPut #-}


------------------------------------------------------------------------------
runLensT :: Monad m => LensT b v s m a -> SimpleLens b v -> s -> m (a, s)
runLensT (LensT m) l = runRST m (ReifyLens l)
{-# INLINE runLensT #-}


------------------------------------------------------------------------------
withLensT :: Monad m
          => ((SimpleReifiedLens b' v') -> (SimpleReifiedLens b v))
          -> LensT b v s m a
          -> LensT b' v' s m a
withLensT f (LensT m) = LensT $ withRST f m
{-# INLINE withLensT #-}


------------------------------------------------------------------------------
withTop :: Monad m
        => (SimpleLens b v')
        -> LensT b v' s m a
        -> LensT b v  s m a
withTop subLens = withLensT (const (ReifyLens subLens))
{-# INLINE withTop #-}


------------------------------------------------------------------------------
with :: Monad m => SimpleLens v v' -> LensT b v' s m a -> LensT b v s m a
with subLens = withLensT (\l -> ReifyLens $ reflectLens l . subLens)

