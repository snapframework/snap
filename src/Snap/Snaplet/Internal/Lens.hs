{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}

module Snap.Snaplet.Internal.Lens where

import           Control.Applicative
import           Control.Category
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Data.Record.Label
import           Prelude hiding ((.), id, catch)
import           Snap.Types

import           Snap.Snaplet.Internal.RST


newtype LensT b e s m a = LensT (RST (b :-> e) s m a)
  deriving ( Monad
           , MonadTrans
           , Functor
           , Applicative
           , MonadIO
           , MonadPlus
           , MonadCatchIO
           , Alternative
           , MonadReader (b :-> e)
           , MonadSnap )


------------------------------------------------------------------------------
instance (Monad m) => MonadState e (LensT b e b m) where
    get = lGet
    put = lPut


------------------------------------------------------------------------------
getBase :: (Monad m) => LensT b e s m s
getBase = LensT get
{-# INLINE getBase #-}


------------------------------------------------------------------------------
putBase :: (Monad m) => s -> LensT b e s m ()
putBase = LensT . put
{-# INLINE putBase #-}


------------------------------------------------------------------------------
lGet :: (Monad m) => LensT b e b m e
lGet = LensT $ do
           !l <- ask
           !b <- get
           return $! getL l b
{-# INLINE lGet #-}


------------------------------------------------------------------------------
lPut :: (Monad m) => e -> LensT b e b m ()
lPut e = LensT $ do
             !l <- ask
             !b <- get
             put $! setL l e b
{-# INLINE lPut #-}


------------------------------------------------------------------------------
runLensT :: (Monad m) =>
            LensT b e s m a
         -> b :-> e
         -> s
         -> m (a, s)
runLensT (LensT m) = runRST m
{-# INLINE runLensT #-}


------------------------------------------------------------------------------
withLens :: Monad m =>
            (e :-> e')
         -> LensT b e' s m a
         -> LensT b e  s m a
withLens !subLens = withLensT (subLens .)
{-# INLINE withLens #-}


------------------------------------------------------------------------------
withLensT :: Monad m =>
             ((b' :-> e') -> (b :-> e))
          -> LensT b e s m a
          -> LensT b' e' s m a
withLensT f (LensT m) = LensT $ withRST f m
{-# INLINE withLensT #-}


------------------------------------------------------------------------------
modLens :: (b :-> e) -> LensT b e s m a -> LensT b' e' s m a
modLens l (LensT m) = LensT $ modR l m
{-# INLINE modLens #-}


------------------------------------------------------------------------------
downcast :: (Monad m) =>
            LensT b b s m a
         -> LensT b e s m a
downcast (LensT m) = LensT $ RST $ \_ -> runRST m id

