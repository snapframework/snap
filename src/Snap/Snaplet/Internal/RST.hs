{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Snap.Snaplet.Internal.RST where

import           Control.Applicative
import           Control.Category
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Prelude hiding ((.), id, catch)
import           Snap.Core


------------------------------------------------------------------------------
-- like RWST, but no writer to bog things down. Also assured strict, inlined
-- monad bind, etc
newtype RST r s m a = RST { runRST :: r -> s -> m (a, s) }


evalRST :: Monad m => RST r s m a -> r -> s -> m a
evalRST m r s = do
    (a,_) <- runRST m r s
    return a
{-# INLINE evalRST #-}


execRST :: Monad m => RST r s m a -> r -> s -> m s
execRST m r s = do
    (_,!s') <- runRST m r s
    return s'
{-# INLINE execRST #-}


withRST :: Monad m => (r' -> r) -> RST r s m a -> RST r' s m a
withRST f m = RST $ \r' s -> runRST m (f r') s
{-# INLINE withRST #-}


instance (Monad m) => MonadReader r (RST r s m) where
    ask = RST $ \r s -> return (r,s)
    local f m = RST $ \r s -> runRST m (f r) s


instance (Functor m) => Functor (RST r s m) where
    fmap f m = RST $ \r s -> fmap (\(a,s') -> (f a, s')) $ runRST m r s


instance (Functor m, Monad m) => Applicative (RST r s m) where
    pure = return
    (<*>) = ap


instance (Functor m, MonadPlus m) => Alternative (RST r s m) where
    empty = mzero
    (<|>) = mplus


instance (Monad m) => MonadState s (RST r s m) where
    get   = RST $ \_ s -> return (s,s)
    put x = RST $ \_ _ -> return ((),x)


mapRST :: (m (a, s) -> n (b, s)) -> RST r s m a -> RST r s n b
mapRST f m = RST $ \r s -> f (runRST m r s)


instance (MonadCatchIO m) => MonadCatchIO (RST r s m) where
    m `catch` f = RST $ \r s -> runRST m r s
                           `catch` \e -> runRST (f e) r s
    block       = mapRST block
    unblock     = mapRST unblock

instance (MonadSnap m) => MonadSnap (RST r s m) where
    liftSnap s = lift $ liftSnap s

rwsBind :: Monad m =>
           RST r s m a
        -> (a -> RST r s m b)
        -> RST r s m b
rwsBind m f = RST go
  where
    go r !s = do
        (a, !s')  <- runRST m r s
        runRST (f a) r s'
{-# INLINE rwsBind #-}

instance (Monad m) => Monad (RST r s m) where
    return a = RST $ \_ s -> return (a, s)
    (>>=)    = rwsBind
    fail msg = RST $ \_ _ -> fail msg


instance (MonadPlus m) => MonadPlus (RST r s m) where
    mzero       = RST $ \_ _ -> mzero
    m `mplus` n = RST $ \r s -> runRST m r s `mplus` runRST n r s


instance MonadTrans (RST r s) where
    lift m = RST $ \_ s -> do
        a <- m
        return $ s `seq` (a, s)

instance (MonadIO m) => MonadIO (RST r s m) where
    liftIO = lift . liftIO



