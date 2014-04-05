{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Snap.Snaplet.Internal.Lensed where


------------------------------------------------------------------------------
import           Control.Applicative         (Alternative (..),
                                              Applicative (..), (<$>))
import           Control.Category            ((.))
import           Control.Lens.Loupe          (ALens', cloneLens, storing, (^#))
import           Control.Monad               (MonadPlus (..), liftM)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Reader        (MonadReader (..))
import           Control.Monad.State.Class   (MonadState (..))
import           Control.Monad.Trans         (MonadIO (..), MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Control.Monad.Trans.State   (StateT(..))
import           Prelude                     (Functor (..), Monad (..), const,
                                              seq, ($), ($!))
import           Snap.Core                   (MonadSnap (..))
------------------------------------------------------------------------------


------------------------------------------------------------------------------
newtype Lensed b v m a = Lensed
    { unlensed :: ALens' b v -> v -> b -> m (a, v, b) }


------------------------------------------------------------------------------
instance Functor m => Functor (Lensed b v m) where
    fmap f (Lensed g) = Lensed $ \l v s ->
        (\(a,v',s') -> (f a, v', s')) <$> g l v s


------------------------------------------------------------------------------
instance (Functor m, Monad m) => Applicative (Lensed b v m) where
    pure a = Lensed $ \_ v s -> return (a, v, s)
    Lensed mf <*> Lensed ma = Lensed $ \l v s -> do
        (f, v', s') <- mf l v s
        (\(a,v'',s'') -> (f a, v'', s'')) <$> ma l v' s'


------------------------------------------------------------------------------
instance Monad m => Monad (Lensed b v m) where
    return a = Lensed $ \_ v s -> return (a, v, s)
    Lensed g >>= k = Lensed $ \l v s -> do
        (a, v', s') <- g l v s
        unlensed (k a) l v' s'


------------------------------------------------------------------------------
instance Monad m => MonadState v (Lensed b v m) where
    get = Lensed $ \_ v s -> return (v, v, s)
    put v' = Lensed $ \_ _ s -> return ((), v', s)


instance Monad m => MonadReader (ALens' b v) (Lensed b v m) where
  ask = Lensed $ \l v s -> return (l, v, s)
  local = lensedLocal

------------------------------------------------------------------------------
lensedLocal :: Monad m => (ALens' b v -> ALens' b v') -> Lensed b v' m a -> Lensed b v m a
lensedLocal f g = do
    l <- ask
    withTop (f l) g

------------------------------------------------------------------------------
instance MonadTrans (Lensed b v) where
    lift m = Lensed $ \_ v b -> do
      res <- m
      return (res, v, b)

------------------------------------------------------------------------------
instance MonadIO m => MonadIO (Lensed b v m) where
  liftIO = lift . liftIO


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (Lensed b v m) where
    mzero = lift mzero
    m `mplus` n = Lensed $ \l v b ->
                  unlensed m l v b `mplus` unlensed n l v b


------------------------------------------------------------------------------
instance (Monad m, Alternative m) => Alternative (Lensed b v m) where
    empty = lift empty
    Lensed m <|> Lensed n = Lensed $ \l v b -> m l v b <|> n l v b


------------------------------------------------------------------------------
instance MonadSnap m => MonadSnap (Lensed b v m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance MonadBase base m => MonadBase base (Lensed b v m) where
    liftBase = lift . liftBase


------------------------------------------------------------------------------
instance MonadBaseControl base m => MonadBaseControl base (Lensed b v m) where
     newtype StM (Lensed b v m) a = StMRS {unStMRS :: ComposeSt (Lensed b v) m a}
     liftBaseWith = defaultLiftBaseWith StMRS
     restoreM = defaultRestoreM unStMRS
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}


------------------------------------------------------------------------------
instance MonadTransControl (Lensed b v) where
    newtype StT (Lensed b v) a = StLensed {unStLensed :: (a, v, b)}
    liftWith f = Lensed $ \l v b -> do
        res <- f $ \(Lensed g) -> liftM StLensed $ g l v b
        return (res, v, b)
    restoreT k = Lensed $ \_ _ _ -> liftM unStLensed k
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


------------------------------------------------------------------------------
globally :: Monad m => StateT b m a -> Lensed b v m a
globally (StateT f) = Lensed $ \l v s ->
                      liftM (\(a, s') -> (a, s' ^# l, s')) $ f (storing l v s)


------------------------------------------------------------------------------
lensedAsState :: Monad m => Lensed b v m a -> ALens' b v -> StateT b m a
lensedAsState (Lensed f) l = StateT $ \s -> do
    (a, v', s') <- f l (s ^# l) s
    return (a, storing l v' s')


------------------------------------------------------------------------------
getBase :: Monad m => Lensed b v m b
getBase = Lensed $ \_ v b -> return (b, v, b)


------------------------------------------------------------------------------
withTop :: Monad m => ALens' b v' -> Lensed b v' m a -> Lensed b v m a
withTop l m = globally $ lensedAsState m l


------------------------------------------------------------------------------
with :: Monad m => ALens' v v' -> Lensed b v' m a -> Lensed b v m a
with l g = do
    l' <- ask
    withTop (cloneLens l' . l) g


------------------------------------------------------------------------------
embed :: Monad m => ALens' v v' -> Lensed v v' m a -> Lensed b v m a
embed l m = locally $ lensedAsState m l


------------------------------------------------------------------------------
locally :: Monad m => StateT v m a -> Lensed b v m a
locally (StateT f) = Lensed $ \_ v s ->
                     liftM (\(a, v') -> (a, v', s)) $ f v


------------------------------------------------------------------------------
runLensed :: Monad m
          => Lensed t1 b m t
          -> ALens' t1 b
          -> t1
          -> m (t, t1)
runLensed (Lensed f) l s = do
    (a, v', s') <- f l (s ^# l) s
    return (a, storing l v' s')
