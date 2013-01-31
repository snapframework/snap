{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Snap.Snaplet.Internal.Lensed where

import Control.Applicative
import Control.Lens.Loupe
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.CatchIO
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Category
import Prelude hiding (catch, id, (.))
import Snap.Core

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
instance MonadCatchIO m => MonadCatchIO (Lensed b v m) where
    catch (Lensed m) f = Lensed $ \l v b -> m l v b `catch` handler l v b
      where
        handler l v b e = let Lensed h = f e
                          in h l v b

    block (Lensed m)   = Lensed $ \l v b -> block (m l v b)
    unblock (Lensed m) = Lensed $ \l v b -> unblock (m l v b)


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
