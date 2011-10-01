{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Generic
  ( GenericT
  , runGenericT
  , override
  , makeData
  , runData
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Data
import           Data.Map (Map)
import qualified Data.Map as Map

------------------------------------------------------------------------------
newtype GenericT conf m a = G (StateT (GenericMap conf m) m a)
  deriving (Monad, Functor, Applicative, Alternative, MonadPlus)


------------------------------------------------------------------------------
instance (MonadError e m) => MonadError e (GenericT c m) where
    throwError = G . throwError
    catchError (G m) h = G $! m `catchError` h'
      where
        h' e = let (G n) = h e in n


------------------------------------------------------------------------------
type GenericMap conf m = Map ByteString (DataType, conf)


------------------------------------------------------------------------------
runGenericT :: (Monad m) =>
               GenericT conf m a
            -> [(DataType, conf)]
            -> m a
runGenericT (G m) overrides = evalStateT m overrideMap
  where
    overrideMap = Map.fromList $
                  map (\(a,b) -> (fullyQualify a, (a,b))) overrides


------------------------------------------------------------------------------
glift :: StateT (GenericMap c m) m a -> GenericT c m a
glift = G


------------------------------------------------------------------------------
fullyQualify :: DataType -> ByteString
fullyQualify t = S.pack $ dataTypeName t


------------------------------------------------------------------------------
override :: Monad m => DataType -> conf -> GenericT conf m ()
override dt conf = G $ modify $ Map.insert dname (dt,conf)
  where
    dname = fullyQualify dt


------------------------------------------------------------------------------
makeData :: (Data a, Monad m, MonadPlus m) =>
            (forall d . Data d => conf -> GenericT conf m d)
            -- ^ code to try for overridden datatypes
         -> (forall d . Data d => m d)
            -- ^ code to try for non-overridden datatypes
         -> GenericT conf m a
makeData overridden nonOverridden = go
  where
    go   = overridden' `mplus` (glift $ lift nonOverridden)

    overridden' = do
        guard $ isAlgType dt
        glift (liftM (Map.lookup dname) get) >>=
           maybe mzero (\(dt',config) -> do
               when (dataTypeRep dt /= dataTypeRep dt') $
                    fail "unexpected datatype mismatch"
               overridden config)

      where
        dt    = dataTypeOf $ resType go
        dname = fullyQualify dt


------------------------------------------------------------------------------
runData :: (Data a, Monad m, MonadPlus m) =>
           a
        -> (forall d . Data d => conf -> d -> GenericT conf m b)
        -> (forall d . Data d => d -> m b)
        -> GenericT conf m b
runData d overridden nonOverridden = go
  where
    go = overridden' `mplus` (glift $ lift $ nonOverridden d)

    overridden' = do
        guard $ isAlgType dt
        glift (liftM (Map.lookup dname) get) >>=
           maybe mzero (\(dt',config) -> do
               when (dataTypeRep dt /= dataTypeRep dt') $
                    fail "unexpected datatype mismatch"
               overridden config d)

      where
        dt    = dataTypeOf d
        dname = fullyQualify dt


resType :: Monad m => m a -> a
resType _ = error "resType"
