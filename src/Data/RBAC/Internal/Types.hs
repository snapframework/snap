{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.RBAC.Internal.Types 
  ( module Data.RBAC.Internal.Role
  , module Data.RBAC.Internal.Rule
  , RoleMonad(..)
  , RuleChecker(..)
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Logic

import           Data.RBAC.Internal.Role
import           Data.RBAC.Internal.Rule


------------------------------------------------------------------------------
-- TODO: should the monads be transformers here? If they were, you could check
-- more complex predicates here


------------------------------------------------------------------------------
newtype RoleMonad a = RoleMonad { _unRC :: Logic a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadLogic)


------------------------------------------------------------------------------
newtype RuleChecker a = RuleChecker (ReaderT (RoleMonad Role) RoleMonad a)
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadLogic)

