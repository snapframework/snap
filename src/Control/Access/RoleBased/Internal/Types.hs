{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Control.Access.RoleBased.Internal.Types
  ( module Control.Access.RoleBased.Internal.Role
  , module Control.Access.RoleBased.Internal.Rule
  , RoleMonad(..)
  , RuleChecker(..)
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Logic
------------------------------------------------------------------------------
import           Control.Access.RoleBased.Internal.Role
import           Control.Access.RoleBased.Internal.Rule


------------------------------------------------------------------------------
-- TODO: should the monads be transformers here? If they were, you could check
-- more complex predicates here


------------------------------------------------------------------------------
newtype RoleMonad a = RoleMonad { _unRC :: Logic a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadLogic)


------------------------------------------------------------------------------
newtype RuleChecker a = RuleChecker (ReaderT (RoleMonad Role) RoleMonad a)
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadLogic)

