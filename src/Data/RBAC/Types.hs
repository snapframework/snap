{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.RBAC.Types
  ( Role(..)                    -- fixme: remove (..)
  , RoleValue(..)               -- fixme
  , RoleValueMeta(..)
  , RoleDataDefinition(..)
  , RoleMetadata(..)
  , Rule
  , RuleChecker
  ) where

import Data.RBAC.Internal.Types
