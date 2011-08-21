{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Internal.RST.Tests
  ( tests ) where

import           Control.Applicative
import           Control.Category
import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import           Data.Lens.Template
import           Data.List
import           Data.Word
import           Prelude hiding (catch, (.))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Snap.Snaplet.Internal.RST


tests :: Test
tests = testGroup "Snap.Snaplet.Internal.RST"
    [ testEval
    , testExec
    ]


testEval :: Test
testEval = testProperty "RST/evalRST" prop
  where
    prop x = runIdentity (evalRST m x undefined) == x
    m :: RST Int () Identity Int
    m = ask


testExec :: Test
testExec = testProperty "RST/execRST" prop
  where
    prop x = runIdentity (execRST m undefined x) == x
    m :: RST () Int Identity Int
    m = get

