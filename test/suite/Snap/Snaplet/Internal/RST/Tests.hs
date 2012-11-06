{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Internal.RST.Tests
  ( tests ) where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Prelude hiding (catch, (.))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)

import           Snap.Snaplet.Internal.RST


tests :: Test
tests = testGroup "Snap.Snaplet.Internal.RST"
    [ testExec
    , testEval
    , testFail
    , testAlternative
    ]


testEval :: Test
testEval = testProperty "RST/execRST" prop
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

testFail :: Test
testFail = testCase "RST/fail" $
    assertEqual "RST fail" rstFail Nothing

testAlternative :: Test
testAlternative = testCase "RST/Alternative" $ do
    assertEqual "Alternative instance" rstAlt (Just (5, 1))
    assertEqual "Alternative instance" rstAlt2 (Just (5, 1))

addEnv :: Monad m => RST Int Int m ()
addEnv = do
    v <- ask
    modify (+v)

rstAlt :: Maybe (Int, Int)
rstAlt = runRST (addEnv >> (empty <|> (return 5))) 1 0

rstAlt2 :: Maybe (Int, Int)
rstAlt2 = runRST (addEnv >> ((return 5) <|> empty)) 1 0

rstFail :: Maybe Int
rstFail = evalRST (fail "foo") (0 :: Int) (0 :: Int)

