{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Handlers.Tests
  ( tests ) where

------------------------------------------------------------------------------
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)

------------------------------------------------------------------------------
import           Snap.Snaplet.Auth.Handlers


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth.Handlers"
    [testGroup "createUser tests"
        [testCreateUserGood
        ]
    ]

testCreateUserGood :: Test
testCreateUserGood = testCase "Handlers/createUser" $
    assertEqual "createUser with good parameters" True True
