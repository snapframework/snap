{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Snap.Snaplet.Auth.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Test.Framework                   (Test, testGroup)
import qualified Snap.Snaplet.Auth.Handlers.Tests
import qualified Snap.Snaplet.Auth.Types.Tests
import qualified Snap.Snaplet.Auth.SpliceTests


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth"
    [ Snap.Snaplet.Auth.Handlers.Tests.tests
    , Snap.Snaplet.Auth.SpliceTests.tests
    , Snap.Snaplet.Auth.Types.Tests.tests
    ]

