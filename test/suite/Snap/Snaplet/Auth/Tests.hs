{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Snap.Snaplet.Auth.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Test.Framework
import qualified Snap.Snaplet.Auth.Handlers.Tests


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth"
    [ Snap.Snaplet.Auth.Handlers.Tests.tests
    ]

