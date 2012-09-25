module Blackbox.Snaplet.Tests
  (
    tests
  ) where


------------------------------------------------------------------------------
import           Blackbox.MusicStoreSnaplet

------------------------------------------------------------------------------
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Snap.Snaplet
import           Snap.Snaplet.Test



------------------------------------------------------------------------------
tests = testGroup "Snaplet.Test-related-tests"
    [ testInitContains
    ]

testInitContains :: Test
testInitContains = testCase "testInitContains" $ do
    initOutput <- runSnaplet Nothing musicStoreInit
    assertInitContains "Initializing musicstore" initOutput
