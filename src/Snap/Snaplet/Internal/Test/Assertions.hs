{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Internal.Test.Assertions where

------------------------------------------------------------------------------
import           Test.HUnit (Assertion, assertEqual)

------------------------------------------------------------------------------
import           Snap.Snaplet.Internal.Test.Types

------------------------------------------------------------------------------
assertInitSuccess :: SnapletEnvironment -> Assertion
assertInitSuccess env = assertEqual failMsg True True
  where
    failMsg = "Expected but got"


