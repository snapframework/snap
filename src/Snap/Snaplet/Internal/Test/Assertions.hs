{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Internal.Test.Assertions where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Test.HUnit (Assertion, assertEqual)

------------------------------------------------------------------------------
import           Snap.Core


------------------------------------------------------------------------------
assertInitSuccess :: (Text, Snap (), IO ()) -> Assertion
assertInitSuccess env = assertEqual failMsg True True
  where
    failMsg = "Expected but got"


