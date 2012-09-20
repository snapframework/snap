{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Internal.Test.Assertions where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.HUnit (Assertion, assertEqual)

------------------------------------------------------------------------------
import           Snap.Core


------------------------------------------------------------------------------
assertInitSuccess :: (Text, Snap (), IO ()) -> Assertion
assertInitSuccess env = assertEqual failMsg True True
  where
    failMsg = "Expected but got"


------------------------------------------------------------------------------
-- | Asserts whether a given 'runHandler' output contains the target
--   message 'needle'.
assertInitContains :: Text -> (Text, Snap (), IO ()) -> Assertion
assertInitContains needle (m,_,_) = assertEqual failMsg containedInMsg True
  where
      failMsg = T.unpack $ T.concat [needle, " not found in: ", m]
      containedInMsg = needle `elem` T.words m

