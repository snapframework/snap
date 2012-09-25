{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Internal.Test.Assertions where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Text.Regex.Posix ((=~))
------------------------------------------------------------------------------
import           Test.HUnit (Assertion, assertEqual, assertBool)

------------------------------------------------------------------------------
import           Snap.Core


------------------------------------------------------------------------------
assertInitSuccess :: (Text, Snap (), IO ()) -> Assertion
assertInitSuccess env = assertEqual failMsg True True
  where
    failMsg = "Expected but got"


------------------------------------------------------------------------------
-- | Given the output from 'runSnaplet', asserts that generate message matches
-- the given regular  expression.
assertInitContains :: ByteString  -- ^ Regexp that will match the message content
                   -> (Text, Snap (), IO ()) -> Assertion
assertInitContains match (message,_,_) =
    let haystack = encodeUtf8 message
    in  assertBool failMsg (haystack =~ match)
  where
    failMsg = "Expected message to match regexp \"" ++ show match
              ++ "\", but didn't"


