module Snap.TestCommon where

------------------------------------------------------------------------------
import Control.Exception (try, SomeException)
import Test.HUnit        (assertFailure)


------------------------------------------------------------------------------
expectException :: String -> IO a -> IO ()
expectException s m = do
  r <- try m
  case r of
    Left (e::SomeException) -> length (show e) `seq` return ()
    Right _ -> assertFailure s