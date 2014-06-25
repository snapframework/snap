module Snap.TestCommon where

------------------------------------------------------------------------------
import Control.Exception (try, SomeException)
import GHC.Read
import Test.HUnit        (Assertion, assertFailure, assertBool)
import Text.ParserCombinators.ReadPrec

------------------------------------------------------------------------------
expectException :: String -> IO a -> IO ()
expectException s m = do
  r <- try m
  case r of
    Left (e::SomeException) -> length (show e) `seq` return ()
    Right _ -> assertFailure s


------------------------------------------------------------------------------
showTestCase :: Show a => a -> Assertion
showTestCase a = assertBool "Show instance failed" $
                 ((showsPrec 5 a) "" == show a)
                 && (showList [a]) "" == "[" ++ show a ++ "]"
                 

------------------------------------------------------------------------------
readTestCase :: (Eq a, Show a, Read a) => a -> Assertion
readTestCase a = assertBool "Read instance failed" $
                 ( ((readsPrec 1) (show a)) == ([(a,"")]))
                 && ((readList ("[" ++ show a ++ "]")) == [([a],"")])
                 && ((readPrec_to_S (readPrec) 5) (show a) == [(a,"")])
                 && ((readPrec_to_S (readListPrec) 5) ("[" ++ show a ++ "]")
                     == [([a],"")])

                 
------------------------------------------------------------------------------
ordTestCase :: (Eq a, Ord a) => a -> a -> Assertion
ordTestCase a b = assertBool "Ord instance failed" $
                  low <= high
                  && (if   low /= high
                      then low < high  && compare low high == LT && high > low
                      else low == high && compare low high == EQ)
  where
    low  = min a b
    high = max a b


------------------------------------------------------------------------------
eqTestCase :: (Eq a) => a -> a -> Assertion
eqTestCase a b = assertBool "Eq instance failed" $
                 if a == b
                 then (a /= b) == False
                 else (a /= b) == True
