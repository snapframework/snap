{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Network.HTTP.Enumerator as HTTP
import           Test.Framework (defaultMain, Test)
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.TestCommon

main :: IO ()
main = defaultMain tests
  where tests = [ testBarebones
                , testDefault
                ]


testBarebones :: Test
testBarebones = testCase "snap/barebones" go
  where
    go = testGeneratedProject "barebonesTest"
                              "-b"
                              ""
                              port
                              testIt
    port = 9990
    testIt = do
        body <- HTTP.simpleHttp "http://127.0.0.1:9990"
        assertEqual "server not up" "hello world" body


testDefault :: Test
testDefault = testCase "snap/default" go
  where
    go = testGeneratedProject "defaultTest"
                              ""
                              ""
                              port
                              testIt
    port = 9991
    testIt = do
        body <- liftM (S.concat . L.toChunks) $
                HTTP.simpleHttp "http://127.0.0.1:9991"
        assertBool "response contains phrase 'it works!'"
                   $ "It works!" `S.isInfixOf` body


-- TODO: test hint code here
