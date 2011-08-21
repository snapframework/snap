{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Internal.Tests
  ( tests, initTest ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import           Data.Lens.Template
import           Data.List
import           Data.Word
import           Prelude hiding (catch, (.))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Snap.Snaplet.Internal.Initializer
import Snap.Snaplet.Internal.Types


data Foo = Foo
    { fooVal :: Int
    }

data Bar = Bar
    { barVal :: Int
    }


data App = App
    { _foo :: Snaplet Foo
    , _bar :: Snaplet Bar
    }

makeLens ''App

showConfig c = do
    putStrLn "SnapletConfig:"
    print $ _scAncestry c
    print $ _scFilePath c
    print $ _scId c
    print $ _scDescription c
    print $ _scRouteContext c
    putStrLn ""

assertGet name getter val = do
    v <- getter
    liftIO $ assertBool name $ v == val

configAssertions pre (a,f,n,d,r) = do
    assertGet (pre++"ancestry") getSnapletAncestry a
    assertGet (pre++"file path") getSnapletFilePath f
    assertGet (pre++"name") getSnapletName n
    assertGet (pre++"description") getSnapletDescription d
    assertGet (pre++"route context") getSnapletRootURL r

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do
    configAssertions "root "
        ([], "", Just "app", "Test application", "")
    f <- nestSnaplet "foo" foo $ fooInit
    b <- nestSnaplet "bar" bar $ barInit
    return $ App f b

fooInit = makeSnaplet "foo" "Foo Snaplet" Nothing $ do
    configAssertions "foo "
        (["app"], "snaplets/foo", Just "foo", "Foo Snaplet", "foo")
    return $ Foo 42

barInit = makeSnaplet "bar" "Bar Snaplet" Nothing $ do
    configAssertions "bar "
        (["app"], "snaplets/bar", Just "bar", "Bar Snaplet", "bar")
    return $ Bar 2

initTest = do
    (out,handler,cleanup) <- runSnaplet appInit
    if out == "aoeu" then putStrLn "Something really strange" else return ()

tests :: Test
tests = testGroup "Snap.Snaplet.Internal"
    [ testCase "initializer tests" initTest
    ]

