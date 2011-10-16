{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Internal.Tests
  ( tests, initTest ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Lens.Template
import           Data.List
import           Data.Text
import           Prelude hiding (catch, (.))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import Snap.Snaplet.Internal.Initializer
import Snap.Snaplet.Internal.Types


data Foo = Foo Int

data Bar = Bar Int


data App = App
    { _foo :: Snaplet Foo
    , _bar :: Snaplet Bar
    }

makeLens ''App

--showConfig :: SnapletConfig -> IO ()
--showConfig c = do
--    putStrLn "SnapletConfig:"
--    print $ _scAncestry c
--    print $ _scFilePath c
--    print $ _scId c
--    print $ _scDescription c
--    print $ _scRouteContext c
--    putStrLn ""

assertGet :: (MonadIO m, Eq a) => String -> m a -> a -> m ()
assertGet name getter val = do
    v <- getter
--  When I add these three lines I get a strange error from GHC:
--      FATAL:Symbol _dmAh_info_dsp already defined.
--    when (v /= val) $ do
--        liftIO $ putStrLn $ "{--- "++(show v)++" ---}"
--        liftIO $ putStrLn $ "{--- "++(show val)++" ---}"
    liftIO $ assertBool name $ v == val

configAssertions :: (MonadSnaplet m, MonadIO (m b v))
                 => [Char]
                 -> ([Text], FilePath, Maybe Text, Text, ByteString)
                 -> m b v ()
configAssertions prefix (a,f,n,d,r) = do
    assertGet (prefix++"ancestry") getSnapletAncestry a
    assertGet (prefix++"file path") getSnapletFilePath f
    assertGet (prefix++"name") getSnapletName n
    assertGet (prefix++"description") getSnapletDescription d
    assertGet (prefix++"route context") getSnapletRootURL r

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do
    configAssertions "root "
        ([], "", Just "app", "Test application", "")
    f <- nestSnaplet "foo" foo $ fooInit
    b <- nestSnaplet "bar" bar $ barInit
    return $ App f b

fooInit :: SnapletInit b Foo
fooInit = makeSnaplet "foo" "Foo Snaplet" Nothing $ do
    configAssertions "foo "
        (["app"], "snaplets/foo", Just "foo", "Foo Snaplet", "foo")
    return $ Foo 42

barInit :: SnapletInit b Bar
barInit = makeSnaplet "bar" "Bar Snaplet" Nothing $ do
    configAssertions "bar "
        (["app"], "snaplets/bar", Just "bar", "Bar Snaplet", "bar")
    return $ Bar 2

initTest :: IO ()
initTest = do
    (out,_,_) <- runSnaplet appInit
    if out == "aoeu" then putStrLn "Something really strange" else return ()

tests :: Test
tests = testGroup "Snap.Snaplet.Internal"
    [ testCase "initializer tests" initTest
    ]

