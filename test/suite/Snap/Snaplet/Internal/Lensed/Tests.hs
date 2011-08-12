{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Internal.Lensed.Tests (tests) where

import           Control.Applicative
import           Control.Category
import           Control.Exception
import           Control.Monad.State.Strict
import           Data.Lens.Template
import           Prelude hiding (catch, (.))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


------------------------------------------------------------------------------
import           Snap.Snaplet.Internal.Lensed


------------------------------------------------------------------------------
data TestType = TestType {
      int0_ :: Int
    , sub_  :: TestSubType
} deriving (Show)

data TestSubType = TestSubType {
      sub0_ :: Int
    , sub1_ :: Int
    , bot_  :: TestBotType
} deriving (Show)

data TestBotType = TestBotType {
      bot0_ :: Int
} deriving (Show)

$( deriveAccessors ''TestType    )
$( deriveAccessors ''TestSubType )
$( deriveAccessors ''TestBotType )


------------------------------------------------------------------------------
defaultState :: TestType
defaultState = TestType 1 $ TestSubType 2 999 $ TestBotType 3


------------------------------------------------------------------------------
tests = testGroup "Snap.Snaplet.Internal.Lensed"
                  [ testfmap
                  , testApplicative
                  , testMonadState
                  ]


------------------------------------------------------------------------------
testfmap :: Test
testfmap = testCase "lensed/fmap" $ do
    x <- evalStateT (runLensed (fmap (*2) three) (bot . sub)) defaultState
    assertEqual "fmap" 6 x

    (y,s') <- runStateT (runLensed twiddle (bot . sub)) defaultState

    assertEqual "fmap2" 12 y
    assertEqual "lens" 13 $ bot0_ $ bot_ $ sub_ s'
    return ()

  where
    three :: Lensed TestType TestBotType IO Int
    three = return 3

    twiddle = do
        modify $ \(TestBotType x) -> TestBotType (x+10)
        fmap (+9) three


------------------------------------------------------------------------------
testApplicative :: Test
testApplicative = testCase "lensed/applicative" $ do
    x <- evalStateT (runLensed (pure (*2) <*> three) (bot . sub)) defaultState
    assertEqual "fmap" 6 x

    (y,s') <- runStateT (runLensed twiddle (bot . sub)) defaultState

    assertEqual "fmap2" (12::Int) y
    assertEqual "lens" 13 $ bot0_ $ bot_ $ sub_ s'
    return ()

  where
    three :: Lensed TestType TestBotType IO Int
    three = pure 3

    twiddle = do
        modify $ \(TestBotType x) -> TestBotType (x+10)
        pure [] *> (pure (+9) <*> three) <* pure []


------------------------------------------------------------------------------
testMonadState :: Test
testMonadState = testCase "lens/MonadState" $ do
    s <- execStateT (runLensed go (bot0 . bot . sub)) defaultState

    assertEqual "bot0" 9 $ bot0_ $ bot_ $ sub_ s
    assertEqual "sub0" 3 $ sub0_ $ sub_ s
    assertEqual "sub1" 1000 $ sub1_ $ sub_ s

  where
    go :: Lensed TestType Int IO ()
    go = do
        modify (*2)
        modify (+3)
        withGlobal sub go'

    go' :: Lensed TestType TestSubType IO ()
    go' = do
        a <- with sub0 get
        with sub0 $ put $ a+1
        embed sub1 go''

    go'' :: Lensed TestSubType Int IO ()
    go'' = modify (+1)


eat :: SomeException -> IO ()
eat _ = return ()

qqq = defaultMainWithArgs [tests] ["--plain"] `catch` eat
