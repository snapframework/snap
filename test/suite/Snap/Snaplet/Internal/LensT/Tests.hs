{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Internal.LensT.Tests (tests) where

import           Control.Lens
import           Control.Applicative
import           Control.Category
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Prelude hiding (catch, (.))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


------------------------------------------------------------------------------
import           Snap.Snaplet.Internal.LensT


------------------------------------------------------------------------------
data TestType = TestType {
      _int0 :: Int
    , _sub  :: TestSubType
} deriving (Show)

data TestSubType = TestSubType {
      _sub0 :: Int
    , _sub1 :: Int
    , _bot  :: TestBotType
} deriving (Show)

data TestBotType = TestBotType {
      _bot0 :: Int
} deriving (Show)

makeLenses ''TestType
makeLenses ''TestSubType
makeLenses ''TestBotType


------------------------------------------------------------------------------
defaultState :: TestType
defaultState = TestType 1 $ TestSubType 2 999 $ TestBotType 3


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Internal.LensT"
                  [ testfmap
                  , testApplicative
                  , testMonadState
                  ]


------------------------------------------------------------------------------
testfmap :: Test
testfmap = testCase "lensed/fmap" $ do
--    x <- evalStateT (lensedAsState (fmap (*2) three) (bot . sub)) defaultState
    let x = fst $ runIdentity (runLensT (fmap (*2) three) (sub . bot) defaultState)
    assertEqual "fmap" 6 x

    let (y,s') = runIdentity (runLensT twiddle (sub . bot) defaultState)

    assertEqual "fmap2" (12 :: Int) y
    assertEqual "lens" (13 :: Int) $ _bot0 $ _bot $ _sub s'
    return ()

  where
--    three :: LensT TestType TestBotType IO Int
    three = return 3

    twiddle = do
        modify $ \(TestBotType x) -> TestBotType (x+10)
        fmap (+9) three


------------------------------------------------------------------------------
testApplicative :: Test
testApplicative = testCase "lensed/applicative" $ do
--    x <- evalStateT (lensedAsState (pure (*2) <*> three) (bot . sub)) defaultState
    let x = fst $ runIdentity (runLensT (pure (*2) <*> three) (sub . bot) defaultState)
    assertEqual "fmap" 6 x

    let (y,s') = runIdentity (runLensT twiddle (sub . bot) defaultState)

    assertEqual "fmap2" (12::Int) y
    assertEqual "lens" 13 $ _bot0 $ _bot $ _sub s'
    return ()

  where
--    three :: LensT TestType TestBotType IO Int
    three = pure 3

    twiddle = do
        modify $ \(TestBotType x) -> TestBotType (x+10)
        pure [] *> (pure (+9) <*> three) <* pure []


------------------------------------------------------------------------------
testMonadState :: Test
testMonadState = testCase "lens/MonadState" $ do
--    s <- execStateT (lensedAsState go (bot0 . bot . sub)) defaultState
    let s = snd $ runIdentity (runLensT go (sub . bot . bot0) defaultState)

    assertEqual "bot0" 9 $ _bot0 $ _bot $ _sub s
    assertEqual "sub0" 3 $ _sub0 $ _sub s
    assertEqual "sub1" 999 $ _sub1 $ _sub s

  where
--    go :: LensT TestType Int IO ()
    go = do
        modify (*2)
        modify (+3)
        withTop sub go'

--    go' :: LensT TestType TestSubType IO ()
    go' = do
        a <- with sub0 get
        with sub0 $ put $ a+1


