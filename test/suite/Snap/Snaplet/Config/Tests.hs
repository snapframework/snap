module Snap.Snaplet.Config.Tests where

------------------------------------------------------------------------------
import Control.Monad
import Data.Function
import Data.Monoid
------------------------------------------------------------------------------
import Snap.Snaplet.Config
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit hiding (Test)


------------------------------------------------------------------------------
configTests :: Test
configTests = testGroup "Snaplet Config"
        [ testProperty "Monoid left identity" monoidLeftIdentity
        , testProperty "Monoid right identity" monoidRightIdentity
        , testProperty "Monoid associativity" monoidAssociativity 
        ]

newtype ArbAppConfig = ArbAppConfig { unArbAppConfig :: AppConfig }

instance Show ArbAppConfig where
  show (ArbAppConfig (AppConfig a)) =
    "ArbAppConfig (AppConfig " ++ show a ++ ")"

instance Eq ArbAppConfig where
  a == b = ((==) `on` (appEnvironment . unArbAppConfig)) a b

instance Arbitrary ArbAppConfig where
  arbitrary = liftM (ArbAppConfig . AppConfig) arbitrary

instance Monoid ArbAppConfig where
  mempty        = ArbAppConfig mempty
  a `mappend` b = ArbAppConfig $ ((<>) `on` unArbAppConfig) a b

monoidLeftIdentity :: ArbAppConfig -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: ArbAppConfig -> Bool
monoidRightIdentity a = a <> mempty == a

monoidAssociativity :: ArbAppConfig -> ArbAppConfig -> ArbAppConfig
                    -> Bool
monoidAssociativity a b c = (a <> b) <> c == a <> (b <> c)
