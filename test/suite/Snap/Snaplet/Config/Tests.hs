module Snap.Snaplet.Config.Tests where

------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Configurator.Types as C
import Data.Function
import qualified Data.Map as Map
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
import Data.Monoid hiding ((<>))
#else
import Data.Monoid
#endif
import Data.Typeable
import System.Environment
------------------------------------------------------------------------------
import Snap.Core
import Snap.Http.Server.Config
import Snap.Snaplet
import Snap.Snaplet.Config
import Snap.Snaplet.Heist
import Snap.Snaplet.Test.Common.App
import Snap.Snaplet.Internal.Initializer
import qualified Snap.Test as ST
import Snap.Snaplet.Test
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit hiding (Test)


------------------------------------------------------------------------------
configTests :: Test
configTests = testGroup "Snaplet Config"
        [ testProperty "Monoid left identity"     monoidLeftIdentity
        , testProperty "Monoid right identity"    monoidRightIdentity
        , testProperty "Monoid associativity"     monoidAssociativity
        , testCase     "Verify Typeable instance" verTypeable
--        , testCase     "Config options used"      appConfigGetsToConfig
        ]

newtype ArbAppConfig = ArbAppConfig { unArbAppConfig :: AppConfig }

instance Show ArbAppConfig where
  show (ArbAppConfig (AppConfig a)) =
    "ArbAppConfig (AppConfig " ++ show a ++ ")"

instance Eq ArbAppConfig where
  a == b = ((==) `on` (appEnvironment . unArbAppConfig)) a b

instance Arbitrary ArbAppConfig where
  arbitrary = liftM (ArbAppConfig . AppConfig) arbitrary

instance Semigroup ArbAppConfig where
  a <> b = ArbAppConfig $ ((<>) `on` unArbAppConfig) a b

instance Monoid ArbAppConfig where
  mempty        = ArbAppConfig mempty
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

monoidLeftIdentity :: ArbAppConfig -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: ArbAppConfig -> Bool
monoidRightIdentity a = a <> mempty == a

monoidAssociativity :: ArbAppConfig -> ArbAppConfig -> ArbAppConfig
                    -> Bool
monoidAssociativity a b c = (a <> b) <> c == a <> (b <> c)


------------------------------------------------------------------------------
verTypeable :: Assertion
verTypeable =
  assertEqual "Unexpected Typeable behavior"
#if MIN_VERSION_base(4,7,0)
    "AppConfig"
#else
    "Snap.Snaplet.Config.AppConfig"
#endif
  (show . typeOf $ (undefined :: AppConfig))


------------------------------------------------------------------------------
appConfigGetsToConfig :: Assertion
appConfigGetsToConfig = do
  opts <- completeConfig =<<
          commandLineAppConfig defaultConfig  :: IO (Config Snap AppConfig)
  a    <- async . withArgs ["-p", "8001","-e","otherEnv"] $
          serveSnaplet opts appInit
  threadDelay 500000
  cancel a
  b    <- async . withArgs ["--environment","devel"] $ serveSnaplet defaultConfig appInit
  threadDelay 500000
  cancel b
  --TODO - Don't just run the server to touch the config code. Check some values
