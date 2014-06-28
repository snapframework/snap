module Snap.Snaplet.Auth.SpliceTests (
  tests
  ) where


------------------------------------------------------------------------------
import           Control.Monad                  (replicateM_, when)
import qualified Data.Map                       as Map
import qualified Data.ByteString                as BS
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------
import           Snap.Core                      as Core
import           Snap.Snaplet                   (with)
import qualified Snap.Test                      as ST
import           Snap.Snaplet.Test              (runHandler,
                                                 withTemporaryFile)
import           Snap.Snaplet.Auth              (Password(ClearText),
                                                 createUser, loginByUsername,
                                                 userISplices)
import           Snap.Snaplet.Heist             (cRender, render,
                                                 withSplices)
import           Snap.Snaplet.Auth.App          (appInit, auth)


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth.SpliceHelpers"
        [testCase "Render new user page"    $ renderNewUser False False
        ,testCase "New user login render"   $ renderNewUser True  False
        ,testCase "New user suspend render" $ renderNewUser False True
        ,testCase "cRender new user page"   $ cRenderNewUser
        ]


------------------------------------------------------------------------------
renderNewUser :: Bool -> Bool -> Assertion
renderNewUser login suspend = withTemporaryFile "users.json" $ do
  let hdl = with auth $ do
        usr <- createUser "foo" "foo"
        _ <- when login $
             loginByUsername "foo" (ClearText "foo") False >> return ()
        _ <- when suspend $ replicateM_ 4 $
             loginByUsername "foo" (ClearText "wrong") False
        either
          (\_ -> Core.modifyResponse $ Core.setResponseStatus 500 "Error")
          (\u -> withSplices (userISplices u) $ render "userpage")
          usr
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
cRenderNewUser :: Assertion
cRenderNewUser = withTemporaryFile "users.json" $ do
  let hdl = with auth $ do
        _ <- createUser "foo" "foo"
        _ <- loginByUsername "foo" (ClearText "foo") True
        cRender "userpage"

      assertValidRes r = do
        rStr <- ST.responseToString r
        assertBool "userpage should contain UserName foo splice" $
          "UserLogin foo" `BS.isInfixOf` rStr

  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) assertValidRes res
