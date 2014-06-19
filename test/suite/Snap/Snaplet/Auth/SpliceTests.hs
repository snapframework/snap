module Snap.Snaplet.Auth.SpliceTests (
  tests
  ) where


------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad
import qualified Data.Map                       as Map
import           Data.Time.Clock
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)

import qualified Heist.Compiled                 as HC
import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Test as ST
import           Snap.Snaplet.Test
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.Heist.Interpreted      as HI
import           Snap.Snaplet.Auth.App


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
          (\_ -> modifyResponse $ setResponseStatus 500 "Login error")
          (\u     -> withSplices (userISplices u) $ render "userpage")
          usr
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
cRenderNewUser :: Assertion
cRenderNewUser = withTemporaryFile "users.json" $ do
  let hdl = with auth $ do
        usr <- createUser "foo" "foo"
        either
          (\_ -> modifyResponse $ setResponseStatus 500 "Login error")
          (\u -> HC.runChildren $ HC.withSplices "userSplice" userCSplices (return ()) )
          usr
 
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res
