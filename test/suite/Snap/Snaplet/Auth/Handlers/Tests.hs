{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Handlers.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Prelude hiding (id)
import           Control.Category
import           Control.Error (isLeft, isRight)
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)


------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.App
import qualified Snap.Test as ST
import           Snap.Snaplet.Test


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth.Handlers"
    [mutuallyExclusive $ testGroup "createUser tests"
        [testCreateUserGood
        ,testCreateEmptyUser
        ]
    ]


------------------------------------------------------------------------------
createGoodUserHdlr :: Handler App App (Either AuthFailure AuthUser)
createGoodUserHdlr = with auth $ createUser "foo" "foo"


------------------------------------------------------------------------------
createEmptyUserHdlr :: Handler App App (Either AuthFailure AuthUser)
createEmptyUserHdlr = with auth $ createUser "" "foo"


------------------------------------------------------------------------------
testCreateUserGood :: Test
testCreateUserGood = testCase "createUser good params" assertGoodUser
  where 
    assertGoodUser :: Assertion
    assertGoodUser = withTemporaryFile "users.json" $ do
        res <- evalHandler (ST.get "" Map.empty) createGoodUserHdlr appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool "user successfully created" $ isRight res'


------------------------------------------------------------------------------
testCreateEmptyUser :: Test
testCreateEmptyUser = testCase "createUser empty username" assertEmptyUser
  where 
    assertEmptyUser :: Assertion
    assertEmptyUser = do
        res <- evalHandler (ST.get "" Map.empty) createEmptyUserHdlr appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool "empty username rejected" $ isLeft res'
