{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

  Some pre-packaged splices that add convenience to a Heist-enabled
  application.

-}

module Snap.Snaplet.Auth.SpliceHelpers
  (
    addAuthSplices
  , compiledAuthSplices
  , userCSplices
  , ifLoggedIn
  , ifLoggedOut
  , loggedInUser
  , cIfLoggedIn
  , cIfLoggedOut
  , cLoggedInUser
  ) where

import           Control.Monad.Trans
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Text.XmlHtml as X
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
import           Heist.Splices

import           Snap.Snaplet
import           Snap.Snaplet.Auth.AuthManager
import           Snap.Snaplet.Auth.Handlers
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet.Heist


------------------------------------------------------------------------------
-- | Add all standard auth splices to a Heist-enabled application.
--
-- This adds the following splices:
-- \<ifLoggedIn\>
-- \<ifLoggedOut\>
-- \<loggedInUser\>
addAuthSplices
  :: HasHeist b
  => SnapletLens b (AuthManager b)
      -- ^ A lens reference to 'AuthManager'
  -> Initializer b v ()
addAuthSplices auth = addSplices
    [ ("ifLoggedIn", ifLoggedIn auth)
    , ("ifLoggedOut", ifLoggedOut auth)
    , ("loggedInUser", loggedInUser auth)
    ]


compiledAuthSplices :: SnapletLens b (AuthManager b)
                    -> [(Text, SnapletCSplice b)]
compiledAuthSplices auth =
    [ ("ifLoggedIn", cIfLoggedIn auth)
    , ("ifLoggedOut", cIfLoggedOut auth)
    , ("loggedInUser", cLoggedInUser auth)
    ]


userCSplices :: Monad m => [(Text, C.Promise AuthUser -> C.Splice m)]
userCSplices = (C.pureSplices $ C.textSplices
    [ ("login", userLogin)
    , ("email", maybe "-" id . userEmail)
    , ("active", T.pack . show . isNothing . userSuspendedAt)
    , ("loginCount", T.pack . show . userLoginCount)
    , ("failedCount", T.pack . show . userFailedLoginCount)
    , ("loginAt", maybe "-" (T.pack . show) . userCurrentLoginAt)
    , ("lastLoginAt", maybe "-" (T.pack . show) . userLastLoginAt)
    , ("suspendedAt", maybe "-" (T.pack . show) . userSuspendedAt)
    , ("loginIP", maybe "-" decodeUtf8 . userCurrentLoginIp)
    , ("lastLoginIP", maybe "-" decodeUtf8 . userLastLoginIp)
    ]) ++
    [ ("ifActive", ifCSplice (isNothing . userSuspendedAt))
    , ("ifSuspended", ifCSplice (isJust . userSuspendedAt))
    ]


------------------------------------------------------------------------------
-- | A splice that can be used to check for existence of a user. If a user is
-- present, this will run the contents of the node.
--
-- > <ifLoggedIn> Show this when there is a logged in user </ifLoggedIn>
ifLoggedIn :: SnapletLens b (AuthManager b) -> SnapletISplice b
ifLoggedIn auth = do
    chk <- lift $ withTop auth isLoggedIn
    case chk of
      True -> getParamNode >>= return . X.childNodes
      False -> return []


------------------------------------------------------------------------------
-- | A splice that can be used to check for existence of a user. If a user is
-- present, this will run the contents of the node.
--
-- > <ifLoggedIn> Show this when there is a logged in user </ifLoggedIn>
cIfLoggedIn :: SnapletLens b (AuthManager b) -> SnapletCSplice b
cIfLoggedIn auth = do
    children <- C.promiseChildren
    return $ C.yieldRuntime $ do
        chk <- lift $ withTop auth isLoggedIn
        case chk of
          True -> children
          False -> return mempty


------------------------------------------------------------------------------
-- | A splice that can be used to check for absence of a user. If a user is
-- not present, this will run the contents of the node.
--
-- > <ifLoggedOut> Show this when there is a logged in user </ifLoggedOut>
ifLoggedOut :: SnapletLens b (AuthManager b) -> SnapletISplice b
ifLoggedOut auth = do
    chk <- lift $ withTop auth isLoggedIn
    case chk of
      False -> getParamNode >>= return . X.childNodes
      True -> return []


------------------------------------------------------------------------------
-- | A splice that can be used to check for absence of a user. If a user is
-- not present, this will run the contents of the node.
--
-- > <ifLoggedOut> Show this when there is a logged in user </ifLoggedOut>
cIfLoggedOut :: SnapletLens b (AuthManager b) -> SnapletCSplice b
cIfLoggedOut auth = do
    children <- C.promiseChildren
    return $ C.yieldRuntime $ do
        chk <- lift $ withTop auth isLoggedIn
        case chk of
          False -> children
          True -> return mempty


-------------------------------------------------------------------------------
-- | A splice that will simply print the current user's login, if
-- there is one.
loggedInUser :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInUser auth = do
    u <- lift $ withTop auth currentUser
    maybe (return []) (I.textSplice . userLogin) u 


-------------------------------------------------------------------------------
-- | A splice that will simply print the current user's login, if
-- there is one.
cLoggedInUser :: SnapletLens b (AuthManager b) -> SnapletCSplice b
cLoggedInUser auth =
    return $ C.yieldRuntimeText $ do
        u <- lift $ withTop auth currentUser
        return $ maybe "" userLogin u 


