{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|

  Some pre-packaged splices that add convenience to a Heist-enabled
  application.

-}

module Snap.Snaplet.Auth.SpliceHelpers
  ( addAuthSplices
  , compiledAuthSplices
  , userCSplices
  , userISplices
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
  => Snaplet (Heist b)
  -> SnapletLens b (AuthManager b)
      -- ^ A lens reference to 'AuthManager'
  -> Initializer b v ()
addAuthSplices h auth = addConfig h $ mempty
    { hcInterpretedSplices = [ ("ifLoggedIn", ifLoggedIn auth)
                             , ("ifLoggedOut", ifLoggedOut auth)
                             , ("loggedInUser", loggedInUser auth)
                             ]
    , hcCompiledSplices = compiledAuthSplices auth
    }


------------------------------------------------------------------------------
-- | List containing compiled splices for ifLoggedIn, ifLoggedOut, and
-- loggedInUser.
compiledAuthSplices :: SnapletLens b (AuthManager b)
                    -> [(Text, SnapletCSplice b)]
compiledAuthSplices auth =
    [ ("ifLoggedIn", cIfLoggedIn auth)
    , ("ifLoggedOut", cIfLoggedOut auth)
    , ("loggedInUser", cLoggedInUser auth)
    ]


------------------------------------------------------------------------------
-- | Function to generate interpreted splices from an AuthUser.
userISplices :: Monad m => AuthUser -> [(Text, I.Splice m)]
userISplices AuthUser{..} =
    [ ("userId", I.textSplice $ maybe "-" unUid userId)
    , ("userLogin", I.textSplice userLogin)
    , ("userEmail", I.textSplice $ fromMaybe "-" userEmail)
    , ("userActive", I.textSplice $ T.pack $ show $ isNothing userSuspendedAt)
    , ("userLoginCount", I.textSplice $ T.pack $ show userLoginCount)
    , ("userFailedCount", I.textSplice $ T.pack $ show userFailedLoginCount)
    , ("userLoginAt", I.textSplice $ maybe "-" (T.pack . show) userCurrentLoginAt)
    , ("userLastLoginAt", I.textSplice $ maybe "-" (T.pack . show) userLastLoginAt)
    , ("userSuspendedAt", I.textSplice $ maybe "-" (T.pack . show) userSuspendedAt)
    , ("userLoginIP", I.textSplice $ maybe "-" decodeUtf8 userCurrentLoginIp)
    , ("userLastLoginIP", I.textSplice $ maybe "-" decodeUtf8 userLastLoginIp)
    , ("userIfActive", ifISplice (isNothing userSuspendedAt))
    , ("userIfSuspended", ifISplice (isJust userSuspendedAt))
    ]


------------------------------------------------------------------------------
-- | Compiled splices for AuthUser.
userCSplices :: Monad m => [(Text, C.Promise AuthUser -> C.Splice m)]
userCSplices = (C.pureSplices $ C.textSplices
    [ ("userId", maybe "-" unUid . userId)
    , ("userLogin", userLogin)
    , ("userEmail", fromMaybe "-" . userEmail)
    , ("userActive", T.pack . show . isNothing . userSuspendedAt)
    , ("userLoginCount", T.pack . show . userLoginCount)
    , ("userFailedCount", T.pack . show . userFailedLoginCount)
    , ("userLoginAt", maybe "-" (T.pack . show) . userCurrentLoginAt)
    , ("userLastLoginAt", maybe "-" (T.pack . show) . userLastLoginAt)
    , ("userSuspendedAt", maybe "-" (T.pack . show) . userSuspendedAt)
    , ("userLoginIP", maybe "-" decodeUtf8 . userCurrentLoginIp)
    , ("userLastLoginIP", maybe "-" decodeUtf8 . userLastLoginIp)
    ]) ++
    [ ("userIfActive", ifCSplice (isNothing . userSuspendedAt))
    , ("userIfSuspended", ifCSplice (isJust . userSuspendedAt))
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


