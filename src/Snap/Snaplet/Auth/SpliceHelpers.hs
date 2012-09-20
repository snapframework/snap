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
  , ifLoggedIn
  , ifLoggedOut
  , loggedInUser
  , cIfLoggedIn
  , cIfLoggedOut
  , cLoggedInUser
  ) where

import           Control.Monad.Trans
import           Data.Lens.Lazy
import           Data.Monoid
import           Data.Text (Text)
import qualified Text.XmlHtml as X
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C

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
  => Lens b (Snaplet (AuthManager b))
      -- ^ A lens reference to 'AuthManager'
  -> Initializer b v ()
addAuthSplices auth = addSplices
    [ ("ifLoggedIn", ifLoggedIn auth)
    , ("ifLoggedOut", ifLoggedOut auth)
    , ("loggedInUser", loggedInUser auth)
    ]


compiledAuthSplices :: Lens b (Snaplet (AuthManager b))
                    -> [(Text, SnapletCSplice b v)]
compiledAuthSplices auth =
    [ ("ifLoggedIn", cIfLoggedIn auth)
    , ("ifLoggedOut", cIfLoggedOut auth)
    , ("loggedInUser", cLoggedInUser auth)
    ]


------------------------------------------------------------------------------
-- | A splice that can be used to check for existence of a user. If a user is
-- present, this will run the contents of the node.
--
-- > <ifLoggedIn> Show this when there is a logged in user </ifLoggedIn>
ifLoggedIn :: Lens b (Snaplet (AuthManager b)) -> SnapletISplice b v
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
cIfLoggedIn :: Lens b (Snaplet (AuthManager b)) -> SnapletCSplice b v
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
ifLoggedOut :: Lens b (Snaplet (AuthManager b)) -> SnapletISplice b v
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
cIfLoggedOut :: Lens b (Snaplet (AuthManager b)) -> SnapletCSplice b v
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
loggedInUser :: Lens b (Snaplet (AuthManager b)) -> SnapletISplice b v
loggedInUser auth = do
    u <- lift $ withTop auth currentUser
    maybe (return []) (I.textSplice . userLogin) u 


-------------------------------------------------------------------------------
-- | A splice that will simply print the current user's login, if
-- there is one.
cLoggedInUser :: Lens b (Snaplet (AuthManager b)) -> SnapletCSplice b v
cLoggedInUser auth =
    return $ C.yieldRuntimeText $ do
        u <- lift $ withTop auth currentUser
        return $ maybe "" userLogin u 


