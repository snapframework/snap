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
  , ifLoggedIn
  , ifLoggedOut
  ) where

import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Crypto.PasswordStore
import           Data.ByteString (ByteString)
import           Data.Lens.Lazy
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text)
import           Data.Time
import qualified Text.XmlHtml as X
import           Text.Templating.Heist

import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.AuthManager (AuthManager(..))
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet
import           Snap.Snaplet.Heist


--------------------------------------------------------------------------------
-- | Add all standard auth splices to a Heist-enabled application.
--
-- This adds the following splices:
-- > <ifLoggedIn>
-- > <ifLoggedOut>
addAuthSplices 
  :: HasHeist b 
  => Lens b (Snaplet (AuthManager b)) 
  -- ^ A lens reference to 'AuthManager'
  -> Initializer b v ()
addAuthSplices auth = addSplices
  [ ("ifLoggedIn", ifLoggedIn auth)
  , ("ifLoggedOut", ifLoggedOut auth)
  ]


------------------------------------------------------------------------------
-- | A splice that can be used to check for existence of a user. If a user is
-- present, this will run the contents of the node.
--
-- > <ifLoggedIn> Show this when there is a logged in user </ifLoggedIn>
ifLoggedIn 
  :: Lens b (Snaplet (AuthManager b))
  -> SnapletSplice b v
ifLoggedIn auth = do
  chk <- liftHandler $ withTop auth isLoggedIn
  case chk of
    True -> liftHeist $ getParamNode >>= return . X.childNodes 
    False -> return []


------------------------------------------------------------------------------
-- | A splice that can be used to check for absence of a user. If a user is
-- not present, this will run the contents of the node.
--
-- > <ifLoggedOut> Show this when there is a logged in user </ifLoggedOut>
ifLoggedOut 
  :: Lens b (Snaplet (AuthManager b))
  -> SnapletSplice b v
ifLoggedOut auth = do
  chk <- liftHandler $ withTop auth isLoggedIn
  case chk of
    False -> liftHeist $ getParamNode >>= return . X.childNodes 
    True -> return []

