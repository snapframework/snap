{-# LANGUAGE OverloadedStrings #-}
{-|

  Convenience Splices to be used in your views. They go hand-in hand with
  handlers defined in this package to help automate some common patterns.

-}

module Snap.Snaplet.Session.Helpers
  ( metaCSRFTag
  , hiddenCSRFTag
  , checkCSRF
  ) where


import Control.Applicative ( (<|>) )
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Text.Encoding as T


import Snap.Core
import Snap.Snaplet.Session (MonadSession(..), sessionCSRFToken)

import qualified Text.XmlHtml as X
import           Text.Templating.Heist


------------------------------------------------------------------------------
-- Use this 'Splice' in your <head> section to insert a meta tag with the
-- authenticity token.
--
-- Use-case similar to Rails 3; you can use unobtrusive JS bindings to extract
-- the token from the webpage and add to your buttons/forms.
metaCSRFTag
  :: (MonadSession m)
  => Splice m
metaCSRFTag = do
  embeddedToken <- lift sessionCSRFToken
  let param = "authenticity_token"
  let metaToken = X.Element "meta"
                    [ ("name", "csrf-token")
                    , ("content", T.decodeUtf8 embeddedToken) ] []
  let metaParam = X.Element "meta"
                    [ ("name", "csrf-param")
                    , ("content", param) ] []
  return $ [metaParam, metaToken]


------------------------------------------------------------------------------
-- Use in your forms to insert a hidden "authenticity_token" field.
hiddenCSRFTag
  :: (MonadSession m)
  => Splice m
hiddenCSRFTag = do
  embeddedToken <- lift sessionCSRFToken
  let param = "authenticity_token"
  return . return $ X.Element "input"
    [ ("type", "hidden")
    , ("name", T.decodeUtf8 param)
    , ("value", T.decodeUtf8 embeddedToken)
    ] []



------------------------------------------------------------------------------
-- | Handler to protect against CSRF attacks. Chain this handler at the
-- beginning of your routing table to enable.
--
-- Example:
--
-- @redirError = logError "Someone tried to bypass CSRF" >> redirect "/"
--
-- checkCSRF redirError >> route [myHandler, myHandler2, ...]
-- @
--
-- The convention is to submit an "authenticity_token" parameter with each
-- 'POST' request. This action will confirm its presence against what is
-- safely embedded in the session and execute the given action if they don't
-- match. The exact name of the parameter is defined by
-- 'authAuthenticityTokenParam'.
checkCSRF :: MonadSession m => m ()
              -- ^ Do this if CSRF token does not match.
          -> m ()
checkCSRF failAct = method POST doCheck <|> return ()
  where
    doCheck = do
      embeddedToken <- sessionCSRFToken
      let param = "authenticity_token"
      submitted <- maybe "" id `fmap` getParam param
      when (submitted /= embeddedToken) failAct
