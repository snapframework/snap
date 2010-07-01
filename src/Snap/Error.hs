{-# LANGUAGE OverloadedStrings #-}
-- | This module contains a couple simple functions for helping Snap
-- applications deal with errors.
module Snap.Error where

------------------------------------------------------------------------------
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO

import           Prelude hiding (catch)

import qualified Data.ByteString.Char8 as S

------------------------------------------------------------------------------
import           Snap.Iteratee
import           Snap.Types


------------------------------------------------------------------------------
-- | This function creates a simple plain text error page with the
-- provided content.  It sets the response status to 500, and
-- short-circuits further handling of the request
internalError :: (MonadSnap m) => S.ByteString -> m a
internalError msg =
    let rsp = setContentType "text/plain; charset=utf-8"
            . setContentLength (fromIntegral $ S.length msg)
            . setResponseStatus 500 "Internal Server Error"
            . modifyResponseBody (>. enumBS msg)
            $ emptyResponse

    in finishWith rsp


------------------------------------------------------------------------------
-- | This function wraps a 'MonadSnap' handler so that if any
-- exceptions escape from it, the exception is rendered as an error
-- page and not propogated any further.
catch500 :: (MonadSnap m) => m a -> m a
catch500 action = action `catch` handler
  where
    handler :: (MonadSnap m) => SomeException -> m a
    handler = internalError
            . S.append "Unhandled error:\r\n\r\n"
            . S.pack
            . show
