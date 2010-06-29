{-# LANGUAGE OverloadedStrings #-}
module Snap.Error (
    catch500
  , internalError
)
where

import           Control.Exception (SomeException)
import           Control.Monad.CatchIO

import           Prelude hiding (catch)

import qualified Data.ByteString.Char8 as S

import           Snap.Iteratee
import           Snap.Types

internalError :: S.ByteString -> Snap a
internalError msg =
    let rsp = setContentType "text/plain; charset=utf-8"
            . setContentLength (fromIntegral $ S.length msg)
            . setResponseStatus 500 "Internal Server Error"
            . modifyResponseBody (>. enumBS msg)
            $ emptyResponse

    in finishWith rsp

catch500 :: Snap a -> Snap a
catch500 action = action `catch` handler
  where
    handler :: SomeException -> Snap a'
    handler = internalError
            . S.append "Unhandled error:\r\n\r\n"
            . S.pack
            . show
