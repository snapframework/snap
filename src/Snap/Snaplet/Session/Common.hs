{-|

  This module contains functionality common among multiple back-ends.

-}

module Snap.Snaplet.Session.Common where


import           Numeric
import           Data.Serialize
import qualified Data.Serialize as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           System.Random.MWC


------------------------------------------------------------------------------
-- | Generates a random salt of given length
randomToken :: Int -> IO ByteString
randomToken n =
  let
    mk :: GenIO -> IO Int
    mk gen = uniformR (0,15) gen
  in do
    is <- withSystemRandom $ \gen -> sequence . take n . repeat $ mk gen
    return . B.pack . concat . map (flip showHex "") $ is


------------------------------------------------------------------------------
-- | Generate a randomized CSRF token
mkCSRFToken :: IO Text
mkCSRFToken = T.decodeUtf8 `fmap` randomToken 40


instance Serialize Text where
  put = S.put . T.encodeUtf8
  get = T.decodeUtf8 `fmap` S.get

