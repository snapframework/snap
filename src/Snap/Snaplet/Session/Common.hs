------------------------------------------------------------------------------
-- | This module contains functionality common among multiple back-ends.
--

module Snap.Snaplet.Session.Common
  ( RNG
  , mkRNG
  , withRNG
  , randomToken
  , mkCSRFToken
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Numeric
import           System.Random.MWC


------------------------------------------------------------------------------
-- | High speed, mutable random number generator state
newtype RNG = RNG (MVar GenIO)

------------------------------------------------------------------------------
-- | Perform given action, mutating the RNG state
withRNG :: RNG
        -> (GenIO -> IO a)
        -> IO a
withRNG (RNG rng) m = withMVar rng m


------------------------------------------------------------------------------
-- | Create a new RNG
mkRNG :: IO RNG
mkRNG = withSystemRandom (newMVar >=> return . RNG)


------------------------------------------------------------------------------
-- | Generates a random salt of given length
randomToken :: Int -> RNG -> IO ByteString
randomToken n rng = do
    is <- withRNG rng $ \gen -> sequence . take n . repeat $ mk gen
    return . B.pack . concat . map (flip showHex "") $ is
  where
    mk :: GenIO -> IO Int
    mk = uniformR (0,15)


------------------------------------------------------------------------------
-- | Generate a randomized CSRF token
mkCSRFToken :: RNG -> IO Text
mkCSRFToken rng = T.decodeUtf8 <$> randomToken 40 rng


