{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | This is a support module meant to back all session back-end
-- implementations.
--
-- It gives us an encrypted and timestamped cookie that can store an arbitrary
-- serializable payload. For security, it will:
--
--   * Encrypt its payload together with a timestamp.
--
--   * Check the timestamp for session expiration everytime you read from the
--     cookie. This will limit intercept-and-replay attacks by disallowing
--     cookies older than the timeout threshold.

module Snap.Snaplet.Session.SecureCookie
       ( SecureCookie
       , getSecureCookie
       , setSecureCookie
       , expireSecureCookie
       -- ** Helper functions
       , encodeSecureCookie
       , decodeSecureCookie
       , checkTimeout
       ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString       (ByteString)
import           Data.Serialize
import           Data.Time
import           Data.Time.Clock.POSIX
import           Snap.Core
import           Web.ClientSession

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

------------------------------------------------------------------------------
-- | Arbitrary payload with timestamp.
type SecureCookie t = (UTCTime, t)


------------------------------------------------------------------------------
-- | Get the cookie payload.
getSecureCookie :: (MonadSnap m, Serialize t)
                => ByteString       -- ^ Cookie name
                -> Key              -- ^ Encryption key
                -> Maybe Int        -- ^ Timeout in seconds
                -> m (Maybe t)
getSecureCookie name key timeout = do
    rqCookie <- getCookie name
    rspCookie <- getResponseCookie name <$> getResponse
    let ck = rspCookie `mplus` rqCookie
    let val = fmap cookieValue ck >>= decodeSecureCookie key
    case val of
      Nothing -> return Nothing
      Just (ts, t) -> do
          to <- checkTimeout timeout ts
          return $ case to of
            True -> Nothing
            False -> Just t


------------------------------------------------------------------------------
-- | Decode secure cookie payload wih key.
decodeSecureCookie  :: Serialize a
                     => Key                     -- ^ Encryption key
                     -> ByteString              -- ^ Encrypted payload
                     -> Maybe (SecureCookie a)
decodeSecureCookie key value = do
    cv <- decrypt key value
    (i, val) <- either (const Nothing) Just $ decode cv
    return $ (posixSecondsToUTCTime (fromInteger i), val)


------------------------------------------------------------------------------
-- | Inject the payload.
setSecureCookie :: (MonadSnap m, Serialize t)
                => ByteString       -- ^ Cookie name
                -> Maybe ByteString -- ^ Cookie domain
                -> Key              -- ^ Encryption key
                -> Maybe Int        -- ^ Max age in seconds
                -> t                -- ^ Serializable payload
                -> m ()
setSecureCookie name domain key to val = do
    t <- liftIO getCurrentTime
    val' <- encodeSecureCookie key (t, val)
    let expire = to >>= Just . flip addUTCTime t . fromIntegral
    let nc = Cookie name val' expire domain (Just "/") False True
    modifyResponse $ addResponseCookie nc


------------------------------------------------------------------------------
-- | Encode SecureCookie with key into injectable payload
encodeSecureCookie :: (MonadIO m, Serialize t)
                    => Key            -- ^ Encryption key
                    -> SecureCookie t -- ^ Payload
                    -> m ByteString
encodeSecureCookie key (t, val) =
    liftIO $ encryptIO key . encode $ (seconds, val)
  where
    seconds = round (utcTimeToPOSIXSeconds t) :: Integer


------------------------------------------------------------------------------
-- | Expire secure cookie
expireSecureCookie :: MonadSnap m
                   => ByteString       -- ^ Cookie name
                   -> Maybe ByteString -- ^ Cookie domain
                   -> m ()
expireSecureCookie name domain = expireCookie cookie
  where
    cookie = Cookie name "" Nothing domain (Just "/") False False


------------------------------------------------------------------------------
-- | Validate session against timeout policy.
--
-- * If timeout is set to 'Nothing', never trigger a time-out.
--
-- * Otherwise, do a regular time-out check based on current time and given
--   timestamp.
checkTimeout :: (MonadSnap m) => Maybe Int -> UTCTime -> m Bool
checkTimeout Nothing _ = return False
checkTimeout (Just x) t0 = do
    t1 <- liftIO getCurrentTime
    return $ t1 > addUTCTime (fromIntegral x) t0
