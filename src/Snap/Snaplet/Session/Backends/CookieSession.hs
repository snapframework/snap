{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snap.Snaplet.Session.Backends.CookieSession 

( initCookieSessionManager ) where

import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Generics
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import qualified Data.Serialize as S
import           Data.Text (Text)
import           Web.ClientSession

import           Snap.Core (Snap)
import           Snap.Snaplet
import           Snap.Snaplet.Session.Common (mkCSRFToken)
import           Snap.Snaplet.Session.SessionManager
import           Snap.Snaplet.Session.SecureCookie


-- | Session data are kept in a 'HashMap' for this backend
type Session = HashMap Text Text


-- | This is what the 'Payload' will be for the CookieSession backend
data CookieSession = CookieSession
  { csCSRFToken :: Text
  , csSession :: Session
} deriving (Eq, Show)


instance Serialize CookieSession where
  put (CookieSession a b) = S.put (a,b)
  get                     = (\(a,b) -> CookieSession a b) `fmap` S.get

instance (Serialize k, Serialize v, Hashable k, Eq k) => 
  Serialize (HashMap k v) where
  put = S.put . HM.toList
  get = HM.fromList `fmap` S.get


mkCookieSession :: IO CookieSession
mkCookieSession = do
  t <- liftIO $ mkCSRFToken
  return $ CookieSession t HM.empty


-- | The manager data type to be stuffed into 'SessionManager'
data CookieSessionManager = CookieSessionManager {
    session :: Maybe CookieSession
  -- ^ Per request cache for 'CookieSession'

  , siteKey :: ByteString
  -- ^ A long encryption key used for secure cookie transport

  , cookieName :: ByteString
  -- ^ Cookie name for the session system

  , timeOut :: Maybe Int
  -- ^ Session cookies will be considered "stale" after this many seconds.
} deriving (Eq,Show,Typeable)


loadDefSession :: CookieSessionManager -> IO CookieSessionManager
loadDefSession mgr@(CookieSessionManager ses _ _ _) = do
  case ses of
    Nothing -> do
      ses' <- mkCookieSession
      return $ mgr { session = Just ses' }
    Just _ -> return mgr


modSession :: (Session -> Session) -> CookieSession -> CookieSession
modSession f (CookieSession t ses) = CookieSession t (f ses)


-- | Initialize a cookie-backed session, returning a 'SessionManager' to be
-- stuffed inside your application's state. This 'SessionManager' will enable
-- the use of all session storage functionality defined in
-- 'Snap.Snaplet.Session'
initCookieSessionManager 
  :: FilePath             -- ^ Path to site-wide encryption key
  -> ByteString           -- ^ Session cookie name
  -> Maybe Int            -- ^ Session time-out (replay attack protection)
  -> SnapletInit b SessionManager
initCookieSessionManager fp cn to = 
  makeSnaplet "CookieSession" "A snaplet providing sessions via HTTP cookies."
         Nothing $ liftIO $ do
    key <- getKey fp
    return . SessionManager $ CookieSessionManager Nothing key cn to 


instance ISessionManager CookieSessionManager where
  load mgr@(CookieSessionManager r _ _ _) = do
    case r of
      Just _ -> return mgr 
      Nothing -> do
        pl <- getPayload mgr
        case pl of
          Nothing -> liftIO $ loadDefSession mgr
          Just (Payload x) -> do
            let c = S.decode x
            case c of 
              Left _ -> liftIO $ loadDefSession mgr
              Right cs -> return $ mgr { session = Just cs }

  commit mgr@(CookieSessionManager r _ _ _) = do
    pl <- case r of
      Just r' -> return . Payload $ S.encode r'
      Nothing -> liftIO mkCookieSession >>= return . Payload . S.encode
    setPayload mgr pl

  reset mgr = do
    cs <- liftIO mkCookieSession 
    return $ mgr { session = Just cs }

  touch = id

  insert k v mgr@(CookieSessionManager r _ _ _) = case r of
    Just r' -> mgr { session = Just $ modSession (HM.insert k v) r' }
    Nothing -> mgr

  lookup k (CookieSessionManager r _ _ _) = r >>= HM.lookup k . csSession 

  delete k mgr@(CookieSessionManager r _ _ _) = case r of
    Just r' -> mgr { session = Just $ modSession (HM.delete k) r' }
    Nothing -> mgr

  csrf (CookieSessionManager r _ _ _) = case r of
    Just r' -> csCSRFToken r'
    Nothing -> ""

  toList (CookieSessionManager r _ _ _) = case r of
    Just r' -> HM.toList . csSession $ r'
    Nothing -> []



-- | A session payload to be stored in a SecureCookie.
newtype Payload = Payload ByteString
  deriving (Eq, Show, Ord, Serialize)


-- | Get the current client-side value
getPayload :: CookieSessionManager -> Snap (Maybe Payload)
getPayload mgr = getSecureCookie (cookieName mgr) (siteKey mgr) (timeOut mgr)


-- | Set the client-side value
setPayload :: CookieSessionManager -> Payload -> Snap ()
setPayload mgr x = setSecureCookie (cookieName mgr) (siteKey mgr) (timeOut mgr) x

  
