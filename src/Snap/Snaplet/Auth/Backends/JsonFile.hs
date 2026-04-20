{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Snap.Snaplet.Auth.Backends.JsonFile
  ( initJsonFileAuthManager
  , mkJsonAuthMgr
  ) where


import           Control.Applicative ((<|>))
import           Control.Monad (join)
import           Control.Monad.State
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Aeson.Parser (json)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as HM
import           Data.Map (Map)
import           Data.Maybe (fromJust, isJust, listToMaybe)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Web.ClientSession
import           System.Directory

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Snap.Snaplet
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet.Auth.AuthManager
import           Snap.Snaplet.Session



------------------------------------------------------------------------------
-- | Initialize a JSON file backed 'AuthManager'
initJsonFileAuthManager :: AuthSettings
                            -- ^ Authentication settings for your app
                        -> SnapletLens b (SessionManager b)
                            -- ^ Lens into a 'SessionManager' auth snaplet will
                           -- use
                        -> FilePath
                            -- ^ Where to store user data as JSON
                        -> SnapletInit b (AuthManager b)
initJsonFileAuthManager s l db = do
    makeSnaplet
        "JsonFileAuthManager"
        "A snaplet providing user authentication using a JSON-file backend"
        Nothing $ liftIO $ do
            rng <- liftIO mkRNG
            key <- getKey (asSiteKey s)
            jsonMgr <- mkJsonAuthMgr db
            return $! AuthManager {
                         backend               = jsonMgr
                       , session               = l
                       , activeUser            = Nothing
                       , minPasswdLen          = asMinPasswdLen s
                       , rememberCookieName    = asRememberCookieName s
                       , rememberCookieDomain  = Nothing
                       , rememberPeriod        = asRememberPeriod s
                       , siteKey               = key
                       , lockout               = asLockout s
                       , randomNumberGenerator = rng
                       }


------------------------------------------------------------------------------
-- | Load/create a datafile into memory cache and return the manager.
--
-- This data type can be used by itself for batch/non-handler processing.
mkJsonAuthMgr :: FilePath -> IO JsonFileAuthManager
mkJsonAuthMgr fp = do
  db <- loadUserCache fp
  let db' = case db of
              Left e  -> error e
              Right x -> x
  cache <- newTVarIO db'

  return $! JsonFileAuthManager {
      memcache = cache
    , dbfile   = fp
  }


------------------------------------------------------------------------------
type UserIdCache = Map UserId AuthUser

#if !MIN_VERSION_aeson(1,0,0)
-- In aeson >= 1 these instances are not needed because we have
-- derived ToJSONKey/FromJSONKey instances for UserId.
instance ToJSON UserIdCache where
  toJSON m = toJSON $ HM.toList m

instance FromJSON UserIdCache where
  parseJSON = fmap HM.fromList . parseJSON
#endif

------------------------------------------------------------------------------
type LoginUserCache = Map Text UserId


------------------------------------------------------------------------------
type EmailUserCache = Map Text UserId


------------------------------------------------------------------------------
type RemTokenUserCache = Map Text UserId


------------------------------------------------------------------------------
-- | JSON user back-end stores the user data and indexes for login and token
-- based logins.
data UserCache = UserCache {
    uidCache    :: UserIdCache          -- ^ the actual datastore
  , loginCache  :: LoginUserCache       -- ^ fast lookup for login field
  , emailCache  :: EmailUserCache       -- ^ fast lookup for email field
  , tokenCache  :: RemTokenUserCache    -- ^ fast lookup for remember tokens
  , uidCounter  :: Int                  -- ^ user id counter
}


------------------------------------------------------------------------------
defUserCache :: UserCache
defUserCache = UserCache {
    uidCache   = HM.empty
  , loginCache = HM.empty
  , emailCache = HM.empty
  , tokenCache = HM.empty
  , uidCounter = 0
}


------------------------------------------------------------------------------
loadUserCache :: FilePath -> IO (Either String UserCache)
loadUserCache fp = do
  chk <- doesFileExist fp
  case chk of
    True -> do
      d <- B.readFile fp
      case Atto.parseOnly json d of
        Left e  -> return $! Left $
                       "Can't open JSON auth backend. Error: " ++ e
        Right v -> case fromJSON v of
          Error e    -> return $! Left $
                        "Malformed JSON auth data store. Error: " ++ e
          Success db -> return $! Right db
    False -> do
      putStrLn "User JSON datafile not found. Creating a new one."
      return $ Right defUserCache


------------------------------------------------------------------------------
data JsonFileAuthManager = JsonFileAuthManager {
    memcache :: TVar UserCache
  , dbfile   :: FilePath
}


------------------------------------------------------------------------------
jsonFileSave :: JsonFileAuthManager
             -> AuthUser
             -> IO (Either AuthFailure AuthUser)
jsonFileSave mgr u = do
    now        <- getCurrentTime
    oldByLogin <- lookupByLogin mgr (userLogin u)
    oldById    <- case userId u of
                    Nothing -> return Nothing
                    Just x  -> lookupByUserId mgr x

    res <- atomically $ do
      cache <- readTVar (memcache mgr)
      res   <- case userId u of
                 Nothing -> create cache now oldByLogin
                 Just _  -> update cache now oldById
      case res of
        Left e             -> return $! Left e
        Right (cache', u') -> do
          writeTVar (memcache mgr) cache'
          return $! Right $! (cache', u')

    case res of
      Left _             -> return $! Left BackendError
      Right (cache', u') -> do
        dumpToDisk cache'
        return $! Right u'

  where
    --------------------------------------------------------------------------
    create :: UserCache
           -> UTCTime
           -> (Maybe AuthUser)
           -> STM (Either AuthFailure (UserCache, AuthUser))
    create cache now old = do
      case old of
        Just _  -> return $! Left DuplicateLogin
        Nothing -> do
          new <- do
            let uid' = UserId . showT $ uidCounter cache + 1
            let u'   = u { userUpdatedAt = Just now, userId = Just uid' }
            return $! cache {
              uidCache   = HM.insert uid' u' $ uidCache cache
            , loginCache = HM.insert (userLogin u') uid' $ loginCache cache
            , emailCache = maybe id (\em -> HM.insert em uid') (userEmail u) $
                           emailCache cache
            , tokenCache = case userRememberToken u' of
                             Nothing -> tokenCache cache
                             Just x  -> HM.insert x uid' $ tokenCache cache
            , uidCounter = uidCounter cache + 1
            }
          return $! Right (new, getLastUser new)

    --------------------------------------------------------------------------
    -- lookup old record, see what's changed and update indexes accordingly
    update :: UserCache
           -> UTCTime
           -> (Maybe AuthUser)
           -> STM (Either AuthFailure (UserCache, AuthUser))
    update cache now old =
      case old of
        Nothing -> return $! Left UserNotFound
        Just x -> do
          let oldLogin = userLogin x
          let oldEmail = userEmail x
          let oldToken = userRememberToken x
          let uid      = fromJust $ userId u
          let newLogin = userLogin u
          let newEmail = userEmail u
          let newToken = userRememberToken u

          let lc       = if oldLogin /= userLogin u
                           then HM.insert newLogin uid $
                                HM.delete oldLogin $
                                loginCache cache
                           else loginCache cache

          let ec       = if oldEmail /= newEmail
                           then (case (oldEmail, newEmail) of
                                   (Nothing, Nothing) -> id
                                   (Just e,  Nothing) -> HM.delete e
                                   (Nothing, Just e ) -> HM.insert e uid
                                   (Just e,  Just e') -> HM.insert e' uid .
                                                         HM.delete e
                                ) (emailCache cache)
                           else emailCache cache

          let tc       = if oldToken /= newToken && isJust oldToken
                           then HM.delete (fromJust oldToken) $ loginCache cache
                           else tokenCache cache

          let tc'      = case newToken of
                           Just t  -> HM.insert t uid tc
                           Nothing -> tc

          let u'       = u { userUpdatedAt = Just now }

          let new      = cache {
                             uidCache   = HM.insert uid u' $ uidCache cache
                           , loginCache = lc
                           , emailCache = ec
                           , tokenCache = tc'
                         }

          return $! Right (new, u')

    --------------------------------------------------------------------------
    -- Sync user database to disk
    -- Need to implement a mutex here; simult syncs could screw things up
    dumpToDisk c = LB.writeFile (dbfile mgr) (encode c)

    --------------------------------------------------------------------------
    -- Gets the last added user
    getLastUser cache = maybe e id $ getUser cache uid
      where
        uid = UserId . showT $ uidCounter cache
        e   = error "getLastUser failed. This should not happen."


------------------------------------------------------------------------------
instance IAuthBackend JsonFileAuthManager where
  save = jsonFileSave

  destroy = error "JsonFile: destroy is not yet implemented"

  lookupByUserId mgr uid = withCache mgr f
    where
      f cache = getUser cache uid

  lookupByLogin mgr login = withCache mgr f
    where
      f cache = getUid >>= getUser cache
        where getUid = HM.lookup login (loginCache cache)

  lookupByEmail mgr email = withCache mgr f
    where
      f cache = getEmail >>= getUser cache
        where getEmail = case HM.lookup email (emailCache cache) of
                      Just u  -> return u
                      Nothing -> (join . fmap userId .
                                  listToMaybe . HM.elems $
                                  HM.filter ((== Just email) . userEmail)
                                  (uidCache  cache))

  lookupByRememberToken mgr token = withCache mgr f
    where
      f cache = getUid >>= getUser cache
        where
          getUid = HM.lookup token (tokenCache cache)


------------------------------------------------------------------------------
withCache :: JsonFileAuthManager -> (UserCache -> a) -> IO a
withCache mgr f = atomically $ do
  cache <- readTVar $ memcache mgr
  return $! f cache


------------------------------------------------------------------------------
getUser :: UserCache -> UserId -> Maybe AuthUser
getUser cache uid = HM.lookup uid (uidCache cache)


------------------------------------------------------------------------------
showT :: Int -> Text
showT = T.pack . show


                             --------------------
                             -- JSON Instances --
                             --------------------

------------------------------------------------------------------------------
instance ToJSON UserCache where
  toJSON uc = object
    [ "uidCache"   .= uidCache   uc
    , "loginCache" .= loginCache uc
    , "emailCache" .= emailCache uc
    , "tokenCache" .= tokenCache uc
    , "uidCounter" .= uidCounter uc
    ]


------------------------------------------------------------------------------
instance FromJSON UserCache where
  parseJSON (Object v) =
    UserCache
      <$> v .: "uidCache"
      <*> v .: "loginCache"
      <*> (v .: "emailCache" <|> pure mempty) -- Old versions of users.json do
                                              -- not carry this field
      <*> v .: "tokenCache"
      <*> v .: "uidCounter"
  parseJSON _ = error "Unexpected JSON input"
