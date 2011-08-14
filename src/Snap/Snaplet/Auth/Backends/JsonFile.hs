{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}

module Snap.Snaplet.Auth.Backends.JsonFile where


import           Control.Applicative
import           Control.Monad.State
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.Attoparsec as Atto
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as HM
import           Data.Map (Map)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Lens.Lazy
import           Web.ClientSession
import           System.Directory

import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet
import           Snap.Snaplet.Session



------------------------------------------------------------------------------
-- | Initialize a JSON file backed 'AuthManager'
initJsonFileAuthManager 
  :: AuthSettings
  -- ^ Authentication settings for your app
  -> Lens b (Snaplet SessionManager)
  -- ^ Lens into a 'SessionManager' auth snaplet will use
  -> FilePath
  -- ^ Where to store user data as JSON
  -> SnapletInit b (AuthManager b)
initJsonFileAuthManager s l db = 
  makeSnaplet "JsonFileAuthManager" 
              "A snaplet providing user authentication using a JSON-file backend"
              Nothing $ liftIO $ do
    key <- getKey (asSiteKey s)
    jsonMgr <- mkJsonAuthMgr db
    return $ AuthManager {
    	  backend = jsonMgr
    	, session = l
    	, activeUser = Nothing
    	, minPasswdLen = asMinPasswdLen s
    	, rememberCookieName = asRememberCookieName s
    	, rememberPeriod = asRememberPeriod s
    	, siteKey = key
    	, lockout = asLockout s 
    }


-- Load an existing datafile into memory cache
mkJsonAuthMgr :: FilePath -> IO JsonFileAuthManager
mkJsonAuthMgr fp = do
  db <- loadUserCache fp
  let db' = case db of
              Left e -> error e
              Right x -> x
  cache <- newMVar db'
  return $ JsonFileAuthManager {
      memcache = cache
    , dbfile = fp
  }


type UserIdCache = Map UserId AuthUser


instance ToJSON UserIdCache where
  toJSON m = toJSON $ HM.toList m


instance FromJSON UserIdCache where
  parseJSON = fmap HM.fromList . parseJSON


type LoginUserCache = Map Text UserId


type RemTokenUserCache = Map Text UserId


-- JSON user back-end stores the user data and indexes for login and token
-- based logins.
data UserCache = UserCache {
	  uidCache    :: UserIdCache          -- the actual datastore
	, loginCache  :: LoginUserCache       -- fast lookup for login field
	, tokenCache  :: RemTokenUserCache    -- fast lookup for remember tokens
	, uidCounter  :: Int                  -- user id counter
}


defUserCache = UserCache {
	  uidCache = HM.empty
	, loginCache = HM.empty
	, tokenCache = HM.empty
	, uidCounter = 0
}


loadUserCache :: FilePath -> IO (Either String UserCache)
loadUserCache fp = do
  chk <- doesFileExist fp
  case chk of
    True -> do
      d <- B.readFile fp
      case Atto.parseOnly json d of
        Left e -> return . Left $ "Can't open JSON auth backend. Error: " ++ e
        Right v -> case fromJSON v of
          Error e -> return . Left $ "Malformed JSON auth data store. Error: " ++ e
          Success db -> return $ Right db
    False -> do
      putStrLn "User JSON datafile not found. Creating a new one."
      return $ Right defUserCache


data JsonFileAuthManager = JsonFileAuthManager {
	  memcache :: MVar UserCache
	, dbfile :: FilePath
}


instance IAuthBackend JsonFileAuthManager where

  -- this is currently wrong. Some fields in AuthUser should change as a result
  -- of saving - like a unique ID being assigned.
  save mgr u = modifyMVar (memcache mgr) f 

    where

      -- Atomically update the cache and dump to disk
      f cache = dumpToDisk new >> return (new, getLastUser new)
        where new = updateCache cache 

      dumpToDisk c = LB.writeFile (dbfile mgr) (encode c)

      updateCache cache = cache { uidCache = uidc 
                                , loginCache = lc 
                                , tokenCache = tc 
                                , uidCounter = ctr }
        where
          -- Assign a userid if it is missing in the given user.
          uid' = maybe (UserId . showT $ uidCounter cache + 1) id $ userId u 

          -- New user might have a newly assigned userid field
          u' = u { userId = Just uid' }

          -- Update caches
          uidc = HM.insert uid' u' $ uidCache cache
          lc = HM.insert (userLogin u') uid' $ loginCache cache
          tc = case userRememberToken u' of
            Nothing -> tokenCache cache 
            Just x -> HM.insert x uid' $ tokenCache cache

          -- Increment counter if a new id has been assigned 
          ctr = if isNothing (userId u) then 
                  uidCounter cache + 1 
                else uidCounter cache

      -- Get's the last added user
      getLastUser cache = maybe e id $ getUser cache uid
        where uid = UserId . showT $ uidCounter cache
              e = error "getLastUser failed. This should not happen."

  destroy = error "JsonFile: destroy is not yet implemented"

  lookupByUserId mgr uid = withCache mgr f
    where f cache = return $ getUser cache uid

  lookupByLogin mgr login = withCache mgr f
    where 
      f cache = return $ getUid >>= getUser cache
        where getUid = HM.lookup login (loginCache cache)
              
  lookupByRememberToken mgr token = withCache mgr f
    where
      f cache = return $ getUid >>= getUser cache
        where getUid = HM.lookup token (tokenCache cache)


withCache mgr f = withMVar (memcache mgr) f

getUser cache uid = HM.lookup uid (uidCache cache)


------------------------------------------------------------------------------
-- JSON Instances
--
------------------------------------------------------------------------------


instance ToJSON UserCache where
  toJSON uc = object 
    [ "uidCache" .= uidCache uc
    , "loginCache" .= loginCache uc
    , "tokenCache" .= tokenCache uc 
    , "uidCounter" .= uidCounter uc]


instance FromJSON UserCache where
  parseJSON (Object v) = 
    UserCache
      <$> v .: "uidCache"
      <*> v .: "loginCache"
      <*> v .: "tokenCache"
      <*> v .: "uidCounter"


instance ToJSON AuthUser where
  toJSON u = object
    [ "uid" .= userId u
    , "login" .= userLogin u
    , "pw" .= userPassword u
    , "activated_at" .= userActivatedAt u
    , "suspended_at" .= userSuspendedAt u
    , "remember_token" .= userRememberToken u
    , "login_count" .= userLoginCount u
    , "failed_login_count" .= userFailedLoginCount u
    , "locked_at" .= userLockedOutAt u
    , "current_login_at" .= userCurrentLoginAt u
    , "last_login_at" .= userLastLoginAt u
    , "current_ip" .= userCurrentLoginIp u
    , "last_ip" .= userLastLoginIp u
    , "created_at" .= userCreatedAt u
    , "updated_at" .= userUpdatedAt u
    , "meta" .= userMeta u ]


instance FromJSON AuthUser where
  parseJSON (Object v) = AuthUser
    <$> v .: "uid"
    <*> v .: "login"
    <*> v .: "pw"
    <*> v .: "activated_at"
    <*> v .: "suspended_at"
    <*> v .: "remember_token"
    <*> v .: "login_count"
    <*> v .: "failed_login_count"
    <*> v .: "locked_at"
    <*> v .: "current_login_at"
    <*> v .: "last_login_at"
    <*> v .: "current_ip"
    <*> v .: "last_ip"
    <*> v .: "created_at"
    <*> v .: "updated_at"
    <*> return []
    <*> v .: "meta"


instance ToJSON Password where
  toJSON (ClearText _) = error "ClearText passwords can't be serialized into JSON"
  toJSON (Encrypted x) = toJSON x


instance FromJSON Password where
  parseJSON = fmap Encrypted . parseJSON
  

showT = T.pack . show
