{-# LANGUAGE TypeSynonymInstances #-}

module Snap.Snaplet.Auth.Backends.JsonFile where


import           Control.Applicative
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as HM
import           Data.Map (Map)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as T

import           Snap.Snaplet.Auth.Types


type UserIdCache = Map UserId AuthUser

instance ToJSON UserIdCache where
  toJSON m = toJSON $ HM.toList m

instance FromJSON UserIdCache where
  parseJSON = fmap HM.fromList . parseJSON

type LoginUserCache = Map Text UserId


type RemTokenUserCache = Map Text UserId


data UserCache = UserCache {
	  uidCache :: UserIdCache
	, loginCache :: LoginUserCache
	, tokenCache :: RemTokenUserCache
	, uidCounter :: Int
}

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

  destroy = undefined

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
