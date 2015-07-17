{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}

------------------------------------------------------------------------------
-- | Pre-packaged Handlers that deal with form submissions and standard
--   use-cases involving authentication.

module Snap.Snaplet.Auth.Handlers where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Serialize hiding (get)
import           Data.Time
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text, null, strip)
import           Prelude hiding (null)
import           Web.ClientSession
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth.AuthManager
import           Snap.Snaplet.Auth.Handlers.Errors
import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet.Session
------------------------------------------------------------------------------


                         ----------------------------
                         -- Higher level functions --
                         ----------------------------

------------------------------------------------------------------------------
-- | Create a new user from just a username and password
--
createUser :: Text              -- ^ Username
           -> ByteString        -- ^ Password
           -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
createUser unm pwd
  | null $ strip unm = return $ Left UsernameMissing
  | otherwise = do
     uExists <- usernameExists unm
     if uExists then return $ Left DuplicateLogin
                else withBackend $ \r -> liftIO $ buildAuthUser r unm pwd


------------------------------------------------------------------------------
-- | Check whether a user with the given username exists.
--
usernameExists :: Text          -- ^ The username to be checked
               -> Handler b (AuthManager b) Bool
usernameExists username =
    withBackend $ \r -> liftIO $ isJust <$> lookupByLogin r username


------------------------------------------------------------------------------
-- | Lookup a user by her username, check given password and perform login
--
loginByUsername :: Text             -- ^ Username/login for user
                -> Password         -- ^ Should be ClearText
                -> Bool             -- ^ Set remember token?
                -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
loginByUsername _ (Encrypted _) _ = return $ Left EncryptedPassword
loginByUsername unm pwd shouldRemember = do
    sk <- gets siteKey
    cn <- gets rememberCookieName
    cd <- gets rememberCookieDomain
    rp <- gets rememberPeriod
    withBackend $ loginByUsername' sk cn cd rp

  where
    --------------------------------------------------------------------------
    loginByUsername' :: (IAuthBackend t) =>
                        Key
                     -> ByteString
                     -> Maybe ByteString
                     -> Maybe Int
                     -> t
                     -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
    loginByUsername' sk cn cd rp r =
        liftIO (lookupByLogin r unm) >>=
        maybe (return $! Left UserNotFound) found

      where
        ----------------------------------------------------------------------
        found user = checkPasswordAndLogin user pwd >>=
                     either (return . Left) matched

        ----------------------------------------------------------------------
        matched user
            | shouldRemember = do
                  token <- gets randomNumberGenerator >>=
                           liftIO . randomToken 64

                  setRememberToken sk cn cd rp token

                  let user' = user {
                                userRememberToken = Just (decodeUtf8 token)
                              }

                  saveUser user'
                  return $! Right user'

            | otherwise = return $ Right user


------------------------------------------------------------------------------
-- | Remember user from the remember token if possible and perform login
--
loginByRememberToken :: Handler b (AuthManager b) (Either AuthFailure AuthUser)
loginByRememberToken = withBackend $ \impl -> do
    key         <- gets siteKey
    cookieName_ <- gets rememberCookieName
    period      <- gets rememberPeriod

    runEitherT $ do
        token <- noteT (AuthError "loginByRememberToken: no remember token") $
                   MaybeT $ getRememberToken key cookieName_ period
        user  <- noteT (AuthError "loginByRememberToken: no remember token") $
                   MaybeT $ liftIO $ lookupByRememberToken impl
                                   $ decodeUtf8 token
        lift $ forceLogin user
        return user


------------------------------------------------------------------------------
-- | Logout the active user
--
logout :: Handler b (AuthManager b) ()
logout = do
    s <- gets session
    withTop s $ withSession s removeSessionUserId
    rc <- gets rememberCookieName
    rd <- gets rememberCookieDomain
    expireSecureCookie rc rd
    modify $ \mgr -> mgr { activeUser = Nothing }


------------------------------------------------------------------------------
-- | Return the current user; trying to remember from cookie if possible.
--
currentUser :: Handler b (AuthManager b) (Maybe AuthUser)
currentUser = cacheOrLookup $ withBackend $ \r -> do
    s   <- gets session
    uid <- withTop s getSessionUserId
    case uid of
      Nothing -> hush <$> loginByRememberToken
      Just uid' -> liftIO $ lookupByUserId r uid'


------------------------------------------------------------------------------
-- | Convenience wrapper around 'rememberUser' that returns a bool result
--
isLoggedIn :: Handler b (AuthManager b) Bool
isLoggedIn = isJust <$> currentUser


------------------------------------------------------------------------------
-- | Create or update a given user
--
saveUser :: AuthUser -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
saveUser u
    | null $ userLogin u = return $ Left UsernameMissing
    | otherwise = withBackend $ \r -> liftIO $ save r u


------------------------------------------------------------------------------
-- | Destroy the given user
--
destroyUser :: AuthUser -> Handler b (AuthManager b) ()
destroyUser u = withBackend $ liftIO . flip destroy u


                      -----------------------------------
                      --  Lower level helper functions --
                      -----------------------------------

------------------------------------------------------------------------------
-- | Mutate an 'AuthUser', marking failed authentication
--
-- This will save the user to the backend.
--
markAuthFail :: AuthUser
             -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
markAuthFail u = withBackend $ \r -> do
    lo <- gets lockout
    incFailCtr u >>= checkLockout lo >>= liftIO . save r

  where
    --------------------------------------------------------------------------
    incFailCtr u' = return $ u' {
                      userFailedLoginCount = userFailedLoginCount u' + 1
                    }

    --------------------------------------------------------------------------
    checkLockout lo u' =
        case lo of
          Nothing          -> return u'
          Just (mx, wait)  ->
              if userFailedLoginCount u' >= mx
                then do
                  now <- liftIO getCurrentTime
                  let reopen = addUTCTime wait now
                  return $! u' { userLockedOutUntil = Just reopen }
                else return u'


------------------------------------------------------------------------------
-- | Mutate an 'AuthUser', marking successful authentication
--
-- This will save the user to the backend.
--
markAuthSuccess :: AuthUser
                -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
markAuthSuccess u = withBackend $ \r ->
                        incLoginCtr u     >>=
                        updateIp          >>=
                        updateLoginTS     >>=
                        resetFailCtr      >>=
                        liftIO . save r
  where
    --------------------------------------------------------------------------
    incLoginCtr u' = return $ u' { userLoginCount = userLoginCount u' + 1 }

    --------------------------------------------------------------------------
    updateIp u' = do
        ip <- rqClientAddr <$> getRequest
        return $ u' { userLastLoginIp = userCurrentLoginIp u'
                    , userCurrentLoginIp = Just ip }

    --------------------------------------------------------------------------
    updateLoginTS u' = do
        now <- liftIO getCurrentTime
        return $
          u' { userCurrentLoginAt = Just now
             , userLastLoginAt = userCurrentLoginAt u' }

    --------------------------------------------------------------------------
    resetFailCtr u' = return $ u' { userFailedLoginCount = 0
                                  , userLockedOutUntil = Nothing }


------------------------------------------------------------------------------
-- | Authenticate and log the user into the current session if successful.
--
-- This is a mid-level function exposed to allow roll-your-own ways of looking
-- up a user from the database.
--
-- This function will:
--
-- 1. Check the password
--
-- 2. Login the user into the current session
--
-- 3. Mark success/failure of the authentication trial on the user record
--
checkPasswordAndLogin
  :: AuthUser               -- ^ An existing user, somehow looked up from db
  -> Password               -- ^ A ClearText password
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
checkPasswordAndLogin u pw =
    case userLockedOutUntil u of
      Just x -> do
        now <- liftIO getCurrentTime
        if now > x
          then auth u
          else return . Left $ LockedOut x
      Nothing -> auth u

  where
    auth :: AuthUser -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
    auth user =
      case authenticatePassword user pw of
        Just e -> do
          markAuthFail user
          return $ Left e

        Nothing -> do
          forceLogin user
          modify (\mgr -> mgr { activeUser = Just user })
          markAuthSuccess user


------------------------------------------------------------------------------
-- | Login and persist the given 'AuthUser' in the active session
--
-- Meant to be used if you have other means of being sure that the person is
-- who she says she is.
--
forceLogin :: AuthUser       -- ^ An existing user, somehow looked up from db
           -> Handler b (AuthManager b) (Either AuthFailure ())
forceLogin u = do
    s <- gets session
    withSession s $
        case userId u of
          Just x -> do
            withTop s (setSessionUserId x)
            return $ Right ()
          Nothing -> return . Left $
                     AuthError $ "forceLogin: Can't force the login of a user "
                                   ++ "without userId"


                     ------------------------------------
                     -- Internal, non-exported helpers --
                     ------------------------------------


------------------------------------------------------------------------------
getRememberToken :: (Serialize t, MonadSnap m)
                 => Key
                 -> ByteString
                 -> Maybe Int
                 -> m (Maybe t)
getRememberToken sk rc rp = getSecureCookie rc sk rp


------------------------------------------------------------------------------
setRememberToken :: (Serialize t, MonadSnap m)
                 => Key
                 -> ByteString
                 -> Maybe ByteString
                 -> Maybe Int
                 -> t
                 -> m ()
setRememberToken sk rc rd rp token = setSecureCookie rc rd sk rp token


------------------------------------------------------------------------------
-- | Set the current user's 'UserId' in the active session
--
setSessionUserId :: UserId -> Handler b SessionManager ()
setSessionUserId (UserId t) = setInSession "__user_id" t


------------------------------------------------------------------------------
-- | Remove 'UserId' from active session, effectively logging the user out.
removeSessionUserId :: Handler b SessionManager ()
removeSessionUserId = deleteFromSession "__user_id"


------------------------------------------------------------------------------
-- | Get the current user's 'UserId' from the active session
--
getSessionUserId :: Handler b SessionManager (Maybe UserId)
getSessionUserId = do
  uid <- getFromSession "__user_id"
  return $ liftM UserId uid


------------------------------------------------------------------------------
-- | Check password for a given user.
--
-- Returns "Nothing" if check is successful and an "IncorrectPassword" error
-- otherwise
--
authenticatePassword :: AuthUser        -- ^ Looked up from the back-end
                     -> Password        -- ^ Check against this password
                     -> Maybe AuthFailure
authenticatePassword u pw = auth
  where
    auth    = case userPassword u of
                Nothing -> Just PasswordMissing
                Just upw -> check $ checkPassword pw upw

    check b = if b then Nothing else Just IncorrectPassword


------------------------------------------------------------------------------
-- | Wrap lookups around request-local cache
--
cacheOrLookup
  :: Handler b (AuthManager b) (Maybe AuthUser)
      -- ^ Lookup action to perform if request local cache is empty
  -> Handler b (AuthManager b) (Maybe AuthUser)
cacheOrLookup f = do
    au <- gets activeUser
    if isJust au
      then return au
      else do
        au' <- f
        modify (\mgr -> mgr { activeUser = au' })
        return au'


------------------------------------------------------------------------------
-- | Register a new user by specifying login and password 'Param' fields
--
registerUser
  :: ByteString            -- ^ Login field
  -> ByteString            -- ^ Password field
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
registerUser lf pf = do
    l <- fmap decodeUtf8 <$> getParam lf
    p <- getParam pf

    let l' = note UsernameMissing l
    let p' = note PasswordMissing p

    -- In case of multiple AuthFailure, the first available one
    -- will be propagated.
    case liftM2 (,) l' p' of
      Left e           -> return $ Left e
      Right (lgn, pwd) -> createUser lgn pwd


------------------------------------------------------------------------------
-- | A 'MonadSnap' handler that processes a login form.
--
-- The request paremeters are passed to 'performLogin'
--
-- To make your users stay logged in for longer than the session replay
-- prevention timeout, you must pass a field name as the third parameter and
-- that field must be set to a value of \"1\" by the submitting form.  This
-- lets you use a user selectable check box.  Or if you want user remembering
-- always turned on, you can use a hidden form field.
loginUser
  :: ByteString
      -- ^ Username field
  -> ByteString
      -- ^ Password field
  -> Maybe ByteString
      -- ^ Remember field; Nothing if you want no remember function.
  -> (AuthFailure -> Handler b (AuthManager b) ())
      -- ^ Upon failure
  -> Handler b (AuthManager b) ()
      -- ^ Upon success
  -> Handler b (AuthManager b) ()
loginUser unf pwdf remf loginFail loginSucc =
    runEitherT (loginUser' unf pwdf remf)
    >>= either loginFail (const loginSucc)


------------------------------------------------------------------------------
loginUser' :: ByteString
           -> ByteString
           -> Maybe ByteString
           -> EitherT AuthFailure (Handler b (AuthManager b)) AuthUser
loginUser' unf pwdf remf = do
    mbUsername <- lift $ getParam unf
    mbPassword <- lift $ getParam pwdf
    remember   <- lift $ liftM (fromMaybe False)
                    (runMaybeT $
                    do field <- MaybeT $ return remf
                       value <- MaybeT $ getParam field
                       return $ value == "1" || value == "on")

    password <- noteT PasswordMissing $ hoistMaybe mbPassword
    username <- noteT UsernameMissing $ hoistMaybe mbUsername

    EitherT $ loginByUsername (decodeUtf8 username)
                              (ClearText password) remember


------------------------------------------------------------------------------
-- | Simple handler to log the user out. Deletes user from session.
--
logoutUser :: Handler b (AuthManager b) ()   -- ^ What to do after logging out
           -> Handler b (AuthManager b) ()
logoutUser target = logout >> target


------------------------------------------------------------------------------
-- | Require that an authenticated 'AuthUser' is present in the current
-- session.
--
-- This function has no DB cost - only checks to see if a user_id is present
-- in the current session.
--
requireUser :: SnapletLens b (AuthManager b)
                -- ^ Lens reference to an "AuthManager"
            -> Handler b v a
                -- ^ Do this if no authenticated user is present.
            -> Handler b v a
                -- ^ Do this if an authenticated user is present.
            -> Handler b v a
requireUser auth bad good = do
    loggedIn <- withTop auth isLoggedIn
    if loggedIn then good else bad


------------------------------------------------------------------------------
-- | Run a function on the backend, and return the result.
--
-- This uses an existential type so that the backend type doesn't
-- 'escape' AuthManager.  The reason that the type is Handler b
-- (AuthManager v) a and not a is because anything that uses the
-- backend will return an IO something, which you can liftIO, or a
-- Handler b (AuthManager v) a if it uses other handler things.
--
withBackend ::
    (forall r. (IAuthBackend r) => r -> Handler b (AuthManager v) a)
      -- ^ The function to run with the handler.
  -> Handler b (AuthManager v) a
withBackend f = join $ do
  (AuthManager backend_ _ _ _ _ _ _ _ _ _) <- get
  return $ f backend_


------------------------------------------------------------------------------
-- | This function generates a random password reset token and stores it in
-- the database for the user.  Call this function when a user forgets their
-- password.  Then use the token to autogenerate a link that the user can
-- visit to reset their password.  This function also sets a timestamp so the
-- reset token can be expired.
setPasswordResetToken :: Text -> Handler b (AuthManager b) (Maybe Text)
setPasswordResetToken login = do
  tokBS <- liftIO . randomToken 40 =<< gets randomNumberGenerator
  let token = decodeUtf8 tokBS
  now <- liftIO getCurrentTime
  success <- modPasswordResetToken login (Just token) (Just now)
  return $ if success then Just token else Nothing


------------------------------------------------------------------------------
-- | Clears a user's password reset token.  Call this when the user
-- successfully changes their password to ensure that the password reset link
-- cannot be used again.
clearPasswordResetToken :: Text -> Handler b (AuthManager b) Bool
clearPasswordResetToken login = modPasswordResetToken login Nothing Nothing


------------------------------------------------------------------------------
-- | Helper function used for setting and clearing the password reset token
-- and associated timestamp.
modPasswordResetToken :: Text
                      -> Maybe Text
                      -> Maybe UTCTime
                      -> Handler v (AuthManager v) Bool
modPasswordResetToken login token timestamp = do
  res <- runMaybeT $ do
      u <- MaybeT $ withBackend $ \b -> liftIO $ lookupByLogin b login
      lift $ saveUser $ u
        { userResetToken = token
        , userResetRequestedAt = timestamp
        }
      return ()
  return $ maybe False (\_ -> True) res



