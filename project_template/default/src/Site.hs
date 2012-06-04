{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App (AuthManager App) ()
index =
  ifTop $ requireUser auth (handleLogin Nothing) loggedIn where
    loggedIn = do
      acc <- fmap (maybe "" userLogin) currentUser
      heistLocal (bindString "login" acc) $ render "index"

-- Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError =
  heistLocal (bindSplices errs) $ render "login" where
    errs = [("loginError", textSplice c) | c <- maybeToList authError]

-- Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
  loginUser "login" "password" Nothing (\_ -> handleLogin err) (redirect "/") where
    err = Just . T.pack $ "Unknown user or password"

handleLogout :: Handler App (AuthManager App) ()
handleLogout = do
  logout
  redirect "/"

-- Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit where
  handleForm =
    heistLocal (bindSplices []) $ render "new_user"

  handleFormSubmit = do
    registerUser "login" "password"
    redirect "/"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",         with auth $ index)
         , ("/login",    with auth $ handleLoginSubmit)
         , ("/logout",   with auth $ handleLogout)
         , ("/new_user", with auth $ handleNewUser)
         , ("",          with heist heistServe)
         , ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices auth
    return $ App h s a
