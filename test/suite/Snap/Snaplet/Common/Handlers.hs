module Snap.Snaplet.Common.Handlers where

------------------------------------------------------------------------------
import Control.Lens                                  (makeLenses)
import Control.Monad.IO.Class                        (liftIO)
------------------------------------------------------------------------------
import Snap.Http.Server.Config                       (Config, completeConfig,
                                                      defaultConfig)
import Snap.Snaplet                                  (Handler, Initializer,
                                                      Snaplet, SnapletInit,
                                                      makeSnaplet,
                                                      nestSnaplet,
                                                      subSnaplet)
import Snap.Snaplet.Auth                             (AuthManager,
                                                      defAuthSettings)
import Snap.Snaplet.Auth.Backends.JsonFile           (initJsonFileAuthManager)
import Snap.Snaplet.Common.FooSnaplet
import Snap.Snaplet.Common.BarSnaplet
import Snap.Snaplet.Common.Types
import Snap.Snaplet.Config                           (AppConfig,
                                                      commandLineAppConfig)
import Snap.Snaplet.Heist                            (Heist, HasHeist,
                                                      heistInit, heistLens)
import Snap.Snaplet.HeistNoClass                     (setInterpreted)
import Snap.Snaplet.Session                          (SessionManager)
import Snap.Snaplet.Session.Backends.CookieSession   (initCookieSessionManager)


-------------------------------------------------------------------------------
routeWithSplice :: Handler App App ()
routeWithSplice = do
    str <- with foo getFooField
    writeText $ T.pack $ "routeWithSplice: "++str


------------------------------------------------------------------------------
routeWithConfig :: Handler App App ()
routeWithConfig = do
    cfg <- getSnapletUserConfig
    val <- liftIO $ lookup cfg "topConfigField"
    writeText $ "routeWithConfig: " `T.append` fromJust val


------------------------------------------------------------------------------
sessionDemo :: Handler App App ()
sessionDemo = withSession session $ do
  with session $ do
    curVal <- getFromSession "foo"
    case curVal of
      Nothing -> setInSession "foo" "bar"
      Just _ -> return ()
  list <- with session $ (T.pack . show) `fmap` sessionToList
  csrf <- with session $ (T.pack . show) `fmap` csrfToken
  HNC.renderWithSplices heist "session" $ do
    "session" ## textSplice list
    "csrf" ## textSplice csrf


------------------------------------------------------------------------------
sessionTest :: Handler App App ()
sessionTest = withSession session $ do
  q <- getParam "q"
  val <- case q of
    Just x -> do
      let x' = T.decodeUtf8 x
      with session $ setInSession "test" x'
      return x'
    Nothing -> fromMaybe "" `fmap` with session (getFromSession "test")
  writeText val
