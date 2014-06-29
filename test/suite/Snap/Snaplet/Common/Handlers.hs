module Snap.Snaplet.Common.Handlers where

------------------------------------------------------------------------------
import Control.Monad.IO.Class                        (liftIO)
import Data.Configurator                             (lookup)
import Data.Maybe                                    (fromJust, fromMaybe)
import Data.Text                                     (append, pack)
import Data.Text.Encoding                            (decodeUtf8)
------------------------------------------------------------------------------
import Data.Map.Syntax                               ((##))
import Heist.Interpreted                             (textSplice)
import Snap.Core                                     (writeText, getParam)
--import Snap.Http.Server.Config                       (Config, completeConfig,
--                                                      defaultConfig)
import Snap.Snaplet                                  (Handler, 
                                                      getSnapletUserConfig,
                                                      nestSnaplet,
                                                      with)
--import Snap.Snaplet.Auth                             (AuthManager,
--                                                      defAuthSettings)
--import Snap.Snaplet.Auth.Backends.JsonFile           (initJsonFileAuthManager)
import Snap.Snaplet.Common.FooSnaplet
--import Snap.Snaplet.Common.BarSnaplet
import Snap.Snaplet.Common.Types
--import Snap.Snaplet.Config                           (AppConfig,
--                                                      commandLineAppConfig)
import Snap.Snaplet.Heist                            (Heist, HasHeist,
                                                      heistInit, heistLens)
import Snap.Snaplet.HeistNoClass                     (setInterpreted,
                                                      renderWithSplices)
import Snap.Snaplet.Session                          (SessionManager,
                                                      csrfToken,
                                                      getFromSession,
                                                      sessionToList,
                                                      setInSession,
                                                      withSession)


-------------------------------------------------------------------------------
routeWithSplice :: Handler App App ()
routeWithSplice = do
    str <- with foo getFooField
    writeText $ pack $ "routeWithSplice: "++str


------------------------------------------------------------------------------
routeWithConfig :: Handler App App ()
routeWithConfig = do
    cfg <- getSnapletUserConfig
    val <- liftIO $ Data.Configurator.lookup cfg "topConfigField"
    writeText $ "routeWithConfig: " `append` fromJust val


------------------------------------------------------------------------------
sessionDemo :: Handler App App ()
sessionDemo = withSession session $ do
  with session $ do
    curVal <- getFromSession "foo"
    case curVal of
      Nothing -> setInSession "foo" "bar"
      Just _ -> return ()
  list <- with session $ (pack . show) `fmap` sessionToList
  csrf <- with session $ (pack . show) `fmap` csrfToken
  renderWithSplices heist "session" $ do
    "session" ## textSplice list
    "csrf" ## textSplice csrf


------------------------------------------------------------------------------
sessionTest :: Handler App App ()
sessionTest = withSession session $ do
  q <- getParam "q"
  val <- case q of
    Just x -> do
      let x' = decodeUtf8 x
      with session $ setInSession "test" x'
      return x'
    Nothing -> fromMaybe "" `fmap` with session (getFromSession "test")
  writeText val
