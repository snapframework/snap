{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blackbox.App where


------------------------------------------------------------------------------
import Prelude hiding (lookup)

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Configurator
import Snap.Core
import Snap.Util.FileServe

------------------------------------------------------------------------------
import Data.Map.Syntax ((##))
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Snap.Snaplet.HeistNoClass as HNC
import Heist
import Heist.Interpreted

------------------------------------------------------------------------------
import Blackbox.Common
import Blackbox.BarSnaplet
import Blackbox.FooSnaplet
import Blackbox.EmbeddedSnaplet
import Blackbox.Types
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession


                            --------------------
                            --  THE SNAPLET   --
                            --------------------
  
------------------------------------------------------------------------------
app :: SnapletInit App App
app = makeSnaplet "app" "Test application" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    fs <- nestSnaplet "foo" foo $ fooInit hs
    bs <- nestSnaplet "" bar $ nameSnaplet "baz" $ barInit hs foo
    sm <- nestSnaplet "session" session $
          initCookieSessionManager "sitekey.txt" "_session" (Just (30 * 60))
    ns <- embedSnaplet "embed" embedded embeddedInit
    _lens <- getLens
    addConfig hs $ mempty
        { hcInterpretedSplices = do
            "appsplice" ## textSplice "contents of the app splice"
            "appconfig" ## shConfigSplice _lens }
    addRoutes [ ("/hello", writeText "hello world")
              , ("/routeWithSplice", routeWithSplice)
              , ("/routeWithConfig", routeWithConfig)
              , ("/public", serveDirectory "public")
              , ("/sessionDemo", sessionDemo)
              , ("/sessionTest", sessionTest)
              ]
    wrapSite (<|> heistServe)
    return $ App hs (over snapletValue fooMod fs) bs sm ns


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

fooMod :: FooSnaplet -> FooSnaplet
fooMod f = f { fooField = fooField f ++ "z" }

