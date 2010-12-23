{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Extension
  ( -- * Introduction
    -- $introduction

    -- ** Using Snap Extensions
    -- $using

    -- *** Define Application State and Monad
    -- $definingtypes

    -- *** Provide Instances For \"HasState\" Classes
    -- $hasstateclasses

    -- *** Define The Initializer
    -- $initializer

    -- *** Simplified Snap Extension Server
    -- $httpserve

    SnapExtend
  , Initializer
  , InitializerState(..)
  , runInitializer
  , runInitializerWithReloadAction
  , getHintInternals
  , mkInitializer
  , defaultReloadHandler
  , nullReloadHandler
  ) where

import           Control.Applicative
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Prelude hiding (catch, init)
import           Snap.Extension.Loader.Devel.Evaluator
import           Snap.Iteratee (enumBS, (>==>))
import           Snap.Types
import           System.IO


{- $introduction

  Snap Extensions is a library which makes it easy to create reusable plugins
  that extend your Snap application with modular chunks of functionality such
  as session management, user authentication, templating, or database
  connection pooling.

  We achieve this by requiring that you create a datatype that holds an
  environment for your application and wrap it around the Snap monad. This new
  construct becomes your application's handler monad and gives you access to
  your application state throughout your handlers.

  Warning: this interface is still EXPERIMENTAL and has a very high likelihood
  of changing substantially in coming versions of Snap.

-}

{- $using

  Every extension has an interface and at least one implementation of that
  interface.

  For some extensions, like Heist, there is only ever going to be one
  implementation of the interface. In these cases, both the interface and the
  implementation are exported from the same module, Snap.Extension.Heist.Impl.

  Hypothetically, for something like session management though, there could be
  multiple implementations, one using a HDBC backend, one using a MongoDB
  backend and one just using an encrypted cookie as backend. In these cases,
  the interface is exported from Snap.Extension.Session, and the
  implementations live in Snap.Extension.Session.HDBC,
  Snap.Extension.Session.MongoDB and Snap.Extension.Session.CookieStore.

  Keeping this in mind, there are a number of things you need to do to use Snap
  extensions in your application. Let's walk through how to set up a simple
  application with the Heist extension turned on.

-}

{- $definingtypes

  First, we define a record type AppState for holding our application's state,
  including the state needed by the extensions we're using.

  At the same time, we also define the monad for our application, App, as
  a type alias to @SnapExtend AppState@. 'SnapExtend' is a 'MonadSnap' and
  a 'MonadReader', whose environment is a given type; in our case, AppState.

  @
module App where

import Database.HDBC
import Database.HDBC.ODBC
import Snap.Extension
import Snap.Extension.Heist
import Snap.Types

type App = SnapExtend AppState

data AppState = AppState
    { heistState  :: HeistState App }
  @

  An important thing to note is that the -State types that we use in the fields
  of AppState are specific to each implementation of a extension's interface.
  That is, Snap.Extension.Session.HDBC will export a different SessionState to
  Snap.Extension.Session.CookieStore, whose internal representation might be
  completely different.

  This state is what the extension's implementation needs to be able to do its
  job.

-}

{- $hasstateclasses

  Now we have a datatype that contains all the internal state needed by our
  application and the extensions it uses. That's a great start! But when do we
  actually get to use this interface and all the functionality that these
  extensions export?  What is actually being extended?

  We use the interface provided by an extension inside our application's monad,
  App. Snap extensions extend our App with new functionality by allowing us to
  user their exported functions inside of our handlers. For example, the Heist
  extension provides the function:

  @render :: MonadHeist m => ByteString -> m ()@ that renders a template by its
  name.

  Is App a 'MonadHeist'? Well, not quite yet. Any 'MonadReader' which is also
  a 'MonadSnap' whose environment contains a 'HeistState' is a 'MonadHeist'.
  That sounds a lot like our App, doesn't it? We just have to tell the Heist
  extension how to find the 'HeistState' in our AppState:

  @
instance HasHeistState AppState where
    getHeistState = heistState
    setHeistState hs as = as { heistState = hs }
  @

  Stated another way, if we give our AppState the ability to hold a HeistState
  and let the HasHeistState typeclass know how to get/set this state, we are
  /automagically/ given the ability to render heist templates in our handlers.

  With these instances, our application's monad App is now a MonadHeist
  giving it access to operations like:

  @render :: MonadHeist m => ByteString -> m ()@

  and

  @heistLocal :: (TemplateState n -> TemplateState n) -> m a -> m a@

-}

{- $initializer

  So, our monad is now a 'MonadHeist', but how do we actually construct our
  AppState and turn an App () into a 'Snap' ()? We need to do this upfront,
  once and right before our web server starts listening for connections.

  Snap extensions have a thing called an 'Initializer' that does these things.
  Each implementation of a Snap extension interface provides an 'Initializer'
  for its -State type. We must construct an initializer type for our -State
  type, AppState. An 'Initializer' monad is provided in this library to make
  it easy to do this. For your convenience, 'Initializer' is an instance of
  'MonadIO'.

  @
appInitializer :: Initializer AppState
appInitializer = do
    hs <- heistInitializer \"resources/templates\"
    return $ AppState hs
  @

  In addition to constructing the AppState, the Initializer monad also
  constructs the init, destroy and reload functions for our application from
  the init, reload and destroy functions for the extensions.

  Although it won't cause a compile-time error, it is important to get the
  order of the initializers correct as much as possible, otherwise they may be
  reloaded and destroyed in the wrong order. The "right" order is an order
  where every extension's dependencies are initialised before that extension.
  For example, Snap.Extension.Session.HDBC would depend on something which
  would extend the monad with MonadConnectionPool, i.e.,
  Snap.Extension.ConnectionPool. If you had this configuration it would be
  important that you put the connectionPoolInitializer before the
  sessionInitializer in your appInitializer.

  This Initializer AppState can then be passed to 'runInitializer', which
  combines our initializer action with our application's handler to produce a
  'Snap' handler (which can be passed to 'httpServe'), a cleanup action (which
  you can run after 'httpServe' finishes), and a reload action (which, for
  example, you may want to use in your handler for the path \"admin/reload\".

  The following is an example of how you might use this in main:

  @
main :: IO ()
main = do
    (snap,cleanup,reload) <- runInitializer appInitializer appSite
    let site = snap
               <|> path "admin/reload" $ defaultReloadHandler reload cleanup
    quickHttpServe site `finally` cleanup
  @

  You'll notice we're using 'defaultReloadHandler'. This is a function exported
  by "Snap.Extension" with the type signature

  @MonadSnap m => IO [(ByteString, Maybe ByteString)] -> m ()@ It takes the
  reload action returned by 'runInitializer' and returns a 'Snap' action which
  renders a simple page showing how the reload went. To avoid denial-of-service
  attacks, the reload handler only works for requests made from the local host.

-}

{- $httpserve

 This is, of course, a lot of avoidable boilerplate. Snap extensions framework
 comes with another module "Snap.Extension.Server", which provides an interface
 mimicking that of "Snap.Http.Server". Their function names clash, so if you
 need to use both of them in the same module, use a qualified import. Using
 this module, the example above becomes:

  @
import Snap.Extension.Server

main :: IO ()
main = quickHttpServe appRunner site
  @

  All it needs is a Initializer AppState and an App () and it is ready to go.
  You might be wondering what happened to all the reload handler bits we
  had before: That stuff has been absorbed into the config for the server.

  One quick note: 'quickHttpServe' doesn't take a config, instead it uses the
  defaults augmented with any options specified on the command-line.  The
  default reload handler path in this case is "admin/reload".

  If you wanted to change this to nullReloadHandler, this is what you would do:

  @
import Snap.Extension.Server

main :: IO ()
main = do
    config <- commandLineConfig emptyConfig
    httpServe (setReloadHandler nullReloadHandler config) appRunner site
  @

  This behaves exactly as the above example apart from the reload handler.

  With this, we now have a fully functional base application that makes use of
  the Snap Extensions mechanism.

  To initialize a directory with all of this setup provided as a starting
  point, simply @cd@ into the desired location and type: @snap init@. An
  example \"Timer\" extension will also be included for your convenience.

-}

------------------------------------------------------------------------------
-- | A 'SnapExtend' is a 'MonadReader' and a 'MonadSnap' whose environment is
-- the application state for a given progam. You would usually type alias
-- @SnapExtend AppState@ to something like @App@ to form the monad in which
-- you write your application.
newtype SnapExtend s a = SnapExtend (ReaderT s Snap a)
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadIO
    , MonadCatchIO
    , MonadSnap
    , MonadReader s
    )


------------------------------------------------------------------------------
-- | The 'SCR' datatype is used internally by the 'Initializer' monad to store
-- the application's state, cleanup actions and reload actions.
data SCR s = SCR
    { _state   :: s
      -- ^ The internal state of the application's Snap Extensions.
    , _cleanup :: IO ()
      -- ^ IO action which when run will cleanup the application's state,
      -- e.g., closing open connections.
    , _reload  :: IO [(ByteString, Maybe ByteString)]
      -- ^ IO action which when run will reload the application's state, e.g.,
      -- refreshing any cached values stored in the state.
      --
      -- It returns a list of tuples whose \"keys\" are the names of the Snap
      -- Extensions which were reloaded and whose \"values\" are @Nothing@
      -- when run successfully and @Just x@ on failure, where @x@ is an error
      -- message.
    }


------------------------------------------------------------------------------
-- | The 'Initializer' monad. The code that initialises your application's
-- state is written in the 'Initializer' monad. It's used for constructing
-- values which have cleanup\/destroy and reload actions associated with them.
newtype Initializer s = Initializer (Bool -> IO (Either s (SCR s)))


------------------------------------------------------------------------------
-- | Values of types which are instances of 'InitializerState' have
-- cleanup\/destroy and reload actions associated with them.
class InitializerState s where
    extensionId :: s -> ByteString
    mkCleanup   :: s -> IO ()
    mkReload    :: s -> IO ()


------------------------------------------------------------------------------
-- | Although it has the same type signature, this is not the same as 'return'
-- in the 'Initializer' monad. Return simply lifts a value into the
-- 'Initializer' monad, but this lifts the value and its destroy\/reload
-- actions. Use this when making your own 'Initializer' actions.
mkInitializer :: InitializerState s => s -> Initializer s
mkInitializer s = Initializer $ \v -> setup v $ Right $ mkSCR v
  where
    handler          :: SomeException -> IO (Maybe ByteString)
    handler e        = return $ Just $ toUTF8 $ show e
    maybeCatch m     = (m >> return Nothing) `catch` handler
    maybeToMsg       = maybe " done." $ const " failed."
    name             = fromUTF8 $ extensionId s
    mkSCR v          = SCR s (cleanup v) (reload v)
    cleanup v        = do
        when v $ hPutStr stderr $ "Cleaning up " ++ name ++ "..."
        m <- maybeCatch $ mkCleanup s
        when v $ hPutStrLn stderr $ maybeToMsg m
    reload v         = do
        when v $ hPutStr stderr $ "Reloading " ++ name ++ "..."
        m <- maybeCatch $ mkReload s
        when v $ hPutStrLn stderr $ maybeToMsg m
        return [(extensionId s, m)]
    setup v r        = do
        when v $ hPutStrLn stderr $ "Initializing " ++ name ++ "... done."
        return r


------------------------------------------------------------------------------
-- | Given the Initializer for your application's state, and a value in the
-- monad formed by 'SnapExtend' wrapped it, this returns a 'Snap' action, a
-- cleanup action and a reload action.
runInitializer
  :: Bool
    -- ^ Verbosity; info is printed to 'stderr' when this is 'True'
  -> Initializer s
    -- ^ The Initializer value
  -> SnapExtend s ()
    -- ^ A web handler in your application's monad
  -> IO (Snap (), IO (), IO [(ByteString, Maybe ByteString)])
     -- ^ Returns a 'Snap' handler, a cleanup action, and a reload action. The
     -- list returned by the reload action is for error reporting. There is one
     -- tuple in the list for each Snap extension; the first element of the
     -- tuple is the name of the Snap extension, and the second is a Maybe
     -- which contains Nothing if there was no error reloading that extension
     -- and a Just with the ByteString containing the error message if there
     -- was.
runInitializer v (Initializer r) (SnapExtend m) =
    r v >>= \e -> case e of
        Left s            -> return (runReaderT m s, return (), return [])
        Right (SCR s a b) -> return (runReaderT m s, a, b)


------------------------------------------------------------------------------
-- | Serves the same purpose as 'runInitializer', but combines the
--   application's web handler with a user-supplied action to be run to reload
--   the application's state.
runInitializerWithReloadAction
  :: Bool
    -- ^ Verbosity; info is printed to 'stderr' when this is 'True'
  -> Initializer s
    -- ^ The Initializer value
  -> SnapExtend s ()
    -- ^ A web handler in your application's monad.
  -> (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
    -- ^ Your desired \"reload\" handler; it gets passed the reload
    -- action. This handler is always run, so you have to guard the path
    -- yourself (with.
  -> IO (Snap (), IO ())
    -- ^ Your 'Snap' handler and a cleanup action.
runInitializerWithReloadAction v (Initializer r) se f = do
    (state, cleanup, reload) <- runInit

    let (SnapExtend m') = f reload <|> se
    return (runReaderT m' state, cleanup)

  where
    runInit = r v >>= \e ->
              case e of
                Left s            -> return (s, return (), return [])
                Right (SCR s a b) -> return (s, a, b)

------------------------------------------------------------------------------
-- | Translates an Initializer and SnapExtend into a HintInternals
-- object, used by the hint loading code.
getHintInternals :: Initializer s
                 -- ^ The Initializer value
                 -> SnapExtend s ()
                 -- ^ An action in your application's monad.
                 -> HintInternals
getHintInternals i se = HintInternals runInit getCleanup getAction
  where
    runInit = runInitializer True i se
    getAction (action, _, _) = action
    getCleanup (_, cleanup, _) = cleanup


------------------------------------------------------------------------------
instance Functor Initializer where
    fmap f (Initializer r) = Initializer $ \v -> r v >>= \e -> return $ case e of
        Left s            -> Left $ f s
        Right (SCR s a b) -> Right $ SCR (f s) a b


------------------------------------------------------------------------------
instance Applicative Initializer where
    pure  = return
    (<*>) = ap


------------------------------------------------------------------------------
instance Monad Initializer where
    return   = Initializer . const . return . Left
    a >>= f  = join' $ fmap f a


------------------------------------------------------------------------------
instance MonadIO Initializer where
    liftIO = Initializer . const . fmap Left


------------------------------------------------------------------------------
-- | Join for the 'Initializer' monad. This is used in the definition of bind
-- for the 'Initializer' monad.
join' :: Initializer (Initializer s) -> Initializer s
join' (Initializer r) = Initializer $ \v -> r v >>= \e -> case e of
    Left  (Initializer r')           -> r' v
    Right (SCR (Initializer r') a b) -> r' v >>= \e' -> return $ Right $ case e' of
        Left  s             -> SCR s a b
        Right (SCR s a' b') -> SCR s (a' >> a) (liftM2 (++) b b')


------------------------------------------------------------------------------
-- | This takes the last value of the tuple returned by 'runInitializer',
-- which is a list representing the results of an attempt to reload the
-- application's Snap Extensions, and turns it into a Snap action which
-- displays the these results.
defaultReloadHandler :: MonadSnap m
                     => IO [(ByteString, Maybe ByteString)]
                     -> m ()
defaultReloadHandler ioms = failIfNotLocal $ do
    ms <- liftIO $ ioms
    let showE e       = mappend "Error: "  $ toUTF8 $ show e
        format (n, m) = mconcat [n, ": ", maybe "Sucess" showE m, "\n"]
        msg           = mconcat $ map format ms
    finishWith $ setContentType "text/plain; charset=utf-8"
        $ setContentLength (fromIntegral $ B.length msg)
        $ modifyResponseBody (>==> enumBS msg) emptyResponse
  where
    failIfNotLocal m = do
        rip <- liftM rqRemoteAddr getRequest
        if not $ elem rip [ "127.0.0.1"
                          , "localhost"
                          , "::1" ]
          then pass
          else m

------------------------------------------------------------------------------
-- | Use this reload handler to disable the ability to have a web handler
-- which reloads Snap extensions.
nullReloadHandler :: MonadSnap m
                  => IO [(ByteString, Maybe ByteString)]
                  -> m ()
nullReloadHandler = const pass


------------------------------------------------------------------------------
fromUTF8 :: ByteString -> String
fromUTF8 = T.unpack . T.decodeUtf8

toUTF8 :: String -> ByteString
toUTF8 = T.encodeUtf8 . T.pack
