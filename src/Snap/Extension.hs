{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Extension
  ( SnapExtend
  , Initializer
  , InitializerState(..)
  , runInitializer
  , runInitializerHint
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
import           Prelude hiding (catch)
import           Snap.Iteratee (enumBS, (>==>))
import           Snap.Types
import           System.IO


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
runInitializer :: Bool
               -- ^ Verbosity; info is printed to 'stderr' when this is 'True'
               -> Initializer s
               -- ^ The Initializer value
               -> SnapExtend s ()
               -- ^ An action in your application's monad
               -> IO (Snap (), IO (), IO [(ByteString, Maybe ByteString)])
               -- ^ This is documented thoroughly in the README
runInitializer v (Initializer r) (SnapExtend m) = r v >>= \e -> case e of
    Left s            -> return (runReaderT m s, return (), return [])
    Right (SCR s a b) -> return (runReaderT m s, a, b)


------------------------------------------------------------------------------
-- | Serves the same purpose as 'runInitializer', but can be used with Hint.
-- This is explained in the README.
runInitializerHint :: Bool
                   -- ^ Verbosity; info is printed to 'stderr' when this is
                   -- 'True'
                   -> Initializer s
                   -- ^ The Initializer value
                   -> SnapExtend s ()
                   -- ^ An action in your application's monad.
                   -> (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
                   -- ^ See README and 'defaultReloadHandler'
                   -> IO (IO s, s -> IO (), s -> Snap ())
                   -- ^ A tuple of values which can be passed to @loadSnapTH@.
runInitializerHint v (Initializer r) se@(SnapExtend m) f = r v >>= \e -> case e of
    Left s            -> return (return s, const $ return (), runReaderT m)
    Right (SCR s a b) -> let (SnapExtend m') = f b <|> se
                         in return (return s, const a, runReaderT m')


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
defaultReloadHandler ioms = do
    ms <- liftIO $ ioms
    let showE e       = mappend "Error: "  $ toUTF8 $ show e
        format (n, m) = mconcat [n, ": ", maybe "Sucess" showE m, "\n"]
        msg           = mconcat $ map format ms
    finishWith $ setContentType "text/plain; charset=utf-8"
        $ setContentLength (fromIntegral $ B.length msg)
        $ modifyResponseBody (>==> enumBS msg) emptyResponse


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
