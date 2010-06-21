{-# LANGUAGE OverloadedStrings #-}
module Server
    ( ServerConfig(..)
    , emptyServerConfig
    , commandLineConfig
    , server
    , quickServer
    ) where
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char
import           Control.Concurrent
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.GZip
import           System hiding (getEnv)
import           System.Posix.Env
import qualified Text.XHtmlCombinators.Escape as XH


data ServerConfig = ServerConfig
    { locale          :: String
    , interface       :: ByteString
    , port            :: Int
    , hostname        :: ByteString
    , accessLog       :: Maybe FilePath
    , errorLog        :: Maybe FilePath
    , compression     :: Bool
    , error500Handler :: SomeException -> Snap ()
    }


emptyServerConfig :: ServerConfig
emptyServerConfig = ServerConfig
    { locale          = "en_US"
    , interface       = "0.0.0.0"
    , port            = 8000
    , hostname        = "myserver"
    , accessLog       = Just "access.log"
    , errorLog        = Just "error.log"
    , compression     = True
    , error500Handler = \e -> do
        let t = T.pack $ show e
            r = setContentType "text/html; charset=utf-8" $
                setResponseStatus 500 "Internal Server Error" emptyResponse
        putResponse r
        writeBS "<html><head><title>Internal Server Error</title></head>"
        writeBS "<body><h1>Internal Server Error</h1>"
        writeBS "<p>A web handler threw an exception. Details:</p>"
        writeBS "<pre>\n"
        writeText $ XH.escape t
        writeBS "\n</pre></body></html>"
    }


commandLineConfig :: IO ServerConfig
commandLineConfig = do
    args <- getArgs
    let conf = case args of
         []        -> emptyServerConfig
         (port':_) -> emptyServerConfig { port = read port' }
    locale' <- getEnv "LANG"
    return $ case locale' of
        Nothing -> conf
        Just l  -> conf {locale = takeWhile (\c -> isAlpha c || c == '_') l}

server :: ServerConfig -> Snap () -> IO ()
server config handler = do
    putStrLn $ "Listening on " ++ (B.unpack $ interface config)
             ++ ":" ++ show (port config)
    setUTF8Locale (locale config)
    try $ httpServe
             (interface config)
             (port      config)
             (hostname  config)
             (accessLog config)
             (errorLog  config)
             (catch500 $ compress $ handler)
             :: IO (Either SomeException ())
    threadDelay 1000000
    putStrLn "Shutting down"
  where
    catch500 = (`catch` (error500Handler config))
    compress = if compression config then withCompression else id


quickServer :: Snap () -> IO ()
quickServer = (commandLineConfig >>=) . flip server


setUTF8Locale :: String -> IO ()
setUTF8Locale locale' = do
    mapM_ (\k -> setEnv k (locale' ++ ".UTF-8") True)
          [ "LANG"
          , "LC_CTYPE"
          , "LC_NUMERIC"
          , "LC_TIME"
          , "LC_COLLATE"
          , "LC_MONETARY"
          , "LC_MESSAGES"
          , "LC_PAPER"
          , "LC_NAME"
          , "LC_ADDRESS"
          , "LC_TELEPHONE"
          , "LC_MEASUREMENT"
          , "LC_IDENTIFICATION"
          , "LC_ALL" ]
