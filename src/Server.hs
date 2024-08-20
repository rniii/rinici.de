{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, dupTChan, newBroadcastTChanIO)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Monad (forever, unless, void)
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString.Base64.URL as Base64
import Data.Default (def)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as I
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, secondsToDiffTime)
import Network.Wai (StreamingBody)
import Network.Wai.Application.Static (StaticSettings (..), defaultWebAppSettings, staticApp)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (GzipFiles (..), GzipSettings (..), gzip)
import System.Environment.Blank (getEnvDefault)
import System.Random (randomRIO)
import WaiAppStatic.Types (Piece (fromPiece), unsafeToPiece)
import Web.ClientSession (Key, decrypt, encryptIO, getKeyEnv)
import Web.Scotty (ActionM, formParam, get, liftIO, middleware, nested, notFound, post, redirect, scotty, setHeader, stream)
import Web.Scotty.Cookie (SetCookie (..), getCookie, sameSiteStrict, setCookie)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = do
  app <- initApp
  port <- read <$> getEnvDefault "PORT" "8000"

  void $ forkIO $ do
    chat <- atomically . dupTChan $ appChat app
    forever $ do
      msg <- atomically $ readTChan chat
      I.putStrLn $ mNick msg <> ": " <> mText msg

  scotty port $ do
    middleware autohead
    middleware logStdoutDev
    middleware $ gzip def{gzipFiles = GzipPreCompressed GzipIgnore}

    get "/chat/history" $ do
      initSession app
      setHeader "Content-Type" "text/html; charset=utf-8"
      chat <- liftIO . atomically . dupTChan $ appChat app
      stream $ chatView chat

    post "/chat/history" $ do
      session <- fromMaybe (error "No session.") <$> getSession' app
      text <- formParam "text"
      chatSend app session text
      redirect "/chat"

    notFound $ nested $ staticApp staticOpts
  where
    staticOpts = (defaultWebAppSettings "_site"){ssIndices = [unsafeToPiece "index.html"], ssLookupFile = lookup}
    defaultLookup = ssLookupFile (defaultWebAppSettings "_site")
    lookup pieces =
      case reverse pieces of
        name : pieces'
          | T.null $ T.dropWhile (/= '.') (fromPiece name) ->
              defaultLookup . reverse $
                unsafeToPiece (fromPiece name <> ".html") : pieces'
        _ -> defaultLookup pieces

chatSend :: AppState -> Session -> Text -> ActionM ()
chatSend app sess text
  | Just text <- T.stripPrefix "/" text =
      uncurry chatCommand $ T.break (== ' ') text
  where
    chatCommand cmd args
      | cmd == "nick", isValidNick args = setSession app sess{sessNick = args}
      | otherwise = return ()
chatSend app sess text = liftIO $ do
  time <- getCurrentTime
  atomically . writeTChan (appChat app) $ Message (sessNick sess) text time

chatView :: TChan Message -> StreamingBody
chatView chat write flush = do
  time <- getCurrentTime
  write
    "<!doctype html>\
    \<meta charset=utf-8>\
    \<link rel=stylesheet href=/styles/chat.css>\
    \<ul>"
  sendMessage $ Message "ServChan" "Welcome!" time
  flush
  forever $ atomically (readTChan chat) >>= sendMessage >> flush
  where
    sendMessage = write . BB.fromByteString . E.encodeUtf8 . formatMessage

formatMessage :: Message -> Text
formatMessage msg =
  mconcat ["<li><time>", ts $ mTime msg, "</time> ", mNick msg, ": ", escape $ mText msg]
  where
    escape = T.concatMap $ \case
      '&' -> "&gt;"
      '<' -> "&lt;"
      '"' -> "&quot;"
      c -> T.singleton c
    ts = T.pack . formatTime defaultTimeLocale "%d/%m/%y %R"

isValidNick :: Text -> Bool
isValidNick =
  T.all (`elem` " -" ++ ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_")

data Message = Message {mNick :: Text, mText :: Text, mTime :: UTCTime}

initSession :: AppState -> ActionM ()
initSession app = do
  hasSession <- isJust <$> getSession' app
  unless hasSession $ do
    nick <- getCookie "nick" >>= maybe defaultNick return
    setSession app $ Session nick
  where
    defaultNick = do
      discrim <- randomRIO (1000, 9999 :: Int)
      return ("anon" <> T.pack (show discrim))

setSession :: AppState -> Session -> ActionM ()
setSession app sess = do
  (liftIO . encryptIO (appAuth app) . E.encodeUtf8 $ sessNick sess)
    >>= cookie "yum" . Base64.encode
  where
    cookie name value =
      setCookie
        def
          { setCookieName = name
          , setCookieValue = value
          , setCookieSameSite = Just sameSiteStrict
          , setCookieMaxAge = Just (secondsToDiffTime 31557600)
          }

getSession' :: AppState -> ActionM (Maybe Session)
getSession' app =
  getCookie "yum" <&> (>>= fmap toSession . decrypt')
  where
    toSession = Session . E.decodeUtf8
    decrypt' = decrypt (appAuth app) . Base64.decodeLenient . E.encodeUtf8

newtype Session = Session {sessNick :: Text}

initApp :: IO AppState
initApp =
  AppState <$> getKeyEnv "SESSIONAUTH" <*> newBroadcastTChanIO

data AppState = AppState
  { appAuth :: Key
  , appChat :: TChan Message
  }
