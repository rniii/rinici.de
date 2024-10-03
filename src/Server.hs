{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, dupTChan, newBroadcastTChanIO)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Monad (forever, unless, void, when)
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString.Lazy (LazyByteString)
import Data.Char (isSpace)
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
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment.Blank (getEnvDefault)
import System.Random (randomRIO)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import WaiAppStatic.Types (Piece (fromPiece), unsafeToPiece)
import Web.ClientSession (Key, decrypt, encryptIO, getKeyEnv)
import Web.Scotty (ActionM, formParam, get, liftIO, middleware, nested, notFound, post, redirect, scotty, setHeader, stream)
import Web.Scotty.Cookie (SetCookie (..), getCookie, sameSiteStrict, setCookie)

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
      session <- fromMaybe (error "No session.") <$> getSession app
      text <- formParam "text"
      chatSend app session text
      redirect "/chat#chatbox"

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

-- TODO: we don't really have a way to reply, we'd have to somehow associate sessions with streams
chatSend :: AppState -> Session -> Text -> ActionM ()
chatSend app sess text | "/" `T.isPrefixOf` text =
  case T.break (== ' ') text of
    ("/nick", nick) ->
      when (isValidNick nick) $ do
        setSession app sess{sessNick = nick}
    ("/me", text) ->
      unless (T.all isSpace text) $ do
        sendMessage app $ Action (sessNick sess) text
    (_, _) -> return ()
chatSend app sess text =
  unless (T.all isSpace text) $ do
    sendMessage app $ Message (sessNick sess) text

sendMessage :: AppState -> (UTCTime -> Message) -> ActionM ()
sendMessage app msg = liftIO $ do
  time <- getCurrentTime
  atomically . writeTChan (appChat app) $ msg time

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
    sendMessage = write . BB.fromLazyByteString . renderMessage

renderMessage :: Message -> LazyByteString
renderMessage msg = renderHtml $ H.li $ do
  H.time (H.text $ fmtTime $ mTime msg) <> " "
  case msg of
    Message{} -> do
      H.span (H.text $ mNick msg <> ":") H.! A.style "color:pink" <> " "
      H.text (mText msg)
    Action{} -> do
      H.em (H.text $ mNick msg <> " " <> mText msg)
  where
    fmtTime = T.pack . formatTime defaultTimeLocale "%d/%m/%y %R"

isValidNick :: Text -> Bool
isValidNick =
  T.all (`elem` " -" ++ ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_")

data Message
  = Message {mNick :: Text, mText :: Text, mTime :: UTCTime}
  | Action {mNick :: Text, mText :: Text, mTime :: UTCTime}

initSession :: AppState -> ActionM ()
initSession app = do
  hasSession <- isJust <$> getSession app
  unless hasSession $ do
    nick <- randomRIO (1000, 9999 :: Int) <&> ("anon" ++) . show
    setSession app $ Session $ T.pack nick

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

getSession :: AppState -> ActionM (Maybe Session)
getSession app =
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
