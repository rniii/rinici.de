{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, dupTChan, newBroadcastTChanIO)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Monad (forever, unless, void, when)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString.Lazy (LazyByteString)
import Data.Char (isSpace, ord)
import Data.Default (def)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as I
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, secondsToDiffTime)
import Network.HTTP.Req (POST (POST), ReqBodyJson (ReqBodyJson), defaultHttpConfig, ignoreResponse, req, runReq, useHttpsURI)
import Network.Wai (StreamingBody)
import Network.Wai.Application.Static (StaticSettings (..), defaultWebAppSettings, staticApp)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (GzipFiles (..), GzipSettings (..), gzip)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment.Blank (getEnv, getEnvDefault)
import System.Random (randomRIO)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.URI (URI, mkURI)
import WaiAppStatic.Types (Piece (fromPiece), unsafeToPiece)
import Web.ClientSession (Key, decrypt, encryptIO, getKeyEnv)
import Web.Scotty (ActionM, formParam, get, liftIO, middleware, nested, notFound, post, redirect, scotty, setHeader, stream)
import Web.Scotty.Cookie (SetCookie (..), getCookie, sameSiteStrict, setCookie)

main :: IO ()
main = do
  app <- initApp
  port <- read <$> getEnvDefault "PORT" "8000"
  webhook <- (mkURI . T.pack =<<) <$> getEnv "WEBHOOK"

  void $ forkIO $ mapM_ (proxyMessages app.chat) webhook
  void $ forkIO $ logMessages app.chat

  scotty port $ do
    middleware autohead
    middleware logStdoutDev
    middleware $ gzip def{gzipFiles = GzipPreCompressed GzipIgnore}

    get "/chat/history" $ do
      initSession app
      setHeader "Content-Type" "text/html; charset=utf-8"
      chat <- liftIO . atomically $ dupTChan app.chat
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
          | isNothing $ T.find (== '.') (fromPiece name) ->
              defaultLookup . reverse $
                unsafeToPiece (fromPiece name <> ".html") : pieces'
        _ -> defaultLookup pieces

-- TODO: we don't really have a way to reply, we'd have to somehow associate sessions with streams
chatSend :: AppState -> Session -> Text -> ActionM ()
chatSend app sess text | "/" `T.isPrefixOf` text =
  case T.break (== ' ') text of
    ("/nick", nick) ->
      when (isValidNick nick) $ do
        setSession app Session{nick = nick}
    ("/me", text) ->
      unless (T.all isSpace text) $ do
        sendMessage app $ Action sess.nick text
    (_, _) -> return ()
chatSend app sess text =
  unless (T.all isSpace text) $ do
    sendMessage app $ Message sess.nick text

sendMessage :: AppState -> (UTCTime -> Message) -> ActionM ()
sendMessage app msg = liftIO $ do
  time <- getCurrentTime
  atomically . writeTChan app.chat $ msg time

chatView :: TChan Message -> StreamingBody
chatView chat write flush = do
  time <- getCurrentTime
  write
    "<!doctype html>\
    \<meta charset=utf-8>\
    \<link rel=stylesheet href=/styles/partial.css>\
    \<body>"
  sendMessage $ Message "ServChan" "Welcome!" time
  flush
  forever $ atomically (readTChan chat) >>= sendMessage >> flush
  where
    sendMessage = write . BB.fromLazyByteString . renderMessage

logMessages :: TChan Message -> IO ()
logMessages chat = do
  chat <- atomically $ dupTChan chat
  forever $ do
    msg <- atomically $ readTChan chat
    I.putStrLn $ msg.nick <> ": " <> msg.text

proxyMessages :: TChan Message -> URI -> IO ()
proxyMessages chat webhook = do
  chat <- atomically $ dupTChan chat
  forever $ do
    msg <- atomically $ readTChan chat
    runReq defaultHttpConfig $ do
      req
        POST
        (fst $ fromJust $ useHttpsURI webhook)
        (ReqBodyJson (object ["content" .= escapeText msg.text, "username" .= msg.nick]))
        ignoreResponse
        mempty
  where
    escapeText = T.concatMap $ \case
      c | c `elem` ("#*<>_^`~" :: String) -> T.snoc "\\" c
      c -> T.singleton c

renderMessage :: Message -> LazyByteString
renderMessage msg = renderHtml $ H.article $ do
  H.time (H.text $ fmtTime msg.time) <> " "
  case msg of
    Message{} -> do
      let hue = T.foldr (\c h -> h * 33 + ord c) 524287 msg.nick `mod` 360
      H.span H.! A.style ("color:hsl(" <> H.stringValue (show hue) <> ",62%,76%)") $ do
        H.text $ msg.nick <> ":"
      " "
      H.text msg.text
    Action{} -> do
      H.em (H.text $ msg.nick <> " " <> msg.text)
  where
    fmtTime = T.pack . formatTime defaultTimeLocale "%d/%m/%y %R"

isValidNick :: Text -> Bool
isValidNick =
  T.all (`elem` " -" ++ ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_")

data Message
  = Message {nick :: Text, text :: Text, time :: UTCTime}
  | Action {nick :: Text, text :: Text, time :: UTCTime}

initSession :: AppState -> ActionM ()
initSession app = do
  hasSession <- isJust <$> getSession app
  unless hasSession $ do
    nick <- randomRIO (1000, 9999 :: Int) <&> ("anon" ++) . show
    setSession app $ Session $ T.pack nick

setSession :: AppState -> Session -> ActionM ()
setSession app sess = do
  (liftIO . encryptIO app.auth $ E.encodeUtf8 sess.nick)
    >>= cookie "chatSession" . Base64.encode
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
  getCookie "chatSession" <&> (>>= fmap toSession . decrypt')
  where
    toSession = Session . E.decodeUtf8
    decrypt' = decrypt app.auth . Base64.decodeLenient . E.encodeUtf8

newtype Session = Session {nick :: Text}

initApp :: IO AppState
initApp =
  AppState <$> getKeyEnv "SESSIONAUTH" <*> newBroadcastTChanIO

data AppState = AppState
  { auth :: Key
  , chat :: TChan Message
  }
