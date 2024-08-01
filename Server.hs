{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Binary (encode)
import Data.Binary.Builder (fromLazyByteString)
import Data.Default (def)
import Data.Text (Text)
import Network.Wai (StreamingBody)
import Network.Wai.Application.Static (StaticSettings (ssIndices), defaultWebAppSettings, staticApp)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (GzipFiles (GzipIgnore, GzipPreCompressed), GzipSettings (gzipFiles), gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import WaiAppStatic.Types (unsafeToPiece)
import Web.Scotty

main :: IO ()
main = do
  chat <- newChan
  scotty 8000 $ do
    middleware logStdout
    middleware autohead
    middleware $ gzip def{gzipFiles = GzipPreCompressed GzipIgnore}

    get "/chat" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      stream $ chatView chat
    post "/chat" $ do
      text <- formParam "text"
      liftIO $ writeChan chat text
      redirect "/"
    notFound $ do
      nested $ staticApp staticOpts
  where
    staticOpts = (defaultWebAppSettings "_site"){ssIndices = [unsafeToPiece "index.html"]}

chatView :: Chan Text -> StreamingBody
chatView chat write flush = do
  chat <- dupChan chat
  write "<!doctype html><link rel=stylesheet href=/assets/chat.css><ul>"
  write $ renderMessage ":3"
  flush
  forever $ do
    text <- readChan chat
    write $ renderMessage text
    flush
  where
    renderMessage text =
      fromLazyByteString $ encode $ renderHtml $ do
        H.li $ H.text text
