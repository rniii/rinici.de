{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad (forM_, forever)
import Data.Binary (encode)
import Data.Binary.Builder (fromLazyByteString)
import Data.Text (Text)
import Network.Wai (StreamingBody)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H hiding (html, text)
import Text.Blaze.Html5.Attributes as A
import Web.Scotty

main :: IO ()
main = do
  chat <- newChan
  scotty 8000 $ do
    get "/" $ do
      html $ renderHtml home
    get "/chat" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      stream $ chatView chat
    post "/chat" $ do
      text' <- formParam "text"
      liftIO $ writeChan chat text'
      text ""

chatView :: Chan Text -> StreamingBody
chatView chat write flush = do
  chan <- dupChan chat
  write "<!doctype html><ul>"
  flush
  forever $ do
    text <- readChan chan
    write "<li>"
    write $ fromLazyByteString $ encode text
    flush

home :: Html
home = layout head $ do
  H.main $ do
    h1 "Welcome!"

    section $ do
      h2 "intra relay cat"
      iframe ! src "/chat" ! A.style "border:0" $ mempty
      iframe ! hidden "" ! name "null" $ mempty
      H.form ! action "/chat" ! method "post" ! target "null" $ do
        input ! name "text"

    nav $ do
      h2 "Links"
      p "Find me here!"
      ul $ forM_ links $ \(url, text) ->
        li $ a ! href url $ text
  where
    head = do
      link ! rel "icon" ! href "/ico.png"
      H.title "~rini"
    links =
      [ ("https://codeberg.org", "codeberg.org/rini")
      , ("https://github.com/rniii", "github.com/rniii")
      , ("https://ko-fi.com/rniii", "ko-fi.com/rniii")
      , ("https://wetdry.world/@rini", "@wetdry.world@rini")
      , ("mailto:rini%40rinici.de", do "rini"; H.span "@"; "rinici.de")
      ]

layout :: Html -> Html -> Html
layout head body = docTypeHtml ! lang "en" $ do
  H.head $ do
    meta ! charset "utf8"
    meta ! name "viewport" ! content "width=device-width,initial-scale=1"
    meta ! name "theme-color" ! content "#d895ee"
    head
  H.body body
