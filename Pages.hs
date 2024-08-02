{-# LANGUAGE OverloadedStrings #-}

module Pages (home) where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

attr :: Tag -> AttributeValue -> Attribute
attr = customAttribute

home :: Html
home = layout head $ do
  main $ do
    h1 "Welcome!"

    section $ do
      h2 "intra relay cat"

      iframe ! src "/chat" ! attr "frameborder" "0" ! A.id "chat" $ mempty
      iframe ! hidden "" ! name "null" $ mempty
      -- trick your browser into loading the page
      script "chat.src='',setTimeout(()=>chat.src='/chat',500)"

      H.form ! action "/chat" ! method "post" ! target "null" $ do
        "chat> "
        input ! name "text" ! placeholder "Send a message..."

  nav $ do
    h2 "Links"
    p "Find me here!"
    ul $ forM_ links $ \(url, text) ->
      li $ a ! href url $ text

  footer $ do
    "☔ Copyright (c) rini 2024" <> br <> br
    "Powered by black magic, really."
  where
    head = do
      H.title "~rini"
      link ! rel "icon" ! href "/ico.png"
      link ! rel "stylesheet" ! href "/assets/index.css"
    links =
      [ ("https://codeberg.org", "codeberg.org/rini")
      , ("https://github.com/rniii", "github.com/rniii")
      , ("https://ko-fi.com/rniii", "ko-fi.com/rniii")
      , ("https://wetdry.world/@rini", "@wetdry.world@rini")
      , ("mailto:rini%40rinici.de", "rini" <> H.span "@" <> "rinici.de")
      ]

layout :: Html -> Html -> Html
layout head body =
  docTypeHtml ! lang "en" $ do
    H.head $ do
      meta ! charset "utf8"
      meta ! name "viewport" ! content "width=device-width,initial-scale=1"
      meta ! name "theme-color" ! content "#d895ee"
      head
    H.body body
