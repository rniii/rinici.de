{-# LANGUAGE OverloadedStrings #-}

module Pages (home, chat, blogTemplate) where

import Control.Monad (forM_)
import Data.Text (Text)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

attr :: Tag -> AttributeValue -> Attribute
attr = customAttribute

home :: Text -> Html
home posts = layout head $ do
  main $ do
    h1 $ code $ b "data " <> strong "rini " <> "c " <> b "where"
    p "nya"

    section $ do
      h2 "Intra Relay Cat"
      p $ do
        "Sent directly to Discord, no JavaScript needed! "
        "I apologise if your page is stuck loading forever, though. "
        a ! href "/blog/streaming-html" $ "Because it is."
      iframe ! src "/chat/history" ! attr "frameborder" "0" ! A.id "chat" $ mempty
      iframe ! src "/chat" ! attr "frameborder" "0" $ mempty
      -- trick your browser into loading the page
      script "chat.src='',setTimeout(()=>chat.src='/chat/history',500)"

  nav $ do
    h2 $ mconcat [b "∀", "c", i ". ", strong "rini", " c ", i "→ ", strong "link"]
    p "Find me here!"
    ul $ forM_ links $ \(url, text) ->
      li $ a ! href url $ text
    h2 "Blog posts"
    p $ do
      "Also available as an " <> (a ! href "/posts/atom.xml") "atom feed!"
    ul $ preEscapedText posts

  footer $ do
    p "Copyright © 2024 rini"
    p $ do
      "Unless stated otherwise, all content is licensed under the "
      a ! href "https://creativecommons.org/licenses/by-sa/4.0/" $ "CC BY-SA 4.0"
      " license."
    p "With love to Anika 💜"
  where
    head = do
      H.title "home ∷ ~rini"
      link ! rel "stylesheet" ! href "/styles/index.css"
      link ! rel "alternate" ! type_ "application/atom+xml" ! href "/posts/atom.xml" ! A.title "Blog posts"
      meta ! property "og:title" ! content "~rini"
      meta ! property "og:description" ! content "Lambda Cube Shrine"
    links =
      [ ("https://codeberg.org", "codeberg.org/rini")
      , ("https://github.com/rniii", "github.com/rniii")
      , ("https://ko-fi.com/rniii", "ko-fi.com/rniii")
      , ("https://wetdry.world/@rini", "@wetdry.world@rini")
      , ("mailto:rini%40rinici.de", "rini" <> H.span "@" <> "rinici.de")
      ]

chat :: Html
chat = layout head $ do
  H.form ! method "post" ! action "/chat/history" $ do
    input ! name "text" ! autofocus "" ! placeholder "Send a message..."
  where
    head = do
      link ! rel "stylesheet" ! href "/styles/base.css"

blogTemplate :: Html
blogTemplate = layout head $ do
  header $ do
    h1 "$title$"
    p ! class_ "meta" $ do
      a ! href "$author.url$" $ "$author.name$"
      " on "
      time ! datetime "$date-meta$" $ "$date$"
    p ! class_ "subtitle" $ "$subtitle$"
  "$body$"
  where
    head = do
      H.title "$title$ ∷ ~rini"
      link ! rel "stylesheet" ! href "/styles/blog.css"
      link ! rel "author" ! href "$author.url$"
      meta ! property "og:title" ! content "$title$"

layout :: Html -> Html -> Html
layout head body =
  docTypeHtml ! lang "en" $ do
    H.head $ do
      meta ! charset "utf8"
      meta ! name "viewport" ! content "width=device-width,initial-scale=1"
      meta ! name "theme-color" ! content "#d895ee"
      link ! rel "icon" ! href "/ico.png"
      meta ! property "og:image" ! content "https://rinici.de/ico.png"
      meta ! property "og:site_name" ! content "rini c."
      head
    H.body body
