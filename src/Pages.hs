{-# LANGUAGE OverloadedStrings #-}

module Pages (home, chat, blogTemplate) where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

attr :: Tag -> AttributeValue -> Attribute
attr = customAttribute

home :: Html
home = layout head $ do
  main $ do
    h1 $ code $ mconcat [b "data", " ", strong "rini", " c ", b "where"]
    p "Hey there!"

    section $ do
      h2 "intra relay cat"
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
    h2 "Latest posts"
    p $ do
      "Also available as an " <> (a ! href "/posts/atom.xml") "atom feed" <> "!"
    ul $ do
      li $ do
        a ! href "/posts/streaming-html" $ "The Cursed Art of Streaming HTML"
        br
        i "rini c. on Aug 18, 2024"

  footer $ do
    "Copyright © 2024 rini" <> br <> br
    "Made with much λ. Unless stated otherwise, all content is licensed under the "
    a ! href "https://creativecommons.org/licenses/by-sa/4.0/" $ "CC BY-SA 4.0"
    " license."
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
    input ! name "text" ! placeholder "Send a message..."
  where
    head = do
      link ! rel "stylesheet" ! href "/styles/base.css"

blogTemplate :: Html
blogTemplate = layout head $ do
  header $ do
    h1 "$title$"
    p ! class_ "subtitle" $ "$subtitle$"
    p ! class_ "meta" $ b "$author$" <> " on " <> (time ! datetime "$date-meta$") "$date$"
  "$body$"
  where
    head = do
      H.title "$title$ ∷ ~rini"
      link ! rel "stylesheet" ! href "/styles/blog.css"
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
