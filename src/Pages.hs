{-# LANGUAGE OverloadedStrings #-}

module Pages (home, chat, blogTemplate, pageTemplate) where

import Control.Monad (forM_)
import Data.Text (Text)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

home :: Text -> Html
home posts = layout head $ do
  main $ do
    h1 "Welcome!"
    p $ do
      "This is my little place for the stuff I make. I usually go by just rini!"
    ul $ do
      li "test"

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
      i $ a ! href "/buttons" $ "I want those!"

  pageFooter
  where
    head = do
      H.title "rini"
      link ! rel "stylesheet" ! href "/styles/index.css"
      link ! rel "alternate" ! type_ "application/atom+xml" ! href "/posts/atom.xml" ! A.title "Blog posts"
    links =
      [ ("https://github.com/rniii", "github.com/rniii")
      , ("https://codeberg.org", "codeberg.org/rini")
      , ("https://ko-fi.com/rniii", "ko-fi.com/rniii")
      , ("https://wetdry.world/@rini", "@wetdry.world@rini")
      , ("mailto:rini%40rinici.de", "rini" <> H.span "@" <> "rinici.de")
      ]
    buttons =
      [ ("https://authenyo.xyz", "/buttons/authen.gif")
      , ("https://sheepy.moe", "/buttons/sheepy.gif")
      , ("https://velzie.rip", "/buttons/velzie.gif")
      , ("https://w.on-t.work", "/buttons/wontwork.png")
      , ("https://tengu.space", "/buttons/tengu.gif")
      , ("https://smokepowered.com", "/buttons/smoke.gif")
      , ("https://store.steampowered.com/app/70/HalfLife/", "/buttons/hl.gif")
      , ("https://book.realworldhaskell.org/read/getting-started.html", "/buttons/haskell.gif")
      ]

chat :: Html
chat = layout head $ do
  H.form ! method "post" ! action "/chat/history" $ do
    input
      ! A.id "chatbox"
      ! name "text"
      ! placeholder "Send a message..."
      ! autocomplete "off"
      ! minlength "1"
      ! required ""
  where
    head = do
      link ! rel "stylesheet" ! href "/styles/base.css"

blogTemplate :: Html
blogTemplate = layout head $ do
  header $ do
    H.div ! A.style "float:right" $ do
      "$for(author)$"
      a ! href "$author.url$" $ do
        img ! src "/pfps/$author.name$.gif"
      "$endfor$"
    h1 "$title$"
    p ! class_ "subtitle" $ "$subtitle$"
    p $ time "$date$" ! datetime "$date-meta$"
  main $ do
    "$body$"
  pageFooter
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
