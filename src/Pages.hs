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
      "My little place on the internet for rants and weird web toys. I usually go by just Rini."
    p "Have a look around!"

    h2 $ "Blog posts " <> a (img ! src "/rss.gif") ! href "/posts/atom.xml"
    H.div ! class_ "posts" $ do
      preEscapedText posts

    h2 "Intra Relay Cat"
    blockquote $ p $ do
      "No JavaScript required! Messages are proxied to "
      a ! href "https://discord.gg/z8BdAERNEP" $ "Discord"
    H.div ! class_ "frame" $ do
      iframe mempty ! src "/chat/history" ! A.id "chat"
      iframe mempty ! src "/chat"
      -- trick your browser into loading the page
      script "chat.src='',setTimeout(()=>chat.src='/chat/history',900)"

  H.div ! class_ "side" $ do
    nav $ do
      h2 "Links"
      p "Find me here!"
      ul $ forM_ links $ \(url, text) ->
        li $ a text ! href url

      p "I'll likely be able to reply if you DM me on fedi or send me an email"

    section ! A.style "flex:1" $ do
      h2 "Stuff"
      ul $ do
        li $ a "Playable 88x31s" ! href "/buttons"
        li $ a "Color palette" ! href "/colors"

      p $ do
        forM_ buttons $ \(url, gif) -> do
          a ! href url $ img ! src gif
          " "
        forM_ ["snake", "flappy", "dvd", "sand"] $ \url -> do
          iframe mempty ! src ("/buttons/" <> url) ! width "88" ! height "31"
          " "
        blockquote "Do hotlink my button!"

        pre $ code "<a href=\"https://rinici.de\">\n  <img src=\"https://rinici.de/button.png\">\n</a>"

  pageFooter
  where
    head = do
      H.title "rini"
      link ! rel "stylesheet" ! href "/styles/index.css"
      link ! rel "alternate" ! type_ "application/atom+xml" ! href "/posts/atom.xml" ! A.title "Blog posts"
    links =
      [ ("https://github.com/rniii", "github.com/rniii")
      , ("https://codeberg.org/rini", "codeberg.org/rini")
      -- , ("https://ko-fi.com/rniii", "ko-fi.com/rniii")
      , ("https://wetdry.world/@rini", "@wetdry.world@rini")
      , ("mailto:rini%40rinici.de", "rini" <> H.span "@" <> "rinici.de")
      , ("/pgp.asc", code "PGP.ASC")
      ]
    buttons =
      [ ("https://rinici.de/", "/buttons/rinicide.png")
      , ("https://exhq.dev", "https://exhq.dev/88x31.png")
      , ("https://authenyo.xyz", "/buttons/authen.gif")
      , ("https://velzie.rip", "https://velzie.rip/88x31.png")
      , ("https://sheepy.moe", "/buttons/sheepy.gif")
      , ("https://blueb.pages.gay", "/buttons/harper.gif")
      , ("https://w.on-t.work", "/buttons/wontwork.png")
      , ("https://tengu.space", "/buttons/tengu.gif")
      , ("https://smokepowered.com", "/buttons/smoke.gif")
      , ("steam://launch/70", "/buttons/hl.gif")
      , ("https://book.realworldhaskell.org/read/getting-started.html", "/buttons/haskell.gif")
      , ("viewsource://rinici.de/", "/buttons/somejs.gif")
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
      link ! rel "stylesheet" ! href "/styles/partial.css"

blogTemplate :: Html
blogTemplate = layout head $ do
  header $ do
    nav $ code $ do
      a ! href "/" $ "Home"
      " > $path$"
    H.div ! A.style "float:right" $ do
      "$for(author)$"
      a ! href "$author.url$" $ do
        img ! src "/pfps/$author.name$.gif"
      "$endfor$"
    h1 "$title$"
    p $ i "$subtitle$"
  main $ do
    p ! class_ "meta" $ do
      "Published " <> time "$date$" ! datetime "$date-meta$" <> " by "
      "$for(author)$"
      a ! href "$author.url$" $ do
        "$author.name$"
      "$sep$, "
      "$endfor$"
    "$body$"
  pageFooter
  where
    head = do
      H.title "$title$"
      meta ! name "description" ! content "$subtitle$"
      link ! rel "stylesheet" ! href "/styles/blog.css"
      link ! rel "author" ! href "$author.url$"

pageTemplate :: Html
pageTemplate = layout head $ do
  header $ do
    nav $ code $ do
      a ! href "/" $ "Home"
      " > $path$"

    h1 "$title$"
    "$if(subtitle)$"
    p $ i "$subtitle$"
    "$endif$"
  main $ do
    "$body$"
  pageFooter
  where
    head = do
      H.title "$title$"
      link ! rel "stylesheet" ! href "/styles/blog.css"

pageFooter :: Html
pageFooter = footer $ p $ do
  b "© " <> "2024 rini · 🅭 🅯 🄎 "
  a "CC-BY-SA-4.0" ! href "https://creativecommons.org/licenses/by-sa/4.0/" ! rel "license"

layout :: Html -> Html -> Html
layout head body =
  docTypeHtml ! lang "en" $ do
    H.head $ do
      meta ! charset "utf8"
      meta ! name "viewport" ! content "width=device-width,initial-scale=1"
      meta ! name "theme-color" ! content "#d895ee"
      link ! rel "icon" ! href "/ico.png"
      meta ! property "og:image" ! content "https://rinici.de/ico.png"
      meta ! property "og:site_name" ! content "rini"
      head
    H.body body
