{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text.IO as I
import System.Environment (getArgs)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main) -- :sob:
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = do
  [page] <- getArgs
  case page of
    "home" -> render . home =<< I.getContents
    "blog" -> render blogTemplate
    "page" -> render pageTemplate
  where
    render = B.putStr . renderHtml

home :: Text -> Html
home posts = layout head $ do
  pageHeader $ do
    h1 "rinici.de"

  H.main $ do
    p $ do
      "My little place on the internet for rants and weird web toys. I usually go by just Rini."
    p "Have a look around!"

    h2 $ "Blog posts " <> a (img ! src "/rss.gif") ! href "/posts/atom.xml"
    H.div ! class_ "posts" $ do
      preEscapedText posts

    h2 "Intra Relay Cat"
    p "Gone!"

    h2 "Links"
    p "Find me here!"
    ul $ forM_ links $ \(url, text) ->
      li $ a text ! href url

    p "I'll likely be able to reply if you DM me on fedi or send me an email"
    pre $ code "curl https://rinici.de/pgp.asc | gpg --import"

    h2 "Stuff"
    ul $ do
      li $ a "Playable 88x31s" ! href "/buttons"
      li $ a "Color palette" ! href "/colors"

    p $ do
      forM_ buttons $ \(url, gif) -> do
        a ! href url $ img ! width "88" ! height "31" ! src gif
        " "
      forM_ ["snake", "flappy", "dvd", "sand"] $ \url -> do
        iframe mempty ! src ("/buttons/" <> url) ! width "88" ! height "31" ! customAttribute "frameborder" "0"
        " "
      blockquote "Do hotlink my button!"

      pre $ code "<a href=\"https://rinici.de\">\n  <img src=\"https://rinici.de/button.png\">\n</a>"
  where
    head = do
      H.title "rini"
      link ! rel "stylesheet" ! href "/styles/index.css"
      link ! rel "alternate" ! type_ "application/atom+xml" ! href "/posts/atom.xml" ! A.title "Blog posts"
    links =
      [ ("https://github.com/rniii", "github.com/rniii")
      , ("https://codeberg.org/rini", "codeberg.org/rini")
      , ("https://ko-fi.com/rniii", "ko-fi.com/rniii")
      , ("https://wetdry.world/@rini", "@wetdry.world@rini")
      , ("mailto:rini%40rinici.de", "rini" <> H.span "@" <> "rinici.de")
      , ("/pgp.asc", code "PGP.ASC")
      ]
    buttons =
      [ ("https://rinici.de", "/buttons/rinicide.png")
      , ("https://meow-d.github.io", "https://meow-d.github.io/assets/images/buttons/meow_d.webp")
      , ("https://calayucu.com", "https://calayucu.com/button-88x31.png")
      , ("https://mary.my.id", "/buttons/mary.webp")
      , ("https://easrng.net", "https://badges.easrng.net/easrng.gif")
      , ("https://girlboss.ceo", "/buttons/june.png")
      , ("https://amy.rip", "https://amy.rip/88x31.png")
      , ("https://tengu.space", "/buttons/tengu.gif")
      , ("https://authenyo.xyz", "/buttons/authen.gif")
      , ("https://velzie.rip", "https://velzie.rip/88x31.png")
      , ("https://sheepy.moe", "/buttons/sheepy.gif")
      , ("https://blueb.pages.gay", "/buttons/harper.gif")
      , ("https://w.on-t.work", "/buttons/wontwork.png")
      , ("https://smokepowered.com", "/buttons/smoke.gif")
      , ("steam://launch/70", "/buttons/hl.gif")
      , ("https://book.realworldhaskell.org/read/getting-started.html", "/buttons/haskell.gif")
      , ("viewsource://rinici.de/", "/buttons/somejs.gif")
      ]

blogTemplate :: Html
blogTemplate = layout head $ do
  pageHeader $ do
    h1 "$title$"
    p $ i "$subtitle$"
  H.main $ do
    p ! class_ "meta" $ do
      "Published " <> time "$date$" ! datetime "$date-meta$" <> " by "
      "$for(author)$"
      a ! href "$author.url$" $ do
        "$author.name$"
      "$sep$, "
      "$endfor$"
    "$body$"
  where
    head = do
      H.title "$title$"
      meta ! name "description" ! content "$subtitle$"
      link ! rel "stylesheet" ! href "/styles/base.css"
      link ! rel "author" ! href "$author.url$"

pageTemplate :: Html
pageTemplate = layout head $ do
  pageHeader $ do
    h1 "$title$"
    "$if(subtitle)$"
    p $ i "$subtitle$"
    "$endif$"
  H.main $ do
    "$body$"
  where
    head = do
      H.title "$title$"
      link ! rel "stylesheet" ! href "/styles/base.css"

layout :: Html -> Html -> Html
layout head body =
  docTypeHtml ! lang "en" $ do
    H.head $ do
      meta ! charset "utf8"
      meta ! name "viewport" ! content "width=device-width,initial-scale=1"
      meta ! name "theme-color" ! content "#d895ee"
      link ! rel "icon" ! href "data:image/gif;base64,R0lGODdhIAAgAHcAACH5BAkKAAAALAAAAAAgACAAgAAAAP///wJVhI+py+0Po5ww2Iuzvmvj5FlKGHQeGDIpQqqrQcYbK9cibNtHruN8vPsBfTMK6mUEDJPKFpOoeUIzktLI6fjQkCZqkLvVfsHN6BFJLgtv0rb7DY9LCwA7"
      head
    H.body $ do
      body
      pageFooter

pageHeader :: Html -> Html
pageHeader title = header $ do
  title
  nav $ do
    a ! href "/" $ "Home"

pageFooter :: Html
pageFooter = footer $ p $ do
  b "© " <> "2024 rini · 🅭 🅯 🄎 "
  a "CC-BY-SA-4.0" ! href "https://creativecommons.org/licenses/by-sa/4.0/" ! rel "license"
