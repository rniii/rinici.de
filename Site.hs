{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, getModificationTime, listDirectory, copyFile)
import System.IO.Error (catchIOError)
import System.Process
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 (Html)

import Pages

main :: IO ()
main = do
  createDirectoryIfMissing True "_site"

  writeHtml "_site/index.html" home
  mapDir "styles" $ \style -> do
    compileSass ("styles/" ++ style) ("_site/assets/" ++ takeWhile (/= '.') style ++ ".css")
  mapDir "public/assets" $ \asset -> do
    copyFile ("public/assets/" ++ asset) ("_site/assets/" ++ asset)

writeHtml :: FilePath -> Html -> IO ()
writeHtml path html =
  readProcess "minhtml" minHtmlOpts (renderHtml html)
    >>= writeFile path
  where
    -- please....
    minHtmlOpts =
      [ "--do-not-minify-doctype"
      , "--ensure-spec-compliant-unquoted-attribute-values"
      , "--keep-spaces-between-attributes"
      ]

mapDir :: FilePath -> (FilePath -> IO ()) -> IO ()
mapDir dir f =
  listDirectory dir >>= mapM_ f

compileSass :: FilePath -> FilePath -> IO ()
compileSass input output = do
  updateOuput input output $ do
    callProcess "sass" [input, output, "-scompressed", "--no-source-map"]

updateOuput :: FilePath -> FilePath -> IO () -> IO ()
updateOuput input output action = do
  iMTime <- getModificationTime input
  oMTime <- getModificationTime output `catchIOError` const (return iMTime)
  when (iMTime >= oMTime) $ do
    putStrLn ("Compiling " ++ input ++ " to " ++ output)
    action
