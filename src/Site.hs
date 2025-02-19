{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, execStateT, modify)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as E
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getModificationTime, listDirectory, removeFile)
import System.Environment (getExecutablePath)
import System.FilePath (combine, dropExtension, joinPath, replaceExtension, splitFileName, splitPath, takeDirectory)
import System.IO (hClose)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, setStdin)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

main :: IO ()
main = compileAll $ do
  transformDir "public" $ do
    -- public/ -> /
    output $ joinPath . tail . splitPath
  transformDir "styles" $ do
    compiler esbuildCss
  transformDir "client" $ do
    output $ extension "js"
    compiler esbuildJs
  transformDir "posts" $ do
    -- yyyy-mm-dd-post -> post
    output $ uncurry combine . fmap removeDate . splitFileName
    output indexHtml
    blogTemplate <- readProcessStdout_ "runhaskell src/Pages.hs blog"
    compiler $ pandoc blogTemplate
  transformDir "pages" $ do
    output $ joinPath . tail . splitPath
    output indexHtml
    pageTemplate <- readProcessStdout_ "runhaskell src/Pages.hs page"
    compiler $ pandoc pageTemplate
  writeTo' "index.html" "src/Pages.hs" $ do
    posts <- readProcessStdout_ "maid -q generate-list"
    home <- readProcessStdout_ (setStdin (byteStringInput posts) "runhaskell src/Pages.hs home")
    minifyHtml home
  writeTo "posts/atom.xml" $ do
    readProcessStdout_ "maid -q generate-feed"
  where
    removeDate = removePart . removePart . removePart
    removePart = dropWhile (== '-') . dropWhile isDigit
    extension = flip replaceExtension
    indexHtml = (`combine` "index.html") . dropExtension

compileAll :: Rules () -> IO ()
compileAll rules = do
  createDirectoryIfMissing False "_site"
  exe <- getExecutablePath -- fallback dependency
  env <- execStateT (runRules rules) []
  paths <- traverseDirectory "_site" return
  forM_ (paths \\ map (combine "_site" . rsOutput) env) $ \p -> do
    putStrLn ("Removing " ++ p)
    removeFile p

  forM_ env $ \res -> do
    let src = fromMaybe exe (rsPath res)
        dst = combine "_site/" $ rsOutput res
    srcMTime <- getModificationTime src
    dstMTime <- getModificationTime dst `catchIOError` const (return srcMTime)
    when (srcMTime >= dstMTime) $ do
      putStrLn ("Writing " ++ dst)
      createDirectoryIfMissing True (takeDirectory dst)
      runReaderT (runCompiler (rsContents res)) res
        >>= B.writeFile dst

writeTo :: FilePath -> IO ByteString -> Rules ()
writeTo path body =
  addResources [Resource Nothing path (liftIO body)]

writeTo' :: FilePath -> FilePath -> IO ByteString -> Rules ()
writeTo' path dep body =
  addResources [Resource (Just dep) path (liftIO body)]

transformDir :: FilePath -> Action () -> Rules ()
transformDir path body =
  (>>= addResources) $ liftIO $ traverseDirectory path $ \p ->
    execStateT (runAction body) $ Resource (Just p) p (liftIO $ B.readFile p)

addResources :: [Resource] -> Rules ()
addResources =
  modify . mappend

traverseDirectory :: FilePath -> (FilePath -> IO a) -> IO [a]
traverseDirectory path f =
  listDirectory path >>= fmap concat . mapM it
  where
    it = (. combine path) $ \p ->
      doesDirectoryExist p
        >>= bool (return <$> f p) (traverseDirectory p f)

output :: (FilePath -> FilePath) -> Action ()
output f =
  modify $ \r -> r{rsOutput = f $ rsOutput r}

pandoc :: ByteString -> ByteString -> Compiler ByteString
pandoc template bs = do
  liftIO $ withSystemTempFile "tmpl.html" $ \p h -> do
    B.hPutStr h template
    hClose h
    pipe "pandoc" ["-fmarkdown-auto_identifiers-smart", "--wrap=none", "--template", p] bs
      >>= minifyHtml

esbuildJs :: ByteString -> Compiler ByteString
esbuildJs =
  pipe "esbuild" ["--minify", "--loader=ts", "--bundle"]

esbuildCss :: ByteString -> Compiler ByteString
esbuildCss =
  pipe "esbuild" ["--minify", "--loader=css", "--supported:nesting=false"]

compiler :: (ByteString -> Compiler ByteString) -> Action ()
compiler c =
  modify $ \r -> r{rsContents = rsContents r >>= c}

minifyHtml :: ByteString -> IO ByteString
minifyHtml =
  pipe
    "minify-html"
    -- please ...
    [ "--do-not-minify-doctype"
    , "--ensure-spec-compliant-unquoted-attribute-values"
    , "--keep-spaces-between-attributes"
    , "--keep-closing-tags"
    ]

pipe :: MonadIO m => FilePath -> [String] -> ByteString -> m ByteString
pipe cmd args =
  readProcessStdout_ . (`setStdin` proc cmd args) . byteStringInput

newtype Rules a = Rules {runRules :: StateT [Resource] IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState [Resource])

newtype Action a = Action {runAction :: StateT Resource IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Resource)

data Resource = Resource
  { rsPath :: Maybe FilePath
  , rsOutput :: FilePath
  , rsContents :: Compiler ByteString
  }

newtype Compiler a = Compiler {runCompiler :: ReaderT Resource IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Resource)
