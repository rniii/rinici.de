{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Pages

import Control.Monad (forM_, when)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.State (MonadState, StateT, execStateT, modify)
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getModificationTime, listDirectory, removeFile)
import System.Environment (getExecutablePath)
import System.FilePath (combine, joinPath, replaceExtension, splitFileName, splitPath, takeDirectory)
import System.IO (hClose)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, setStdin)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Char (isDigit)

main :: IO ()
main = compileAll $ do
  transformDir "public" $ do
    -- public/ -> /
    output $ joinPath . tail . splitPath
  transformDir "styles" $ do
    output $ extension "css"
    sassCompiler
  transformDir "posts" $ do
    -- yyyy-mm-dd-post -> post
    output $ uncurry combine . fmap removeDate . splitFileName
    output $ extension "html"
    pandocCompiler blogTemplate
  writeTo "index.html" $ do
    minifyHtml $ renderHtml home
  writeTo "chat.html" $ do
    minifyHtml $ renderHtml chat
  writeTo "posts/atom.xml" $ do
    readProcessStdout_ (proc "maid" ["-q", "generate-feed"])
  where
    removeDate = removePart . removePart . removePart
    removePart = dropWhile (== '-') . dropWhile isDigit
    extension = flip replaceExtension

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
      runCompiler (rsContents res) >>= B.writeFile dst

writeTo :: FilePath -> IO ByteString -> Rules ()
writeTo path body =
  addResources [Resource Nothing path (liftIO body)]

transformDir :: FilePath -> Action () -> Rules ()
transformDir path body =
  (>>= addResources) $ liftIO $ traverseDirectory path $ \p ->
    execStateT (runAction body) $ Resource (Just p) p (liftIO $ B.readFile p)

addResources :: [Resource] -> Rules ()
addResources resources =
  modify (mappend resources)

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

pandocCompiler :: Html -> Action ()
pandocCompiler template = compiler $ \bs -> do
  liftIO $ withSystemTempFile "tmpl.html" $ \p h -> do
    B.hPutStr h $ renderHtml template
    hClose h
    pipe "pandoc" ["-fmarkdown-auto_identifiers", "--wrap=none", "--template", p] bs
      >>= minifyHtml

sassCompiler :: Action ()
sassCompiler =
  compiler $ pipe "sass" ["-", "-scompressed"]

compiler :: (ByteString -> Compiler ByteString) -> Action ()
compiler c =
  modify $ \r -> r{rsContents = rsContents r >>= c}

minifyHtml :: ByteString -> IO ByteString
minifyHtml =
  pipe
    "minhtml"
    -- please ...
    [ "--do-not-minify-doctype"
    , "--ensure-spec-compliant-unquoted-attribute-values"
    , "--keep-spaces-between-attributes"
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

newtype Compiler a = Compiler {runCompiler :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)