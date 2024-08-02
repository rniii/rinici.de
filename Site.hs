{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Pages

import Control.Monad (forM_, when)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.State (MonadState, StateT, execStateT, modify)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getModificationTime, listDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (combine, joinPath, replaceExtension, splitPath, takeDirectory)
import System.IO.Error (catchIOError)
import System.Process.Typed
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

main :: IO ()
main = compileAll $ do
  transformDir "public" $ do
    output (joinPath . drop 1 . splitPath)
  transformDir "styles" $ do
    output (`replaceExtension` "css")
    sassCompiler
  writeTo "index.html" $ do
    minifyHtml $ renderHtml home

compileAll :: Rules () -> IO ()
compileAll rules = do
  exe <- getExecutablePath -- fallback dependency
  env <- execStateT (runRules rules) []
  forM_ env $ \res -> do
    let src = fromMaybe exe (rsPath res)
        dst = "_site/" <> rsOutput res
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
  modify (resources <>)

traverseDirectory :: FilePath -> (FilePath -> IO a) -> IO [a]
traverseDirectory path f =
  listDirectory path >>= (fmap concat . mapM it)
  where
    it x = do
      let p = combine path x
      isDir <- doesDirectoryExist p
      if isDir
        then traverseDirectory p f
        else return <$> f p

output :: (FilePath -> FilePath) -> Action ()
output f =
  modify $ \r -> r{rsOutput = f $ rsOutput r}

sassCompiler :: Action ()
sassCompiler = do
  procCompiler "sass" ["-", "-scompressed"]

procCompiler :: FilePath -> [String] -> Action ()
procCompiler cmd args =
  compiler $ pipe cmd args

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
