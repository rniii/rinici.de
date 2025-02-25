{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.Wai.Application.Static (StaticSettings (..), defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (GzipFiles (GzipIgnore, GzipPreCompressed), GzipSettings (gzipFiles), def, gzip)
import WaiAppStatic.Types (Piece (fromPiece), unsafeToPiece)

main :: IO ()
main = do
  runEnv 8000 $ middleware $ staticApp staticOpts
  where
    middleware = autohead . gzip def{gzipFiles = GzipPreCompressed GzipIgnore}

    staticOpts = (defaultWebAppSettings "_site"){ssIndices = [unsafeToPiece "index.html"], ssLookupFile = lookup}
    defaultLookup = ssLookupFile (defaultWebAppSettings "_site")
    lookup pieces =
      case reverse pieces of
        name : pieces'
          | isNothing $ T.find (== '.') (fromPiece name) ->
              defaultLookup . reverse $
                unsafeToPiece (fromPiece name <> ".html") : pieces'
        _ -> defaultLookup pieces
