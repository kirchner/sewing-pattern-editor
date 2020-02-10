{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (asum)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Char8 (unpack)
import Data.HashMap.Strict (insert)
import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import Control.Monad.IO.Class (liftIO)
import Snap.Util.FileServe (DirectoryConfig, simpleDirectoryConfig, defaultMimeTypes, mimeTypes, serveFile, serveDirectoryWith)
import System.Console.CmdArgs

import Data.Aeson

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http



---- FLAGS


data Flags =
  Flags
    { port :: Int
    , clientId :: String
    , clientSecret :: String
    , environment :: String
    }
    deriving (Data, Typeable, Show, Eq)


flags :: Flags
flags =
  Flags
    { port = 4321
        &= help "port of the server"
    , clientId = ""
        &= help "client id of the github oauth app"
    , clientSecret = ""
        &= help "client secret of the github oauth app"
    , environment = "development"
        &= help "either development or production"
    }



---- MAIN


main :: IO ()
main =
  do  Flags port clientId clientSecret environment <- cmdArgs flags

      S.httpServe (config port) (serve clientId clientSecret environment)


config :: Int -> S.Config S.Snap a
config port =
  S.setPort port S.defaultConfig



---- SERVE


serve :: String -> String -> String -> S.Snap ()
serve clientId clientSecret environment =
  let
    assetsDir =
      case environment of
        "development" -> "_debug"
        "production"  -> "_build"
        _             -> "_debug"
  in
  asum
    [ S.route
        [ ( "/client_id", S.writeLBS $ toLazyByteString $ stringUtf8 clientId )
        , ( "/static", serveDirectoryWith directoryConfig assetsDir )
        , ( "/service-worker.js", serveFile (assetsDir ++ "/service-worker.js") )
        ]
    , serveFile (assetsDir ++ "/app.html")
    ]


directoryConfig =
  simpleDirectoryConfig
    { mimeTypes = insert ".js" "application/javascript" defaultMimeTypes }
