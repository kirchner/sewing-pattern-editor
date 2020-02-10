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
    , assets :: String
    , frontend :: String
    , debug :: Bool
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
    , assets = "_build"
        &= help "path to static assets"
    , frontend = "elm.js"
        &= help "path to frontend asset"
    , debug = False
        &= help "debug mode"
    }



---- MAIN


main :: IO ()
main =
  do  Flags port clientId clientSecret assets frontend debug <- cmdArgs flags

      S.httpServe (config port) (serve clientId clientSecret assets frontend debug)


config :: Int -> S.Config S.Snap a
config port =
  S.setPort port S.defaultConfig



---- SERVE


serve :: String -> String -> String -> String -> Bool -> S.Snap ()
serve clientId clientSecret assets frontend debug =
  do  asum
        [ S.route
            [ ( "/client_id", S.writeLBS $ toLazyByteString $ stringUtf8 clientId )
            , ( "/static/elm.js", serveFile frontend )
            , ( "/static", serveDirectoryWith directoryConfig assets )
            , ( "/service-worker.js", serveFile (assets ++ "/service-worker.js") )
            ]
        , serveFile (assets ++ "/app.html")
        ]

      setCacheControl debug


setCacheControl :: Bool -> S.Snap ()
setCacheControl debug =
  if debug then
    S.modifyResponse (S.setHeader "Cache-Control" "private")

  else
    return ()


directoryConfig =
  simpleDirectoryConfig
    { mimeTypes = insert ".js" "application/javascript" defaultMimeTypes }
