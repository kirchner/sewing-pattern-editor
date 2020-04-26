module Viewer exposing (Viewer, decoder)

import Json.Decode as Decode exposing (Decoder)


type Viewer
    = Viewer


decoder : Decoder Viewer
decoder =
    Decode.succeed Viewer
