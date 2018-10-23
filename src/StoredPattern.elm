module StoredPattern exposing
    ( Position
    , StoredPattern
    , decoder
    , encode
    , init
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Pattern exposing (Pattern)


type alias StoredPattern =
    { slug : String
    , name : String
    , pattern : Pattern
    , zoom : Float
    , center : Position
    }


type alias Position =
    { x : Float
    , y : Float
    }


init slug name =
    { slug = slug
    , name = name
    , pattern =
        Pattern.empty
            |> Pattern.insertPoint
                (Just "origin")
                (Pattern.origin { x = 0, y = 0 })
    , zoom = 1
    , center = Position 0 0
    }


decoder : Decoder StoredPattern
decoder =
    Decode.succeed StoredPattern
        |> Decode.required "slug" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "pattern" Pattern.decoder
        |> Decode.required "zoom" Decode.float
        |> Decode.required "center" positionDecoder


positionDecoder : Decoder Position
positionDecoder =
    Decode.succeed Position
        |> Decode.required "x" Decode.float
        |> Decode.required "y" Decode.float


encode : StoredPattern -> Value
encode { slug, name, pattern, zoom, center } =
    Encode.object
        [ ( "slug", Encode.string slug )
        , ( "name", Encode.string name )
        , ( "pattern", Pattern.encode pattern )
        , ( "zoom", Encode.float zoom )
        , ( "center", encodePosition center )
        ]


encodePosition : Position -> Value
encodePosition { x, y } =
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        ]
