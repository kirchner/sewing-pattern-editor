port module Port exposing
    ( patternReceived
    , requestPattern
    , requestViewport
    , safePattern
    , safeViewport
    , viewportReceived
    )


import Json.Encode exposing (Value)


port safePattern : Value -> Cmd msg


port requestPattern : () -> Cmd msg


port patternReceived : (Value -> msg) -> Sub msg


port safeViewport : Value -> Cmd msg


port requestViewport : () -> Cmd msg


port viewportReceived : (Value -> msg) -> Sub msg
