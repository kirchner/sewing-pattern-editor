port module Port exposing
    ( onCacheChange
    , safeCache
    )

import Json.Encode exposing (Value)


port safeCache : Value -> Cmd msg


port onCacheChange : (Value -> msg) -> Sub msg
