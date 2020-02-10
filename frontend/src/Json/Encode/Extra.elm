module Json.Encode.Extra exposing (withType)

{-|

@docs withType

-}

import Json.Encode as Encode exposing (Value)


withType : String -> List ( String, Value ) -> Value
withType type_ fields =
    Encode.object (( "type", Encode.string type_ ) :: fields)
