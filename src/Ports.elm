port module Ports exposing
    ( requestSeed
    , seedReceived
    , selectAllTextIn
    )


port requestSeed : () -> Cmd msg


port seedReceived : (( Int, List Int ) -> msg) -> Sub msg


port selectAllTextIn : String -> Cmd msg
