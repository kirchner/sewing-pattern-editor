port module Ports exposing
    ( requestSeed
    , seedReceived
    )


port requestSeed : () -> Cmd msg


port seedReceived : (( Int, List Int ) -> msg) -> Sub msg
