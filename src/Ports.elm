port module Ports exposing
    ( onNewWorker
    , requestSeed
    , seedReceived
    , selectAllTextIn
    )


port requestSeed : () -> Cmd msg


port seedReceived : (( Int, List Int ) -> msg) -> Sub msg


port selectAllTextIn : String -> Cmd msg


port onNewWorker : (() -> msg) -> Sub msg
