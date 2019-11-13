port module Ports exposing
    ( requestSeed, seedReceived
    , selectAllTextIn
    , onNewWorker
    )

{-|

@docs requestSeed, seedReceived
@docs selectAllTextIn
@docs onNewWorker

-}


{-| -}
port requestSeed : () -> Cmd msg


{-| -}
port seedReceived : (( Int, List Int ) -> msg) -> Sub msg


{-| -}
port selectAllTextIn : String -> Cmd msg


{-| -}
port onNewWorker : (() -> msg) -> Sub msg
