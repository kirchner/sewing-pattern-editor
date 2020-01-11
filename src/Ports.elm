port module Ports exposing
    ( selectAllTextIn
    , changedCamera, changedMarkers, changedPose, resizeVideo, startVideo
    )

{-|

@docs selectAllTextIn

-}


{-| -}
port selectAllTextIn : String -> Cmd msg


port startVideo : { width : Float, height : Float } -> Cmd msg


port resizeVideo : { width : Float, height : Float } -> Cmd msg


port changedMarkers :
    ({ markers :
        List
            { corners :
                List
                    { x : Int
                    , y : Int
                    }
            , id : Int
            }
     }
     -> msg
    )
    -> Sub msg


port changedPose :
    ({ pose :
        { rotation : List Float
        , translation :
            { x : Float
            , y : Float
            , z : Float
            }
        }
     }
     -> msg
    )
    -> Sub msg


port changedCamera :
    ({ camera :
        { width : Int
        , height : Int
        }
     }
     -> msg
    )
    -> Sub msg
