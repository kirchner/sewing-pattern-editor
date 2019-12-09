port module LocalStorage exposing
    ( changedStore
    , updateZoom, requestZoom
    , updateCenter, requestCenter
    )

{-|

@docs changedStore
@docs updateZoom, requestZoom
@docs updateCenter, requestCenter

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Length exposing (Meters)
import Point2d exposing (Point2d)


port storeCache : { key : String, value : String } -> Cmd msg


port requestCache : { key : String } -> Cmd msg


port onStoreChange : ({ key : String, value : String } -> msg) -> Sub msg


type alias Target =
    { owner : String
    , repo : String
    , ref : String
    }



---- ZOOM


updateZoom : Target -> Float -> Cmd msg
updateZoom { owner, repo, ref } zoom =
    storeCache
        { key = key [ owner, repo, "tree", ref, "zoom" ]
        , value = Encode.encode 0 (Encode.float zoom)
        }


requestZoom : Target -> Cmd msg
requestZoom { owner, repo, ref } =
    requestCache
        { key = key [ owner, repo, "tree", ref, "zoom" ] }



---- CENTER


updateCenter : Target -> Point2d Meters coordinates -> Cmd msg
updateCenter { owner, repo, ref } center =
    storeCache
        { key = key [ owner, repo, "tree", ref, "center" ]
        , value = Encode.encode 0 (encodeCenter center)
        }


requestCenter : Target -> Cmd msg
requestCenter { owner, repo, ref } =
    requestCache
        { key = key [ owner, repo, "tree", ref, "center" ] }


encodeCenter : Point2d Meters coordinates -> Value
encodeCenter center =
    let
        { x, y } =
            Point2d.unwrap center
    in
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        ]


centerDecoder : Decoder (Point2d Meters coordinates)
centerDecoder =
    Decode.succeed (\x y -> Point2d.unsafe { x = x, y = y })
        |> Decode.required "x" Decode.float
        |> Decode.required "y" Decode.float



---- CHANGED STORE


changedStore :
    { changedZoom : Target -> Float -> msg
    , changedCenter : Target -> Point2d Meters coordinates -> msg
    , changedAnything : msg
    }
    -> Sub msg
changedStore cfg =
    onStoreChange
        (\data ->
            case String.split "/" data.key of
                owner :: repo :: "tree" :: ref :: "zoom" :: [] ->
                    case Decode.decodeString Decode.float data.value of
                        Err _ ->
                            cfg.changedAnything

                        Ok zoom ->
                            cfg.changedZoom
                                { owner = owner
                                , repo = repo
                                , ref = ref
                                }
                                zoom

                owner :: repo :: "tree" :: ref :: "center" :: [] ->
                    case Decode.decodeString centerDecoder data.value of
                        Err _ ->
                            cfg.changedAnything

                        Ok center ->
                            cfg.changedCenter
                                { owner = owner
                                , repo = repo
                                , ref = ref
                                }
                                center

                _ ->
                    cfg.changedAnything
        )


key : List String -> String
key =
    String.join "/"
