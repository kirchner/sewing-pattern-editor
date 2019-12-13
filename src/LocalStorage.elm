port module LocalStorage exposing
    ( Address
    , changedStore
    , updateZoom, requestZoom
    , updateCenter, requestCenter
    )

{-|

@docs Address
@docs changedStore
@docs updateZoom, requestZoom
@docs updateCenter, requestCenter

-}

import Git
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Length exposing (Meters)
import Point2d exposing (Point2d)


port storeCache : { key : String, value : String } -> Cmd msg


port requestCache : { key : String } -> Cmd msg


port onStoreChange : ({ key : String, value : String } -> msg) -> Sub msg


type alias Address =
    { repo : Git.Repo
    , ref : Git.Ref
    }



---- ZOOM


updateZoom : Address -> Float -> Cmd msg
updateZoom { repo, ref } zoom =
    storeCache
        { key = key [ repo.owner, repo.name, Git.refToString ref, "zoom" ]
        , value = Encode.encode 0 (Encode.float zoom)
        }


requestZoom : Address -> Cmd msg
requestZoom { repo, ref } =
    requestCache
        { key = key [ repo.owner, repo.name, Git.refToString ref, "zoom" ] }



---- CENTER


updateCenter : Address -> Point2d Meters coordinates -> Cmd msg
updateCenter { repo, ref } center =
    storeCache
        { key = key [ repo.owner, repo.name, Git.refToString ref, "center" ]
        , value = Encode.encode 0 (encodeCenter center)
        }


requestCenter : Address -> Cmd msg
requestCenter { repo, ref } =
    requestCache
        { key = key [ repo.owner, repo.name, Git.refToString ref, "center" ] }


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
    { changedZoom : Address -> Float -> msg
    , changedCenter : Address -> Point2d Meters coordinates -> msg
    , changedWhatever : msg
    }
    -> Sub msg
changedStore cfg =
    onStoreChange
        (\data ->
            let
                valueChanged repo ref value =
                    case value of
                        "zoom" ->
                            case Decode.decodeString Decode.float data.value of
                                Err _ ->
                                    cfg.changedWhatever

                                Ok zoom ->
                                    cfg.changedZoom
                                        { repo = repo
                                        , ref = ref
                                        }
                                        zoom

                        "center" ->
                            case Decode.decodeString centerDecoder data.value of
                                Err _ ->
                                    cfg.changedWhatever

                                Ok center ->
                                    cfg.changedCenter
                                        { repo = repo
                                        , ref = ref
                                        }
                                        center

                        _ ->
                            cfg.changedWhatever
            in
            case String.split "/" data.key of
                owner :: repo :: "commits" :: sha :: value :: [] ->
                    valueChanged
                        { owner = owner
                        , name = repo
                        }
                        (Git.commit sha)
                        value

                owner :: repo :: "branches" :: name :: value :: [] ->
                    valueChanged
                        { owner = owner
                        , name = repo
                        }
                        (Git.branch name)
                        value

                owner :: repo :: "tags" :: name :: value :: [] ->
                    valueChanged
                        { owner = owner
                        , name = repo
                        }
                        (Git.tag name)
                        value

                _ ->
                    cfg.changedWhatever
        )


key : List String -> String
key =
    String.join "/"
