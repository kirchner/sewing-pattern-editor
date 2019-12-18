port module LocalStorage exposing
    ( Address(..), addressParser, addressToPathSegments
    , changedStore
    , updateAddresses, requestAddresses
    , updateZoom, requestZoom
    , updateCenter, requestCenter
    )

{-|

@docs Address, addressParser, addressToPathSegments
@docs changedStore
@docs updateAddresses, requestAddresses
@docs updateZoom, requestZoom
@docs updateCenter, requestCenter

-}

import Git
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


port storeCache : { key : String, value : String } -> Cmd msg


port requestCache : { key : String } -> Cmd msg


port onStoreChange : ({ key : String, value : String } -> msg) -> Sub msg


port onStoreMissing : ({ key : String } -> msg) -> Sub msg


type Address
    = GitRepo { repo : Git.Repo, ref : Git.Ref }
    | Browser { slug : String }


addressParser : Parser (Address -> a) a
addressParser =
    oneOf
        [ map (\repo ref -> GitRepo { repo = repo, ref = ref })
            (top </> s "github" </> Git.repoParser </> Git.refParser)
        , map (\slug -> Browser { slug = slug })
            (top </> s "browser" </> string)
        ]


addressToPathSegments : Address -> List String
addressToPathSegments address =
    case address of
        GitRepo { repo, ref } ->
            "github" :: repo.owner :: repo.name :: Git.refToPathSegments ref

        Browser { slug } ->
            "browser" :: slug :: []


addressDecoder : Decoder Address
addressDecoder =
    Decode.oneOf
        [ Decode.succeed (\repo ref -> GitRepo { repo = repo, ref = ref })
            |> Decode.required "repo" Git.repoDecoder
            |> Decode.required "ref" Git.refDecoder
            |> Decode.ensureType "gitRepo"
        , Decode.succeed (\slug -> Browser { slug = slug })
            |> Decode.required "slug" Decode.string
            |> Decode.ensureType "browser"
        ]


encodeAddress : Address -> Value
encodeAddress address =
    case address of
        GitRepo { repo, ref } ->
            Encode.withType "gitRepo"
                [ ( "repo", Git.encodeRepo repo )
                , ( "ref", Git.encodeRef ref )
                ]

        Browser { slug } ->
            Encode.withType "browser"
                [ ( "slug", Encode.string slug ) ]



---- ADDRESSES


updateAddresses : List Address -> Cmd msg
updateAddresses addresses =
    storeCache
        { key = "addresses"
        , value = Encode.encode 0 (Encode.list encodeAddress addresses)
        }


requestAddresses : Cmd msg
requestAddresses =
    requestCache
        { key = "addresses" }



---- ZOOM


updateZoom : Address -> Float -> Cmd msg
updateZoom address zoom =
    storeCache
        { key = key address "zoom"
        , value = Encode.encode 0 (Encode.float zoom)
        }


requestZoom : Address -> Cmd msg
requestZoom address =
    requestCache
        { key = key address "zoom" }



---- CENTER


updateCenter : Address -> Point2d Meters coordinates -> Cmd msg
updateCenter address center =
    storeCache
        { key = key address "center"
        , value = Encode.encode 0 (encodeCenter center)
        }


requestCenter : Address -> Cmd msg
requestCenter address =
    requestCache
        { key = key address "center" }


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
    , changedAddresses : List Address -> msg
    , changedWhatever : msg
    }
    -> Sub msg
changedStore cfg =
    Sub.batch
        [ onStoreMissing
            (\data ->
                if data.key == "addresses" then
                    cfg.changedAddresses []

                else
                    cfg.changedWhatever
            )
        , onStoreChange
            (\data ->
                let
                    valueChanged address value =
                        case value of
                            "zoom" ->
                                case Decode.decodeString Decode.float data.value of
                                    Err _ ->
                                        cfg.changedWhatever

                                    Ok zoom ->
                                        cfg.changedZoom address zoom

                            "center" ->
                                case Decode.decodeString centerDecoder data.value of
                                    Err _ ->
                                        cfg.changedWhatever

                                    Ok center ->
                                        cfg.changedCenter address center

                            _ ->
                                cfg.changedWhatever
                in
                case String.split "/" data.key of
                    "addresses" :: [] ->
                        case Decode.decodeString (Decode.list addressDecoder) data.value of
                            Err _ ->
                                cfg.changedWhatever

                            Ok addresses ->
                                cfg.changedAddresses addresses

                    "github" :: owner :: repo :: refType :: refValue :: value :: [] ->
                        let
                            maybeRef =
                                case refType of
                                    "commits" ->
                                        Just (Git.commit refValue)

                                    "branches" ->
                                        Just (Git.branch refValue)

                                    "tags" ->
                                        Just (Git.tag refValue)

                                    _ ->
                                        Nothing
                        in
                        case maybeRef of
                            Nothing ->
                                cfg.changedWhatever

                            Just ref ->
                                valueChanged
                                    (GitRepo
                                        { repo = { owner = owner, name = repo }
                                        , ref = ref
                                        }
                                    )
                                    value

                    "browser" :: slug :: value :: [] ->
                        valueChanged (Browser { slug = slug }) value

                    _ ->
                        cfg.changedWhatever
            )
        ]


key : Address -> String -> String
key address name =
    String.join "/" <|
        case address of
            GitRepo { repo, ref } ->
                [ "github", repo.owner, repo.name, Git.refToString ref, name ]

            Browser { slug } ->
                [ "browser", slug ]
