port module Worker exposing (main)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Random.Pcg.Extended as Random
import StoredPattern
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>))
import Uuid exposing (Uuid)


port log : String -> Cmd msg


port onRequest : (Value -> msg) -> Sub msg


type alias Request =
    { url : Url
    , method : String
    , body : String
    }


port sendJsonResponse :
    { statusCode : Int
    , statusText : String
    , payload : Value
    }
    -> Cmd msg


port ignoreRequest : () -> Cmd msg


port dbGetAll :
    { uuid : String
    , storeNames : List String
    , objectStore : String
    }
    -> Cmd msg


port dbGet :
    { uuid : String
    , storeNames : List String
    , objectStore : String
    , key : String
    }
    -> Cmd msg


port dbAdd :
    { uuid : String
    , storeNames : List String
    , objectStore : String
    , data : Value
    }
    -> Cmd msg


port dbPut :
    { uuid : String
    , storeNames : List String
    , objectStore : String
    , data : Value
    }
    -> Cmd msg


port dbDelete :
    { uuid : String
    , storeNames : List String
    , objectStore : String
    , key : String
    }
    -> Cmd msg


port onDbResponse : ({ uuid : String, data : Value } -> msg) -> Sub msg


onRequestDecoder : Decoder Request
onRequestDecoder =
    Decode.succeed Request
        |> Decode.required "url" urlDecoder
        |> Decode.required "method" Decode.string
        |> Decode.required "body" Decode.string


urlDecoder : Decoder Url
urlDecoder =
    Decode.string
        |> Decode.andThen
            (\rawUrl ->
                case Url.fromString rawUrl of
                    Nothing ->
                        Decode.fail ("invalid url: " ++ rawUrl)

                    Just url ->
                        Decode.succeed url
            )


protocolDecoder : Decoder Protocol
protocolDecoder =
    Decode.string
        |> Decode.andThen
            (\rawProtocol ->
                case rawProtocol of
                    "http" ->
                        Decode.succeed Http

                    "https" ->
                        Decode.succeed Https

                    _ ->
                        Decode.fail ("unsupported protocol: " ++ rawProtocol)
            )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL


type Model
    = Initialized Random.Seed
    | Handling String Route Uuid
    | Finished


type alias Flags =
    { seed : Int
    , seedExtension : List Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Initialized (Random.initialSeed flags.seed flags.seedExtension)
    , log "elm worker started"
    )



---- UPDATE


type Msg
    = OnRequest Value
    | OnDbResponse { uuid : String, data : Value }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRequest value ->
            case model of
                Initialized seed ->
                    case Decode.decodeValue onRequestDecoder value of
                        Err error ->
                            ( model
                            , log (Decode.errorToString error)
                            )

                        Ok request ->
                            handle seed request

                _ ->
                    ( model, Cmd.none )

        OnDbResponse payload ->
            case model of
                Handling method route uuid ->
                    if Uuid.toString uuid == payload.uuid then
                        let
                            ( statusCode, statusText ) =
                                case ( method, route ) of
                                    ( "GET", Patterns ) ->
                                        ( 200, "OK" )

                                    ( "GET", Pattern _ ) ->
                                        ( 200, "OK" )

                                    ( "POST", Patterns ) ->
                                        ( 201, "Created" )

                                    ( "PUT", Patterns ) ->
                                        ( 200, "Updated" )

                                    ( "DELETE", Pattern slug ) ->
                                        ( 200, "Deleted" )

                                    _ ->
                                        ( 500, "Internal server error" )
                        in
                        ( Finished
                        , Cmd.batch
                            [ log <|
                                String.join " "
                                    [ "finished request"
                                    , String.fromInt statusCode
                                    , statusText
                                    ]
                            , sendJsonResponse
                                { statusCode = statusCode
                                , statusText = statusText
                                , payload = payload.data
                                }
                            ]
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


type Route
    = Patterns
    | Pattern String


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.s "api"
        </> Url.Parser.oneOf
                [ Url.Parser.map Patterns (Url.Parser.s "patterns")
                , Url.Parser.map Pattern (Url.Parser.s "patterns" </> Url.Parser.string)
                ]


handle : Random.Seed -> Request -> ( Model, Cmd Msg )
handle seed request =
    let
        ( uuid, _ ) =
            Random.step Uuid.generator seed
    in
    case Url.Parser.parse routeParser request.url of
        Nothing ->
            ( Finished
            , Cmd.batch
                [ log "could not parse route"
                , ignoreRequest ()
                ]
            )

        Just route ->
            case ( request.method, route ) of
                ( "GET", Patterns ) ->
                    ( Handling request.method route uuid
                    , Cmd.batch
                        [ log (request.method ++ " " ++ request.url.path)
                        , dbGetAll
                            { uuid = Uuid.toString uuid
                            , storeNames = [ "patterns" ]
                            , objectStore = "patterns"
                            }
                        ]
                    )

                ( "GET", Pattern slug ) ->
                    ( Handling request.method route uuid
                    , Cmd.batch
                        [ log (request.method ++ " " ++ request.url.path)
                        , dbGet
                            { uuid = Uuid.toString uuid
                            , storeNames = [ "patterns" ]
                            , objectStore = "patterns"
                            , key = slug
                            }
                        ]
                    )

                ( "POST", Patterns ) ->
                    case Decode.decodeString StoredPattern.decoder request.body of
                        Err error ->
                            ( Finished
                            , log (Decode.errorToString error)
                            )

                        Ok storedPattern ->
                            ( Handling request.method route uuid
                            , Cmd.batch
                                [ log (request.method ++ " " ++ request.url.path)
                                , dbAdd
                                    { uuid = Uuid.toString uuid
                                    , storeNames = [ "patterns" ]
                                    , objectStore = "patterns"
                                    , data = StoredPattern.encode storedPattern
                                    }
                                ]
                            )

                ( "PUT", Patterns ) ->
                    case Decode.decodeString StoredPattern.decoder request.body of
                        Err error ->
                            ( Handling request.method route uuid
                            , log (Decode.errorToString error)
                            )

                        Ok storedPattern ->
                            ( Handling request.method route uuid
                            , Cmd.batch
                                [ log (request.method ++ " " ++ request.url.path)
                                , dbPut
                                    { uuid = Uuid.toString uuid
                                    , storeNames = [ "patterns" ]
                                    , objectStore = "patterns"
                                    , data = StoredPattern.encode storedPattern
                                    }
                                ]
                            )

                ( "DELETE", Pattern slug ) ->
                    ( Handling request.method route uuid
                    , Cmd.batch
                        [ log (request.method ++ " " ++ request.url.path)
                        , dbDelete
                            { uuid = Uuid.toString uuid
                            , storeNames = [ "patterns" ]
                            , objectStore = "patterns"
                            , key = slug
                            }
                        ]
                    )

                _ ->
                    ( Finished
                    , Cmd.batch
                        [ log "could not handle method"
                        , ignoreRequest ()
                        ]
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onRequest OnRequest
        , onDbResponse OnDbResponse
        ]
