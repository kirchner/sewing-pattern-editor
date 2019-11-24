port module Worker exposing (main)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Random.Pcg.Extended as Random
import Pattern.Store
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
    , payload : String
    }
    -> Cmd msg


port ignoreRequest : () -> Cmd msg


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
    , Cmd.none
    )



---- UPDATE


type Msg
    = OnRequest Value
    | OnDbResponse Value


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

        OnDbResponse value ->
            case Decode.decodeValue dbResponseDecoder value of
                Err error ->
                    ( model
                    , log (Decode.errorToString error)
                    )

                Ok payload ->
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
            let
                dbActionInfo =
                    { uuid = uuid
                    , storeNames = [ "patterns" ]
                    , objectStore = "patterns"
                    , keyPath = "slug"
                    }
            in
            case ( request.method, route ) of
                ( "GET", Patterns ) ->
                    ( Handling request.method route uuid
                    , Cmd.batch
                        [ log (request.method ++ " " ++ request.url.path)
                        , dbGetAll dbActionInfo
                        ]
                    )

                ( "GET", Pattern slug ) ->
                    ( Handling request.method route uuid
                    , Cmd.batch
                        [ log (request.method ++ " " ++ request.url.path)
                        , dbGet dbActionInfo slug
                        ]
                    )

                ( "POST", Patterns ) ->
                    case Decode.decodeString Pattern.Store.decoder request.body of
                        Err error ->
                            ( Finished
                            , log (Decode.errorToString error)
                            )

                        Ok storedPattern ->
                            ( Handling request.method route uuid
                            , Cmd.batch
                                [ log (request.method ++ " " ++ request.url.path)
                                , dbAdd dbActionInfo
                                    (Pattern.Store.encode storedPattern)
                                ]
                            )

                ( "PUT", Patterns ) ->
                    case Decode.decodeString Pattern.Store.decoder request.body of
                        Err error ->
                            ( Handling request.method route uuid
                            , log (Decode.errorToString error)
                            )

                        Ok storedPattern ->
                            ( Handling request.method route uuid
                            , Cmd.batch
                                [ log (request.method ++ " " ++ request.url.path)
                                , dbPut dbActionInfo
                                    (Pattern.Store.encode storedPattern)
                                ]
                            )

                ( "DELETE", Pattern slug ) ->
                    ( Handling request.method route uuid
                    , Cmd.batch
                        [ log (request.method ++ " " ++ request.url.path)
                        , dbDelete dbActionInfo slug
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



---- DATABASE


port dbRequest : Value -> Cmd msg


port onDbResponse : (Value -> msg) -> Sub msg


dbGetAll : DbActionInfo -> Cmd msg
dbGetAll info =
    dbRequest (encodeDbAction (DbGetAll info))


dbGet : DbActionInfo -> String -> Cmd msg
dbGet info key =
    dbRequest (encodeDbAction (DbGet info key))


dbAdd : DbActionInfo -> Value -> Cmd msg
dbAdd info data =
    dbRequest (encodeDbAction (DbAdd info data))


dbPut : DbActionInfo -> Value -> Cmd msg
dbPut info data =
    dbRequest (encodeDbAction (DbPut info data))


dbDelete : DbActionInfo -> String -> Cmd msg
dbDelete info key =
    dbRequest (encodeDbAction (DbDelete info key))


type DbAction
    = DbGetAll DbActionInfo
    | DbGet DbActionInfo String
    | DbAdd DbActionInfo Value
    | DbPut DbActionInfo Value
    | DbDelete DbActionInfo String


type alias DbActionInfo =
    { uuid : Uuid
    , storeNames : List String
    , objectStore : String
    , keyPath : String
    }


encodeDbAction : DbAction -> Value
encodeDbAction dbAction =
    case dbAction of
        DbGetAll info ->
            Encode.object
                [ ( "method", Encode.string "getAll" )
                , ( "info", encodeDbActionInfo info )
                ]

        DbGet info key ->
            Encode.object
                [ ( "method", Encode.string "get" )
                , ( "info", encodeDbActionInfo info )
                , ( "key", Encode.string key )
                ]

        DbAdd info data ->
            Encode.object
                [ ( "method", Encode.string "add" )
                , ( "info", encodeDbActionInfo info )
                , ( "data", data )
                ]

        DbPut info data ->
            Encode.object
                [ ( "method", Encode.string "put" )
                , ( "info", encodeDbActionInfo info )
                , ( "data", data )
                ]

        DbDelete info key ->
            Encode.object
                [ ( "method", Encode.string "delete" )
                , ( "info", encodeDbActionInfo info )
                , ( "key", Encode.string key )
                ]


encodeDbActionInfo : DbActionInfo -> Value
encodeDbActionInfo info =
    Encode.object
        [ ( "uuid", Encode.string (Uuid.toString info.uuid) )
        , ( "storeNames", Encode.list Encode.string info.storeNames )
        , ( "objectStore", Encode.string info.objectStore )
        , ( "keyPath", Encode.string info.keyPath )
        ]


type alias DbResponse =
    { uuid : String, data : String }


dbResponseDecoder : Decoder DbResponse
dbResponseDecoder =
    Decode.succeed DbResponse
        |> Decode.required "uuid" Decode.string
        |> Decode.required "data" Decode.string
