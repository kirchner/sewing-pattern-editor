module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Page.Editor as Editor
import Page.Home as Home
import Pattern exposing (Pattern)
import Port
import Route exposing (Route)
import StoredPattern exposing (StoredPattern)
import Url exposing (Url)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



---- MODEL


type alias Model =
    { prefix : String
    , key : Navigation.Key
    , page : Page
    , windowWidth : Int
    , windowHeight : Int
    , cache : Dict String StoredPattern
    }


type alias Position =
    { x : Float
    , y : Float
    }


cacheDecoder : Decoder (Dict String StoredPattern)
cacheDecoder =
    Decode.dict StoredPattern.decoder


encodeCache : Dict String StoredPattern -> Value
encodeCache cache =
    Encode.dict identity StoredPattern.encode cache



---- PAGE


type Page
    = NotFound
      -- PAGES
    | Home Home.Model
    | Editor String Editor.Model


type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    , maybeCache : Maybe (Dict String StoredPattern)
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Decode.required "windowWidth" Decode.int
        |> Decode.required "windowHeight" Decode.int
        |> Decode.optional "cache" (Decode.map Just cacheDecoder) Nothing


init : Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init value url key =
    case Decode.decodeValue flagsDecoder value of
        Err error ->
            let
                initialCache =
                    Dict.singleton "first-pattern"
                        (StoredPattern.init "first-pattern" "First pattern")

                page =
                    case Route.fromUrl url of
                        Nothing ->
                            NotFound

                        Just newRoute ->
                            case newRoute of
                                Route.Home ->
                                    Home Home.init

                                Route.Editor patternSlug maybePoint ->
                                    Editor "first-pattern" Editor.init
            in
            ( { prefix = Route.prefixFromUrl url
              , key = key
              , page = page
              , windowWidth = 1280
              , windowHeight = 640
              , cache = initialCache
              }
            , Port.safeCache (encodeCache initialCache)
            )

        Ok flags ->
            let
                cache =
                    Maybe.withDefault
                        (Dict.singleton "first-pattern"
                            (StoredPattern.init "first-pattern" "First pattern")
                        )
                        flags.maybeCache

                ( page, computedCache ) =
                    case Route.fromUrl url of
                        Nothing ->
                            ( NotFound, cache )

                        Just newRoute ->
                            case newRoute of
                                Route.Home ->
                                    ( Home Home.init, cache )

                                Route.Editor patternSlug maybePoint ->
                                    case Dict.get patternSlug cache of
                                        Nothing ->
                                            ( NotFound, cache )

                                        Just viewedPattern ->
                                            let
                                                computedPattern =
                                                    { viewedPattern
                                                        | pattern =
                                                            viewedPattern.pattern
                                                                |> Pattern.computeGeometry
                                                                |> Tuple.first
                                                    }

                                                newCache =
                                                    Dict.insert patternSlug
                                                        computedPattern
                                                        cache
                                            in
                                            ( Editor patternSlug Editor.init
                                            , newCache
                                            )
            in
            ( { prefix = Route.prefixFromUrl url
              , key = key
              , page = page
              , windowWidth = flags.windowWidth
              , windowHeight = flags.windowHeight
              , cache = computedCache
              }
            , if flags.maybeCache == Nothing then
                Port.safeCache (encodeCache computedCache)

              else
                Cmd.none
            )



---- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "Sewing pattern editor"
            , body = [ Html.text "We are sorry, this page does not exist." ]
            }

        Home homeModel ->
            { title = "Sewing pattern editor"
            , body =
                [ Html.map HomeMsg
                    (Home.view model.prefix
                        (Dict.values model.cache)
                        homeModel
                    )
                ]
            }

        Editor patternSlug editorModel ->
            case Dict.get patternSlug model.cache of
                Nothing ->
                    { title = "Sewing pattern editor"
                    , body = [ Html.text "We are sorry, but something went wrong." ]
                    }

                Just storedPattern ->
                    let
                        patternDocument =
                            Editor.view
                                model.prefix
                                model.windowWidth
                                model.windowHeight
                                storedPattern
                                editorModel
                    in
                    { title = patternDocument.title
                    , body = List.map (Html.map EditorMsg) patternDocument.body
                    }



---- UPDATE


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | CacheChanged Value
      -- PAGES
    | HomeMsg Home.Msg
    | EditorMsg Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key url.path
                    )

                Browser.External externalUrl ->
                    ( model
                    , Navigation.load externalUrl
                    )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( CacheChanged value, _ ) ->
            --case Decode.decodeValue cacheDecoder value of
            --    Err _ ->
            --        ( model, Cmd.none )
            --    Ok newCache ->
            --        ( { model | cache = newCache }
            --        , Cmd.none
            --        )
            -- FIXME: try simple geometry caching
            ( model, Cmd.none )

        -- PAGES
        ( _, NotFound ) ->
            ( model, Cmd.none )

        ( HomeMsg homeMsg, Home homeModel ) ->
            let
                ( newHomeModel, homeCmd, maybeNewCache ) =
                    Home.update model.key model.cache homeMsg homeModel
            in
            case maybeNewCache of
                Nothing ->
                    ( { model | page = Home newHomeModel }
                    , Cmd.map HomeMsg homeCmd
                    )

                Just newCache ->
                    ( { model | page = Home newHomeModel }
                    , Cmd.batch
                        [ Cmd.map HomeMsg homeCmd
                        , Port.safeCache (encodeCache newCache)
                        ]
                    )

        ( EditorMsg patternMsg, Editor patternSlug editorModel ) ->
            case Dict.get patternSlug model.cache of
                Nothing ->
                    ( model, Cmd.none )

                Just viewedPattern ->
                    let
                        ( newEditorModel, patternCmd, maybeNewViewedPattern ) =
                            Editor.update model.key viewedPattern patternMsg editorModel

                        newModel =
                            { model | page = Editor patternSlug newEditorModel }
                    in
                    case maybeNewViewedPattern of
                        Nothing ->
                            ( newModel
                            , Cmd.map EditorMsg patternCmd
                            )

                        Just newViewedPattern ->
                            let
                                computedPattern =
                                    { newViewedPattern
                                        | pattern =
                                            newViewedPattern.pattern
                                                |> Pattern.computeGeometry
                                                |> Tuple.first
                                    }

                                newCache =
                                    Dict.insert patternSlug computedPattern newModel.cache
                            in
                            ( { newModel | cache = newCache }
                            , Cmd.batch
                                [ Cmd.map EditorMsg patternCmd
                                , Port.safeCache (encodeCache newCache)
                                ]
                            )

        _ ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        newModel =
            case maybeRoute of
                Nothing ->
                    { model | page = NotFound }

                Just newRoute ->
                    case newRoute of
                        Route.Home ->
                            { model | page = Home Home.init }

                        Route.Editor patternSlug maybePoint ->
                            case Dict.get patternSlug model.cache of
                                Nothing ->
                                    { model | page = NotFound }

                                Just viewedPattern ->
                                    let
                                        computedPattern =
                                            { viewedPattern
                                                | pattern =
                                                    viewedPattern.pattern
                                                        |> Pattern.computeGeometry
                                                        |> Tuple.first
                                            }

                                        newCache =
                                            Dict.insert patternSlug
                                                computedPattern
                                                model.cache
                                    in
                                    { model
                                        | page = Editor patternSlug Editor.init
                                        , cache = newCache
                                    }
    in
    ( newModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.onCacheChange CacheChanged
        , case model.page of
            NotFound ->
                Sub.none

            -- PAGES
            Home homeModel ->
                Sub.map HomeMsg (Home.subscriptions homeModel)

            Editor _ editorModel ->
                Sub.map EditorMsg (Editor.subscriptions editorModel)
        ]
