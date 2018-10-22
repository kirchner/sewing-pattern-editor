module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Page.Home as Home
import Page.Pattern as Pattern exposing (ViewedPattern)
import Pattern exposing (Pattern)
import Port
import Route exposing (Route)
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
    , cache : Dict String ViewedPattern
    }


type alias Position =
    { x : Float
    , y : Float
    }


cacheDecoder : Decoder (Dict String ViewedPattern)
cacheDecoder =
    Decode.dict Pattern.viewedPatternDecoder


encodeCache : Dict String ViewedPattern -> Value
encodeCache cache =
    Encode.dict identity Pattern.encodeViewedPattern cache



---- PAGE


type Page
    = NotFound
      -- PAGES
    | Home Home.Model
    | Pattern String Pattern.Model


type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    , maybeCache : Maybe (Dict String ViewedPattern)
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
        Err _ ->
            let
                initialCache =
                    Dict.singleton "first-pattern" Pattern.defaultViewedPattern

                page =
                    case Route.fromUrl url of
                        Nothing ->
                            NotFound

                        Just newRoute ->
                            case newRoute of
                                Route.Home ->
                                    Home Home.init

                                Route.Pattern patternSlug ->
                                    Pattern "first-pattern" Pattern.init
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
                        (Dict.singleton "first-pattern" Pattern.defaultViewedPattern)
                        flags.maybeCache

                page =
                    case Route.fromUrl url of
                        Nothing ->
                            NotFound

                        Just newRoute ->
                            case newRoute of
                                Route.Home ->
                                    Home Home.init

                                Route.Pattern patternSlug ->
                                    case Dict.get patternSlug cache of
                                        Nothing ->
                                            NotFound

                                        Just viewedPattern ->
                                            Pattern patternSlug Pattern.init
            in
            ( { prefix = Route.prefixFromUrl url
              , key = key
              , page = page
              , windowWidth = flags.windowWidth
              , windowHeight = flags.windowHeight
              , cache = cache
              }
            , if flags.maybeCache == Nothing then
                Port.safeCache (encodeCache cache)

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
            let
                namedPatterns =
                    model.cache
                        |> Dict.toList
                        |> List.map
                            (\( patternSlug, { pattern } ) ->
                                ( patternSlug, pattern )
                            )
            in
            { title = "Sewing pattern editor"
            , body = [ Html.map HomeMsg (Home.view namedPatterns homeModel) ]
            }

        Pattern patternSlug patternModel ->
            case Dict.get patternSlug model.cache of
                Nothing ->
                    { title = "Sewing pattern editor"
                    , body = [ Html.text "We are sorry, but something went wrong." ]
                    }

                Just viewedPattern ->
                    let
                        patternDocument =
                            Pattern.view
                                model.prefix
                                model.windowWidth
                                model.windowHeight
                                patternSlug
                                viewedPattern
                                patternModel
                    in
                    { title = patternDocument.title
                    , body = List.map (Html.map PatternMsg) patternDocument.body
                    }



---- UPDATE


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | CacheChanged Value
      -- PAGES
    | HomeMsg Home.Msg
    | PatternMsg Pattern.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    changeRouteTo (Route.fromUrl url) model

                Browser.External externalUrl ->
                    ( model
                    , Navigation.load externalUrl
                    )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( CacheChanged value, _ ) ->
            case Decode.decodeValue cacheDecoder value of
                Err _ ->
                    ( model, Cmd.none )

                Ok newCache ->
                    ( { model | cache = newCache }
                    , Cmd.none
                    )

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

        ( PatternMsg patternMsg, Pattern patternSlug patternModel ) ->
            case Dict.get patternSlug model.cache of
                Nothing ->
                    ( model, Cmd.none )

                Just viewedPattern ->
                    let
                        ( newPatternModel, patternCmd, maybeNewViewedPattern ) =
                            Pattern.update model.key viewedPattern patternMsg patternModel

                        newModel =
                            { model | page = Pattern patternSlug newPatternModel }
                    in
                    case maybeNewViewedPattern of
                        Nothing ->
                            ( newModel
                            , Cmd.map PatternMsg patternCmd
                            )

                        Just newViewedPattern ->
                            let
                                newCache =
                                    Dict.insert patternSlug newViewedPattern newModel.cache
                            in
                            ( { newModel | cache = newCache }
                            , Cmd.batch
                                [ Cmd.map PatternMsg patternCmd
                                , Port.safeCache (encodeCache newCache)
                                ]
                            )

        _ ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        newPage =
            case maybeRoute of
                Nothing ->
                    NotFound

                Just newRoute ->
                    case newRoute of
                        Route.Home ->
                            Home Home.init

                        Route.Pattern patternSlug ->
                            case Dict.get patternSlug model.cache of
                                Nothing ->
                                    NotFound

                                Just viewedPattern ->
                                    Pattern patternSlug Pattern.init
    in
    ( { model | page = newPage }
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

            Pattern _ patternModel ->
                Sub.map PatternMsg (Pattern.subscriptions patternModel)
        ]
