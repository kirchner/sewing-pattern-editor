module Main exposing (main)

{-
   Sewing pattern editor
   Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Git
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Page.Details as Details
import Page.Pattern as Pattern
import Page.PatternNew as PatternNew
import Page.Patterns as Patterns
import Pattern exposing (Pattern)
import Ports
import RemoteData exposing (WebData)
import Route exposing (Route)
import Task
import Ui.Atom
import Ui.Atom.Input
import Ui.Molecule.Modal
import Ui.Theme.Color
import Ui.Theme.Spacing
import Url exposing (Url)
import Url.Builder


main : Program {} Model Msg
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


type Model
    = RequestingClientId RequestingClientIdData
    | Loading LoadingData
    | Loaded LoadedData


type alias RequestingClientIdData =
    { key : Browser.Navigation.Key
    , domain : String
    , url : Url
    }


type alias LoadingData =
    { key : Browser.Navigation.Key
    , domain : String
    , clientId : String
    , maybeRoute : Maybe Route
    , githubAccessToken : WebData String
    , maybeDevice : Maybe Element.Device
    }


type alias LoadedData =
    { device : Element.Device
    , identity : Git.Identity
    , key : Browser.Navigation.Key
    , domain : String
    , clientId : String
    , page : Page
    }



---- PAGE


type Page
    = NotFound
      -- PAGES
    | Patterns Patterns.Model
    | PatternNew PatternNew.Model
    | Pattern Pattern.Model
    | Details Details.Model


init : {} -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        domain =
            String.concat
                [ case url.protocol of
                    Url.Http ->
                        "http"

                    Url.Https ->
                        "https"
                , "://"
                , url.host
                , case url.port_ of
                    Nothing ->
                        ""

                    Just port_ ->
                        ":" ++ String.fromInt port_
                ]
    in
    ( RequestingClientId
        { key = key
        , domain = domain
        , url = url
        }
    , Http.get
        { url = "/client_id"
        , expect = Http.expectString ReceivedClientId
        }
    )



---- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        RequestingClientId _ ->
            { title = "Requesting Client ID..."
            , body =
                [ viewHelp <|
                    Element.el [ Element.centerX, Element.centerY ] <|
                        Element.text "Requesting Client ID..."
                ]
            }

        Loading data ->
            { title = "Requesting GitHub API Access Token..."
            , body =
                [ viewHelp <|
                    Element.el [ Element.centerX, Element.centerY ] <|
                        Element.text "Requesting GitHub API Access Token..."
                ]
            }

        Loaded data ->
            case data.page of
                NotFound ->
                    { title = "Sewing pattern editor"
                    , body =
                        [ viewHelp <|
                            Element.el [ Element.centerX, Element.centerY ] <|
                                Element.text "We are sorry, this page does not exist."
                        ]
                    }

                Patterns patternsModel ->
                    let
                        { title, body, dialog } =
                            Patterns.view data.device data.identity patternsModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map PatternsMsg body) ]
                    }

                PatternNew newModel ->
                    let
                        { title, body, dialog } =
                            PatternNew.view data.device data.identity newModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map PatternNewMsg body) ]
                    }

                Pattern patternModel ->
                    let
                        { title, body, dialog } =
                            Pattern.view data.device data.identity patternModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map PatternMsg body) ]
                    }

                Details detailsModel ->
                    let
                        { title, body, dialog } =
                            Details.view data.device data.identity detailsModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map DetailsMsg body) ]
                    }


viewHelp : Element msg -> Html msg
viewHelp body =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.family
            [ Font.typeface "Rubik"
            , Font.sansSerif
            ]
        ]
        body



---- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
      -- CLIENT ID
    | ReceivedClientId (Result Http.Error String)
      -- LOADING
    | ReceivedGithubAccessToken (WebData String)
    | ChangedDevice Element.Device
      -- PAGES
    | PatternsMsg Patterns.Msg
    | PatternNewMsg PatternNew.Msg
    | PatternMsg Pattern.Msg
    | DetailsMsg Details.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        RequestingClientId data ->
            case msg of
                ReceivedClientId (Err httpError) ->
                    ( model, Cmd.none )

                ReceivedClientId (Ok clientId) ->
                    case Route.fromUrlWithCode data.url of
                        Nothing ->
                            ( Loading
                                { key = data.key
                                , domain = data.domain
                                , clientId = clientId
                                , maybeRoute = Nothing
                                , githubAccessToken = RemoteData.NotAsked
                                , maybeDevice = Nothing
                                }
                            , Browser.Dom.getViewport
                                |> Task.perform
                                    (\{ viewport } ->
                                        ChangedDevice
                                            (Element.classifyDevice
                                                { width = floor viewport.width
                                                , height = floor viewport.height
                                                }
                                            )
                                    )
                            )

                        Just { route, code } ->
                            case code of
                                Nothing ->
                                    ( Loading
                                        { key = data.key
                                        , domain = data.domain
                                        , clientId = clientId
                                        , maybeRoute = Just route
                                        , githubAccessToken = RemoteData.NotAsked
                                        , maybeDevice = Nothing
                                        }
                                    , Browser.Dom.getViewport
                                        |> Task.perform
                                            (\{ viewport } ->
                                                ChangedDevice
                                                    (Element.classifyDevice
                                                        { width = floor viewport.width
                                                        , height = floor viewport.height
                                                        }
                                                    )
                                            )
                                    )

                                Just actualCode ->
                                    ( Loading
                                        { key = data.key
                                        , domain = data.domain
                                        , clientId = clientId
                                        , maybeRoute = Just route
                                        , githubAccessToken = RemoteData.Loading
                                        , maybeDevice = Nothing
                                        }
                                    , Cmd.batch
                                        [ Browser.Dom.getViewport
                                            |> Task.perform
                                                (\{ viewport } ->
                                                    ChangedDevice
                                                        (Element.classifyDevice
                                                            { width = floor viewport.width
                                                            , height = floor viewport.height
                                                            }
                                                        )
                                                )
                                        , Http.post
                                            { url = Url.Builder.absolute [ "access_token" ] []
                                            , body =
                                                Http.multipartBody
                                                    [ Http.stringPart "code" actualCode ]
                                            , expect =
                                                Http.expectJson
                                                    (RemoteData.fromResult
                                                        >> ReceivedGithubAccessToken
                                                    )
                                                    (Decode.field "access_token"
                                                        Decode.string
                                                    )
                                            }
                                        ]
                                    )

                _ ->
                    ( model, Cmd.none )

        Loading data ->
            case msg of
                ReceivedGithubAccessToken githubAccessToken ->
                    { data | githubAccessToken = githubAccessToken }
                        |> checkLoaded

                ChangedDevice device ->
                    { data | maybeDevice = Just device }
                        |> checkLoaded

                _ ->
                    ( model, Cmd.none )

        Loaded data ->
            case ( msg, data.page ) of
                ( UrlRequested urlRequest, _ ) ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Browser.Navigation.pushUrl data.key url.path
                            )

                        Browser.External externalUrl ->
                            ( model
                            , Browser.Navigation.load externalUrl
                            )

                ( UrlChanged url, _ ) ->
                    case Route.fromUrl url of
                        Nothing ->
                            ( Loaded { data | page = NotFound }
                            , Cmd.none
                            )

                        Just route ->
                            let
                                ( page, cmd ) =
                                    changePageTo data.identity route
                            in
                            ( Loaded { data | page = page }
                            , cmd
                            )

                -- CLIENT ID
                ( ReceivedClientId _, _ ) ->
                    ( model, Cmd.none )

                -- TOKENS
                ( ReceivedGithubAccessToken _, _ ) ->
                    ( model, Cmd.none )

                ( ChangedDevice device, _ ) ->
                    if data.device /= device then
                        ( Loaded { data | device = device }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                -- PAGES
                ( _, NotFound ) ->
                    ( model, Cmd.none )

                ( PatternsMsg patternsMsg, Patterns patternsModel ) ->
                    let
                        ( newPatternsModel, patternsCmd ) =
                            Patterns.update
                                data.key
                                data.domain
                                data.clientId
                                data.identity
                                patternsMsg
                                patternsModel
                    in
                    ( Loaded { data | page = Patterns newPatternsModel }
                    , Cmd.map PatternsMsg patternsCmd
                    )

                ( PatternsMsg _, _ ) ->
                    ( model, Cmd.none )

                ( PatternNewMsg newMsg, PatternNew newModel ) ->
                    let
                        ( newNewModel, newCmd ) =
                            PatternNew.update
                                data.key
                                data.domain
                                data.clientId
                                data.identity
                                newMsg
                                newModel
                    in
                    ( Loaded { data | page = PatternNew newNewModel }
                    , Cmd.map PatternNewMsg newCmd
                    )

                ( PatternNewMsg _, _ ) ->
                    ( model, Cmd.none )

                ( PatternMsg patternMsg, Pattern patternModel ) ->
                    let
                        ( newPatternModel, patternCmd ) =
                            Pattern.update
                                data.key
                                data.domain
                                data.clientId
                                data.device
                                data.identity
                                patternMsg
                                patternModel
                    in
                    ( Loaded { data | page = Pattern newPatternModel }
                    , Cmd.map PatternMsg patternCmd
                    )

                ( PatternMsg _, _ ) ->
                    ( model, Cmd.none )

                ( DetailsMsg detailsMsg, Details detailsModel ) ->
                    let
                        ( newDetailsModel, detailsCmd ) =
                            Details.update
                                data.key
                                data.domain
                                data.clientId
                                data.device
                                data.identity
                                detailsMsg
                                detailsModel
                    in
                    ( Loaded { data | page = Details newDetailsModel }
                    , Cmd.map DetailsMsg detailsCmd
                    )

                ( DetailsMsg _, _ ) ->
                    ( model, Cmd.none )


checkLoaded : LoadingData -> ( Model, Cmd Msg )
checkLoaded data =
    case ( data.githubAccessToken, data.maybeDevice ) of
        ( RemoteData.Success githubAccessToken, Just device ) ->
            initLoaded data (Just githubAccessToken) device

        ( RemoteData.NotAsked, Just device ) ->
            initLoaded data Nothing device

        _ ->
            ( Loading data
            , Cmd.none
            )


initLoaded : LoadingData -> Maybe String -> Element.Device -> ( Model, Cmd Msg )
initLoaded data maybeGithubAccessToken device =
    let
        ( page, cmd ) =
            case data.maybeRoute of
                Nothing ->
                    ( NotFound
                    , Cmd.none
                    )

                Just route ->
                    changePageTo identity route

        identity =
            case maybeGithubAccessToken of
                Nothing ->
                    Git.Anonymous

                Just githubAccessToken ->
                    Git.OauthToken githubAccessToken
    in
    ( Loaded
        { device = device
        , identity = identity
        , domain = data.domain
        , key = data.key
        , clientId = data.clientId
        , page = page
        }
    , Cmd.batch
        [ cmd
        , case data.maybeRoute of
            Nothing ->
                Cmd.none

            Just route ->
                Route.replaceUrl data.key route
        ]
    )


changePageTo : Git.Identity -> Route -> ( Page, Cmd Msg )
changePageTo identity route =
    case route of
        Route.Patterns ->
            let
                ( patterns, patternsCmd ) =
                    Patterns.init
            in
            ( Patterns patterns
            , Cmd.map PatternsMsg patternsCmd
            )

        Route.Pattern address ->
            let
                ( pattern, patternCmd ) =
                    Pattern.init identity address
            in
            ( Pattern pattern
            , Cmd.map PatternMsg patternCmd
            )

        Route.PatternNew newParameters ->
            let
                ( new, newCmd ) =
                    PatternNew.init identity newParameters
            in
            ( PatternNew new
            , Cmd.map PatternNewMsg newCmd
            )

        Route.Details address ->
            let
                ( details, detailsCmd ) =
                    Details.init identity address
            in
            ( Details details
            , Cmd.map DetailsMsg detailsCmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        RequestingClientId _ ->
            Sub.none

        Loading _ ->
            Browser.Events.onResize <|
                \width height ->
                    ChangedDevice <|
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }

        Loaded data ->
            Sub.batch
                [ Browser.Events.onResize <|
                    \width height ->
                        ChangedDevice <|
                            Element.classifyDevice
                                { width = width
                                , height = height
                                }
                , case data.page of
                    NotFound ->
                        Sub.none

                    -- PAGES
                    Patterns patternsModel ->
                        Sub.map PatternsMsg (Patterns.subscriptions patternsModel)

                    PatternNew newModel ->
                        Sub.map PatternNewMsg (PatternNew.subscriptions newModel)

                    Pattern patternModel ->
                        Sub.map PatternMsg (Pattern.subscriptions patternModel)

                    Details detailsModel ->
                        Sub.map DetailsMsg (Details.subscriptions detailsModel)
                ]
