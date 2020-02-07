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
import Element exposing (Element)
import Element.Font as Font
import Github
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Page.Details as Details
import Page.Pattern as Pattern
import Page.PatternNew as PatternNew
import Page.Patterns as Patterns
import Pattern exposing (Pattern)
import RemoteData exposing (WebData)
import Route exposing (Route)
import Session exposing (Session)
import Task
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
    { session : Session
    , maybeRoute : Maybe Route
    , githubAccessToken : WebData String
    , maybeDevice : Maybe Element.Device
    }


type alias LoadedData =
    { device : Element.Device
    , page : Page
    }


toSession : Page -> Session
toSession page =
    case page of
        NotFound session ->
            session

        -- PAGES
        Patterns patterns ->
            Patterns.toSession patterns

        PatternNew patternNew ->
            PatternNew.toSession patternNew

        Pattern pattern ->
            Pattern.toSession pattern

        Details details ->
            Details.toSession details



---- PAGE


type Page
    = NotFound Session
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

        Loading _ ->
            { title = "Requesting GitHub API Access Token..."
            , body =
                [ viewHelp <|
                    Element.el [ Element.centerX, Element.centerY ] <|
                        Element.text "Requesting GitHub API Access Token..."
                ]
            }

        Loaded data ->
            case data.page of
                NotFound _ ->
                    { title = "Sewing pattern editor"
                    , body =
                        [ viewHelp <|
                            Element.el [ Element.centerX, Element.centerY ] <|
                                Element.text "We are sorry, this page does not exist."
                        ]
                    }

                Patterns patternsModel ->
                    let
                        { title, body } =
                            Patterns.view data.device patternsModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map PatternsMsg body) ]
                    }

                PatternNew newModel ->
                    let
                        { title, body } =
                            PatternNew.view data.device newModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map PatternNewMsg body) ]
                    }

                Pattern patternModel ->
                    let
                        { title, body } =
                            Pattern.view data.device patternModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map PatternMsg body) ]
                    }

                Details detailsModel ->
                    let
                        { title, body } =
                            Details.view data.device detailsModel
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
            updateRequestingClientId msg data

        Loading data ->
            updateLoading msg data

        Loaded data ->
            updateLoaded msg data



---- UPDATE REQUESTING CLIENT ID


updateRequestingClientId : Msg -> RequestingClientIdData -> ( Model, Cmd Msg )
updateRequestingClientId msg data =
    case msg of
        ReceivedClientId (Err _) ->
            ( RequestingClientId data, Cmd.none )

        ReceivedClientId (Ok clientId) ->
            let
                session =
                    Session.anonymous clientId data.key data.domain
            in
            case Route.fromUrlWithCode data.url of
                Nothing ->
                    ( Loading
                        { session = session
                        , maybeRoute = Nothing
                        , githubAccessToken = RemoteData.NotAsked
                        , maybeDevice = Nothing
                        }
                    , getViewport
                    )

                Just { route, code } ->
                    case code of
                        Nothing ->
                            ( Loading
                                { session = session
                                , maybeRoute = Just route
                                , githubAccessToken = RemoteData.NotAsked
                                , maybeDevice = Nothing
                                }
                            , getViewport
                            )

                        Just actualCode ->
                            ( Loading
                                { session = session
                                , maybeRoute = Just route
                                , githubAccessToken = RemoteData.Loading
                                , maybeDevice = Nothing
                                }
                            , Cmd.batch
                                [ getViewport
                                , requestGithubAccessToken actualCode
                                ]
                            )

        _ ->
            ( RequestingClientId data, Cmd.none )


getViewport : Cmd Msg
getViewport =
    let
        toMsg { viewport } =
            ChangedDevice <|
                Element.classifyDevice
                    { width = floor viewport.width
                    , height = floor viewport.height
                    }
    in
    Task.perform toMsg Browser.Dom.getViewport


requestGithubAccessToken : String -> Cmd Msg
requestGithubAccessToken code =
    Http.post
        { url = Url.Builder.absolute [ "access_token" ] [ Url.Builder.string "code" code ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (ReceivedGithubAccessToken << RemoteData.fromResult)
                (Decode.field "access_token" Decode.string)
        }



---- UPDATE LOADING


updateLoading : Msg -> LoadingData -> ( Model, Cmd Msg )
updateLoading msg data =
    case msg of
        ReceivedGithubAccessToken githubAccessToken ->
            { data | githubAccessToken = githubAccessToken }
                |> checkLoaded

        ChangedDevice device ->
            { data | maybeDevice = Just device }
                |> checkLoaded

        _ ->
            ( Loading data, Cmd.none )


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
                    ( NotFound session
                    , Cmd.none
                    )

                Just route ->
                    changePageTo session route

        session =
            case maybeGithubAccessToken of
                Nothing ->
                    data.session

                Just githubAccessToken ->
                    Session.toGithubUser githubAccessToken data.session
    in
    ( Loaded
        { device = device
        , page = page
        }
    , Cmd.batch
        [ cmd
        , case data.maybeRoute of
            Nothing ->
                Cmd.none

            Just route ->
                Route.replaceUrl (Session.navKey data.session) route
        ]
    )



---- UPDATE LOADED


updateLoaded : Msg -> LoadedData -> ( Model, Cmd Msg )
updateLoaded msg data =
    case ( msg, data.page ) of
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        key =
                            Session.navKey (toSession data.page)
                    in
                    ( Loaded data
                    , Browser.Navigation.pushUrl key url.path
                    )

                Browser.External externalUrl ->
                    ( Loaded data
                    , Browser.Navigation.load externalUrl
                    )

        ( UrlChanged url, _ ) ->
            let
                session =
                    toSession data.page
            in
            case Route.fromUrl url of
                Nothing ->
                    ( Loaded { data | page = NotFound session }
                    , Cmd.none
                    )

                Just route ->
                    let
                        ( page, cmd ) =
                            changePageTo session route
                    in
                    ( Loaded { data | page = page }
                    , cmd
                    )

        -- CLIENT ID
        ( ReceivedClientId _, _ ) ->
            ( Loaded data, Cmd.none )

        -- TOKENS
        ( ReceivedGithubAccessToken _, _ ) ->
            ( Loaded data, Cmd.none )

        ( ChangedDevice device, _ ) ->
            if data.device /= device then
                ( Loaded { data | device = device }
                , Cmd.none
                )

            else
                ( Loaded data, Cmd.none )

        -- PAGES
        ( _, NotFound _ ) ->
            ( Loaded data, Cmd.none )

        ( PatternsMsg patternsMsg, Patterns patternsModel ) ->
            let
                ( newPatternsModel, patternsCmd ) =
                    Patterns.update patternsMsg patternsModel
            in
            ( Loaded { data | page = Patterns newPatternsModel }
            , Cmd.map PatternsMsg patternsCmd
            )

        ( PatternsMsg _, _ ) ->
            ( Loaded data, Cmd.none )

        ( PatternNewMsg newMsg, PatternNew newModel ) ->
            let
                ( newNewModel, newCmd ) =
                    PatternNew.update newMsg newModel
            in
            ( Loaded { data | page = PatternNew newNewModel }
            , Cmd.map PatternNewMsg newCmd
            )

        ( PatternNewMsg _, _ ) ->
            ( Loaded data, Cmd.none )

        ( PatternMsg patternMsg, Pattern patternModel ) ->
            let
                ( newPatternModel, patternCmd ) =
                    Pattern.update data.device patternMsg patternModel
            in
            ( Loaded { data | page = Pattern newPatternModel }
            , Cmd.map PatternMsg patternCmd
            )

        ( PatternMsg _, _ ) ->
            ( Loaded data, Cmd.none )

        ( DetailsMsg detailsMsg, Details detailsModel ) ->
            let
                ( newDetailsModel, detailsCmd ) =
                    Details.update data.device detailsMsg detailsModel
            in
            ( Loaded { data | page = Details newDetailsModel }
            , Cmd.map DetailsMsg detailsCmd
            )

        ( DetailsMsg _, _ ) ->
            ( Loaded data, Cmd.none )



---- CHANGE PAGE TO


changePageTo : Session -> Route -> ( Page, Cmd Msg )
changePageTo session route =
    case route of
        Route.Patterns ->
            let
                ( patterns, patternsCmd ) =
                    Patterns.init session
            in
            ( Patterns patterns
            , Cmd.map PatternsMsg patternsCmd
            )

        Route.Pattern address ->
            let
                ( pattern, patternCmd ) =
                    Pattern.init session address
            in
            ( Pattern pattern
            , Cmd.map PatternMsg patternCmd
            )

        Route.PatternNew newParameters ->
            let
                ( new, newCmd ) =
                    PatternNew.init session newParameters
            in
            ( PatternNew new
            , Cmd.map PatternNewMsg newCmd
            )

        Route.Details address ->
            let
                ( details, detailsCmd ) =
                    Details.init session address
            in
            ( Details details
            , Cmd.map DetailsMsg detailsCmd
            )



---- SUBSCRIPTIONS


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
                    NotFound _ ->
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
