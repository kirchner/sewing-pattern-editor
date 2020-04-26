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

import Api
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Page.Details as Details
import Page.Pattern as Pattern
import Page.PatternNew as PatternNew
import Page.Patterns as Patterns
import Page.Root as Root
import Pattern exposing (Pattern)
import Route exposing (Route)
import Session exposing (Session)
import Task
import Ui.Theme.Spacing
import Url exposing (Url)
import Viewer exposing (Viewer)


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


type Model
    = Error String
    | Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { key : Browser.Navigation.Key
    , url : Url
    , csrfToken : String
    , maybeDevice : Maybe Element.Device
    , maybeViewer : Maybe (Maybe Viewer)
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
        Root root ->
            Root.toSession root

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
    | Root Root.Model
    | Patterns Patterns.Model
    | PatternNew PatternNew.Model
    | Pattern Pattern.Model
    | Details Details.Model


type alias Flags =
    { csrfToken : String }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Decode.required "csrfToken" Decode.string


init : Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init rawFlags url key =
    case Decode.decodeValue flagsDecoder rawFlags of
        Err decodeError ->
            ( Error (Decode.errorToString decodeError)
            , Cmd.none
            )

        Ok flags ->
            ( Loading
                { key = key
                , url = url
                , csrfToken = flags.csrfToken
                , maybeDevice = Nothing
                , maybeViewer = Nothing
                }
            , Cmd.batch
                [ getViewport
                , Api.getUser ReceivedViewer
                ]
            )



---- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Error error ->
            { title = "Error"
            , body =
                [ viewHelp <|
                    Element.column
                        [ Element.centerX
                        , Element.centerY
                        , Element.width (Element.px 640)
                        , Element.spacing Ui.Theme.Spacing.level4
                        ]
                        [ Element.text "Something went very wrong:"
                        , Element.el
                            [ Font.family
                                [ Font.monospace
                                ]
                            , Font.size 14
                            ]
                            (Element.text error)
                        ]
                ]
            }

        Loading _ ->
            { title = "Initializing..."
            , body =
                [ viewHelp <|
                    Element.el [ Element.centerX, Element.centerY ] <|
                        Element.text "Initializing..."
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

                Root rootModel ->
                    let
                        { title, body } =
                            Root.view rootModel
                    in
                    { title = title
                    , body = [ viewHelp (Element.map RootMsg body) ]
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
      -- LOADING
    | ChangedDevice Element.Device
    | ReceivedViewer (Result Http.Error Viewer)
      -- PAGES
    | RootMsg Root.Msg
    | PatternsMsg Patterns.Msg
    | PatternNewMsg PatternNew.Msg
    | PatternMsg Pattern.Msg
    | DetailsMsg Details.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Error _ ->
            ( model, Cmd.none )

        Loading data ->
            updateLoading msg data

        Loaded data ->
            updateLoaded msg data


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



---- UPDATE LOADING


updateLoading : Msg -> LoadingData -> ( Model, Cmd Msg )
updateLoading msg data =
    case msg of
        ChangedDevice device ->
            { data | maybeDevice = Just device }
                |> checkLoaded

        ReceivedViewer result ->
            case result of
                Err _ ->
                    { data | maybeViewer = Just Nothing }
                        |> checkLoaded

                Ok viewer ->
                    { data | maybeViewer = Just (Just viewer) }
                        |> checkLoaded

        _ ->
            ( Loading data, Cmd.none )


checkLoaded : LoadingData -> ( Model, Cmd Msg )
checkLoaded data =
    case ( data.maybeDevice, data.maybeViewer ) of
        ( Just device, Just viewer ) ->
            initLoaded data device viewer

        _ ->
            ( Loading data
            , Cmd.none
            )


initLoaded : LoadingData -> Element.Device -> Maybe Viewer -> ( Model, Cmd Msg )
initLoaded data device viewer =
    let
        ( page, cmd ) =
            case Route.fromUrl data.url of
                Nothing ->
                    ( NotFound session
                    , Cmd.none
                    )

                Just route ->
                    changePageTo session route

        session =
            Session.fromViewer
                { key = data.key
                , csrfToken = data.csrfToken
                }
                viewer
    in
    ( Loaded
        { device = device
        , page = page
        }
    , cmd
    )



---- UPDATE LOADED


updateLoaded : Msg -> LoadedData -> ( Model, Cmd Msg )
updateLoaded msg data =
    case ( msg, data.page ) of
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( Loaded data
                    , Browser.Navigation.pushUrl (Session.key (toSession data.page)) url.path
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

        ( ChangedDevice device, _ ) ->
            if data.device /= device then
                ( Loaded { data | device = device }
                , Cmd.none
                )

            else
                ( Loaded data, Cmd.none )

        ( ReceivedViewer _, _ ) ->
            ( Loaded data, Cmd.none )

        -- PAGES
        ( _, NotFound _ ) ->
            ( Loaded data, Cmd.none )

        ( RootMsg rootMsg, Root rootModel ) ->
            let
                ( newRootModel, rootCmd ) =
                    Root.update rootMsg rootModel
            in
            ( Loaded { data | page = Root newRootModel }
            , Cmd.map RootMsg rootCmd
            )

        ( RootMsg _, _ ) ->
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
        Route.Root ->
            case Session.viewer session of
                Nothing ->
                    let
                        ( root, rootCmd ) =
                            Root.init session
                    in
                    ( Root root
                    , Cmd.map RootMsg rootCmd
                    )

                Just _ ->
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
        Error _ ->
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
                    Root rootModel ->
                        Sub.map RootMsg (Root.subscriptions rootModel)

                    Patterns patternsModel ->
                        Sub.map PatternsMsg (Patterns.subscriptions patternsModel)

                    PatternNew newModel ->
                        Sub.map PatternNewMsg (PatternNew.subscriptions newModel)

                    Pattern patternModel ->
                        Sub.map PatternMsg (Pattern.subscriptions patternModel)

                    Details detailsModel ->
                        Sub.map DetailsMsg (Details.subscriptions detailsModel)
                ]
