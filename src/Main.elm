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
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Page.Editor as Editor
import Page.Patterns as Patterns
import Pattern exposing (Pattern)
import Ports
import Route exposing (Route)
import Ui.Atom
import Ui.Atom.Input
import Ui.Molecule.Modal
import Ui.Theme.Color
import Ui.Theme.Spacing
import Url exposing (Url)


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


type alias Model =
    { key : Navigation.Key
    , domain : String
    , page : Page
    , newWorkerModal : Maybe Ui.Molecule.Modal.State
    }



---- PAGE


type Page
    = NotFound
      -- PAGES
    | Patterns Patterns.Model
    | Editor Editor.Model


init : {} -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url)
        { key = key
        , domain =
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
        , page = NotFound
        , newWorkerModal = Nothing
        }



---- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "Sewing pattern editor"
            , body = [ Html.text "We are sorry, this page does not exist." ]
            }

        Patterns patternsModel ->
            let
                { title, body, dialog } =
                    Patterns.view patternsModel
            in
            { title = title
            , body =
                [ viewHelp (Element.map PatternsMsg body) <|
                    case model.newWorkerModal of
                        Nothing ->
                            Maybe.map (Element.map PatternsMsg) dialog

                        Just state ->
                            Just (viewNewWorkerDialog state)
                ]
            }

        Editor editorModel ->
            let
                { title, body, dialog } =
                    Editor.view editorModel
            in
            { title = title
            , body =
                [ viewHelp (Element.map EditorMsg body) <|
                    case model.newWorkerModal of
                        Nothing ->
                            Maybe.map (Element.map EditorMsg) dialog

                        Just state ->
                            Just (viewNewWorkerDialog state)
                ]
            }


viewHelp : Element msg -> Maybe (Element msg) -> Html msg
viewHelp body dialog =
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
            [ Font.external
                { name = "Rubik"
                , url = "https://fonts.googleapis.com/css?family=Rubik:300"
                }
            , Font.sansSerif
            ]
        , Element.inFront (Maybe.withDefault Element.none dialog)
        ]
        body


viewNewWorkerDialog : Ui.Molecule.Modal.State -> Element Msg
viewNewWorkerDialog state =
    Ui.Molecule.Modal.small state
        { onCancelPress = NewWorkerDialogCancelPressed
        , onClosed = ModalClosed
        , title = "New version available"
        , content =
            Element.el
                [ Element.spacing Ui.Theme.Spacing.level2
                , Element.htmlAttribute (Html.Attributes.id "dialog--body")
                , Element.width Element.fill
                , Element.padding Ui.Theme.Spacing.level2
                , Background.color Ui.Theme.Color.white
                ]
                (Element.paragraph []
                    [ Element.text "A new version is available. You have to reload to activate it."
                    ]
                )
        , actions =
            [ Ui.Atom.Input.btnPrimary
                { id = "new-worker-modal__reload-btn"
                , onPress = Just NewWorkerDialogReloadPressed
                , label = "Reload"
                }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.Input.btnCancel
                    { id = "new-worker-modal__cancel-btn"
                    , onPress = Just NewWorkerDialogCancelPressed
                    , label = "Cancel"
                    }
            ]
        }



---- UPDATE


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
      -- PAGES
    | PatternsMsg Patterns.Msg
    | EditorMsg Editor.Msg
      -- SERVICE WORKER
    | OnNewWorker ()
    | NewWorkerDialogCancelPressed
    | NewWorkerDialogReloadPressed
    | ModalStateChanged Ui.Molecule.Modal.State
    | ModalClosed


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

        -- PAGES
        ( _, NotFound ) ->
            ( model, Cmd.none )

        ( PatternsMsg patternsMsg, Patterns patternsModel ) ->
            let
                ( newPatternsModel, patternsCmd ) =
                    Patterns.update model.key patternsMsg patternsModel
            in
            ( { model | page = Patterns newPatternsModel }
            , Cmd.map PatternsMsg patternsCmd
            )

        ( PatternsMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditorMsg patternMsg, Editor editorModel ) ->
            let
                ( newEditorModel, patternCmd ) =
                    Editor.update model.key model.domain patternMsg editorModel
            in
            ( { model | page = Editor newEditorModel }
            , Cmd.map EditorMsg patternCmd
            )

        ( EditorMsg _, _ ) ->
            ( model, Cmd.none )

        -- SERVICE WORKER
        ( OnNewWorker _, _ ) ->
            ( { model | newWorkerModal = Just Ui.Molecule.Modal.Opening }
            , Cmd.none
            )

        ( NewWorkerDialogCancelPressed, _ ) ->
            ( { model | newWorkerModal = Just Ui.Molecule.Modal.Closing }
            , Cmd.none
            )

        ( NewWorkerDialogReloadPressed, _ ) ->
            ( { model | newWorkerModal = Just Ui.Molecule.Modal.Closing }
            , Cmd.none
            )

        ( ModalStateChanged newState, _ ) ->
            ( { model | newWorkerModal = Just newState }
            , Cmd.none
            )

        ( ModalClosed, _ ) ->
            ( { model | newWorkerModal = Nothing }
            , Navigation.reload
            )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        ( newModel, cmd ) =
            case maybeRoute of
                Nothing ->
                    ( { model | page = NotFound }
                    , Cmd.none
                    )

                Just newRoute ->
                    case newRoute of
                        Route.Patterns ->
                            let
                                ( patterns, patternsCmd ) =
                                    Patterns.init
                            in
                            ( { model | page = Patterns patterns }
                            , Cmd.map PatternsMsg patternsCmd
                            )

                        Route.GitHub repo ref maybeCode ->
                            let
                                ( editor, editorCmd ) =
                                    Editor.init repo ref maybeCode
                            in
                            ( { model | page = Editor editor }
                            , Cmd.map EditorMsg editorCmd
                            )
    in
    ( newModel
    , cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.onNewWorker OnNewWorker
        , case model.newWorkerModal of
            Nothing ->
                Sub.none

            Just state ->
                Sub.map ModalStateChanged (Ui.Molecule.Modal.subscriptions state)
        , case model.page of
            NotFound ->
                Sub.none

            -- PAGES
            Patterns patternsModel ->
                Sub.map PatternsMsg (Patterns.subscriptions patternsModel)

            Editor editorModel ->
                Sub.map EditorMsg (Editor.subscriptions editorModel)
        ]
