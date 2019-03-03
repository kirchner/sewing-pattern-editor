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
import Design
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
import Page.Home as Home
import Pattern exposing (Pattern)
import Ports
import Route exposing (Route)
import StoredPattern exposing (StoredPattern)
import Url exposing (Url)
import View.Input
import View.Modal


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
    { prefix : String
    , key : Navigation.Key
    , page : Page
    , newWorkerModal : Bool
    }



---- PAGE


type Page
    = NotFound
      -- PAGES
    | Home Home.Model
    | Editor Editor.Model


init : {} -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url)
        { prefix = Route.prefixFromUrl url
        , key = key
        , page = NotFound
        , newWorkerModal = False
        }



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
                { title, body, dialog } =
                    Home.view model.prefix homeModel
            in
            { title = title
            , body =
                [ viewHelp (Element.map HomeMsg body) <|
                    if model.newWorkerModal then
                        Just viewNewWorkerDialog

                    else
                        Maybe.map (Element.map HomeMsg) dialog
                ]
            }

        Editor editorModel ->
            let
                { title, body, dialog } =
                    Editor.view model.prefix editorModel
            in
            { title = title
            , body =
                [ viewHelp (Element.map EditorMsg body) <|
                    if model.newWorkerModal then
                        Just viewNewWorkerDialog

                    else
                        Maybe.map (Element.map EditorMsg) dialog
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
                { name = "Roboto"
                , url = "https://fonts.googleapis.com/css?family=Roboto"
                }
            , Font.sansSerif
            ]
        , Element.inFront (Maybe.withDefault Element.none dialog)
        ]
        body


viewNewWorkerDialog : Element Msg
viewNewWorkerDialog =
    View.Modal.small
        { onCancelPress = NewWorkerDialogCancelPressed
        , title = "New version available"
        , content =
            Element.el
                [ Element.spacing Design.small
                , Element.htmlAttribute (Html.Attributes.id "dialog--body")
                , Element.width Element.fill
                , Element.padding Design.small
                , Background.color Design.white
                ]
                (Element.paragraph []
                    [ Element.text "A new version is available. You have to reload to activate it."
                    ]
                )
        , actions =
            [ View.Input.btnPrimary
                { onPress = Just NewWorkerDialogReloadPressed
                , label = "Reload"
                }
            , Element.el [ Element.alignRight ] <|
                View.Input.btnCancel
                    { onPress = Just NewWorkerDialogCancelPressed
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
    | HomeMsg Home.Msg
    | EditorMsg Editor.Msg
      -- SERVICE WORKER
    | OnNewWorker ()
    | NewWorkerDialogCancelPressed
    | NewWorkerDialogReloadPressed


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

        ( HomeMsg homeMsg, Home homeModel ) ->
            let
                ( newHomeModel, homeCmd ) =
                    Home.update model.prefix model.key homeMsg homeModel
            in
            ( { model | page = Home newHomeModel }
            , Cmd.map HomeMsg homeCmd
            )

        ( HomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditorMsg patternMsg, Editor editorModel ) ->
            let
                ( newEditorModel, patternCmd ) =
                    Editor.update model.key patternMsg editorModel
            in
            ( { model | page = Editor newEditorModel }
            , Cmd.map EditorMsg patternCmd
            )

        ( EditorMsg _, _ ) ->
            ( model, Cmd.none )

        -- SERVICE WORKER
        ( OnNewWorker _, _ ) ->
            ( { model | newWorkerModal = True }
            , Cmd.none
            )

        ( NewWorkerDialogCancelPressed, _ ) ->
            ( { model | newWorkerModal = False }
            , Cmd.none
            )

        ( NewWorkerDialogReloadPressed, _ ) ->
            ( { model | newWorkerModal = False }
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
                        Route.Home ->
                            let
                                ( home, homeCmd ) =
                                    Home.init
                            in
                            ( { model | page = Home home }
                            , Cmd.map HomeMsg homeCmd
                            )

                        Route.Editor patternSlug maybePoint ->
                            let
                                ( editor, editorCmd ) =
                                    Editor.init patternSlug
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
        , case model.page of
            NotFound ->
                Sub.none

            -- PAGES
            Home homeModel ->
                Sub.map HomeMsg (Home.subscriptions homeModel)

            Editor editorModel ->
                Sub.map EditorMsg (Editor.subscriptions editorModel)
        ]
