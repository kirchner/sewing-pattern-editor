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
import Html
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Page.Editor as Editor
import Page.Home as Home
import Pattern exposing (Pattern)
import Route exposing (Route)
import StoredPattern exposing (StoredPattern)
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
    { prefix : String
    , key : Navigation.Key
    , page : Page
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
            { title = "Sewing pattern editor"
            , body =
                [ Html.map HomeMsg <|
                    Home.view model.prefix homeModel
                ]
            }

        Editor editorModel ->
            let
                { title, body } =
                    Editor.view model.prefix editorModel
            in
            { title = title
            , body = List.map (Html.map EditorMsg) body
            }



---- UPDATE


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
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
    case model.page of
        NotFound ->
            Sub.none

        -- PAGES
        Home homeModel ->
            Sub.map HomeMsg (Home.subscriptions homeModel)

        Editor editorModel ->
            Sub.map EditorMsg (Editor.subscriptions editorModel)
