module Page.Persons exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

{-
   Sewing pattern editor
   Copyright (C) 2019  Fabian Kirchner <kirchner@posteo.de>

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

import Browser.Navigation
import Element exposing (Element)
import Element.Background as Background
import Header
import Sidebar
import Ui.Theme.Color
import Ui.Theme.Spacing


type alias Model =
    { dialog : Dialog
    }


type Dialog
    = NoDialog


init : ( Model, Cmd Msg )
init =
    ( { dialog = NoDialog }
    , Cmd.none
    )


type Msg
    = NoOp


update : Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


headerHeight : Int
headerHeight =
    2 * Ui.Theme.Spacing.level8


view : Model -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view model =
    { title = "Persons"
    , body =
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Sidebar.view
                { headerHeight = headerHeight
                , currentPage = Sidebar.Persons
                }
            , viewBody
            ]
    , dialog = Nothing
    }


viewBody : Element msg
viewBody =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Header.view
            { headerHeight = headerHeight
            , label = "Persons"
            , actions = Element.none
            }
        , Element.el
            [ Element.height Element.fill
            , Element.width Element.fill
            ]
            Element.none
        , Element.el
            [ Element.width Element.fill
            , Element.height (Element.px Ui.Theme.Spacing.level1)
            , Background.color Ui.Theme.Color.primary
            ]
            Element.none
        ]
