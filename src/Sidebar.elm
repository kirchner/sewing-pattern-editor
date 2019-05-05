module Sidebar exposing
    ( Page(..)
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

import Design
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Route
import View.Icon


type Page
    = Patterns
    | Measurements
    | Persons


pageUrl : Page -> String
pageUrl page =
    case page of
        Patterns ->
            Route.toString Route.Patterns

        Measurements ->
            Route.toString Route.Measurements

        Persons ->
            Route.toString Route.Persons


pageIcon : Page -> String
pageIcon page =
    case page of
        Patterns ->
            "tshirt"

        Measurements ->
            "ruler"

        Persons ->
            "users"


pageLabel : Page -> String
pageLabel page =
    case page of
        Patterns ->
            "Patterns"

        Measurements ->
            "Measurements"

        Persons ->
            "Persons"


view : { headerHeight : Int, currentPage : Page } -> Element msg
view { headerHeight, currentPage } =
    Element.column
        [ Element.height Element.fill ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height (Element.px headerHeight)
            , Background.color Design.primary
            ]
            Element.none
        , viewNavigation currentPage
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color Design.primary
            ]
            Element.none
        ]


viewNavigation : Page -> Element msg
viewNavigation currentPage =
    let
        viewEntry page =
            Element.link
                [ Element.width Element.fill
                , Element.paddingEach
                    { top = Design.small
                    , bottom = Design.small
                    , left = Design.normal
                    , right = Design.large
                    }
                , Font.color <|
                    if page == currentPage then
                        Design.primaryDark

                    else
                        Design.white
                , Background.color <|
                    if page == currentPage then
                        Design.white

                    else
                        Design.primary
                , Element.mouseOver
                    [ Background.color <|
                        if page == currentPage then
                            Design.white

                        else
                            Design.primaryDark
                    ]
                ]
                { url = pageUrl page
                , label =
                    Element.row
                        [ Element.spacing Design.small
                        , Font.size 16
                        ]
                        [ View.Icon.fa (pageIcon page)
                        , Element.text (pageLabel page)
                        ]
                }
    in
    Element.column
        []
        [ viewEntry Patterns
        , viewEntry Measurements
        , viewEntry Persons
        ]
