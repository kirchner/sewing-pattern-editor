module View.Icon exposing
    ( fa
    , faBrandLarge
    , faLarge
    , faMedium
    )

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

import Element
import Html
import Html.Attributes as Attributes


fa name =
    Element.el [] <|
        Element.html <|
            Html.i
                [ Attributes.class "fas"
                , Attributes.class ("fa-" ++ name)
                , Attributes.style "font-size" "12px"
                , Attributes.style "color" "inherit"
                ]
                []


faMedium name =
    Element.el [] <|
        Element.html <|
            Html.i
                [ Attributes.class "fas"
                , Attributes.class ("fa-" ++ name)
                , Attributes.style "font-size" "18px"
                , Attributes.style "color" "inherit"
                ]
                []


faLarge name =
    Element.el [] <|
        Element.html <|
            Html.i
                [ Attributes.class "fas"
                , Attributes.class ("fa-" ++ name)
                , Attributes.style "font-size" "24px"
                , Attributes.style "color" "inherit"
                ]
                []


faBrandLarge name =
    Element.el [] <|
        Element.html <|
            Html.i
                [ Attributes.class "fab"
                , Attributes.class ("fa-" ++ name)
                , Attributes.style "font-size" "24px"
                , Attributes.style "color" "inherit"
                ]
                []
