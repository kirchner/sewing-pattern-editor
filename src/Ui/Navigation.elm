module Ui.Navigation exposing
    ( accordion
    , link
    , newTabLink
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

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Ui.Atom
import Ui.Color
import Ui.Space
import Ui.Typography


link : { url : String, label : String } -> Element msg
link { url, label } =
    Element.link []
        { url = url
        , label =
            Element.el
                [ Font.underline
                , Font.color Ui.Color.primary
                , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
                ]
                (Ui.Typography.body label)
        }


newTabLink : { url : String, label : String } -> Element msg
newTabLink { url, label } =
    Element.newTabLink []
        { url = url
        , label =
            Element.el
                [ Font.underline
                , Font.color Ui.Color.primary
                , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
                ]
                (Ui.Typography.body label)
        }


accordion :
    { onPress : msg
    , label : String
    , open : Bool
    , content : Element msg
    }
    -> Element msg
accordion { onPress, label, open, content } =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Space.level1
        ]
        [ Input.button
            [ Element.width Element.fill
            , Element.padding Ui.Space.level1
            , Font.color Ui.Color.black
            , Border.widthEach
                { left = 0
                , right = 0
                , top = 0
                , bottom = 1
                }
            , Border.color Ui.Color.black
            , Element.mouseOver
                [ Border.color Ui.Color.primaryDark
                , Font.color Ui.Color.primaryDark
                ]
            ]
            { onPress = Just onPress
            , label =
                Element.row
                    [ Element.width Element.fill ]
                    [ Element.el
                        [ Element.width Element.fill
                        , Font.size 16
                        ]
                        (Element.text label)
                    , Element.el
                        [ Element.centerY
                        , Element.centerX
                        ]
                        (if open then
                            Ui.Atom.fa "chevron-up"

                         else
                            Ui.Atom.fa "chevron-down"
                        )
                    ]
            }
        , if open then
            Element.el
                [ Element.width Element.fill
                , Element.padding Ui.Space.level2
                ]
                content

          else
            Element.none
        ]
