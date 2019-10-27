module Ui.Modal exposing
    ( State(..)
    , small
    , subscriptions
    , wide
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

import Browser.Events
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Ui.Atom
import Ui.Color
import Ui.Space
import Ui.Typography


type State
    = Opening
    | Open
    | Closing


subscriptions : State -> Sub State
subscriptions state =
    case state of
        Opening ->
            Browser.Events.onAnimationFrame (\_ -> Open)

        Open ->
            Sub.none

        Closing ->
            Sub.none



---- VIEWS


type alias ViewConfig msg =
    { onCancelPress : msg
    , onClosed : msg
    , title : String
    , content : Element msg
    , actions : List (Element msg)
    }


wide : State -> ViewConfig msg -> Element msg
wide =
    custom (20 * Ui.Space.level8)


small : State -> ViewConfig msg -> Element msg
small =
    custom (10 * Ui.Space.level8)


custom : Int -> State -> ViewConfig msg -> Element msg
custom width state config =
    let
        backdropAttrs attrs =
            case state of
                Opening ->
                    style "opacity" "0"
                        :: attrs

                Open ->
                    style "opacity" "1"
                        :: style "transition" "opacity 0.3s"
                        :: attrs

                Closing ->
                    style "opacity" "0"
                        :: style "transition" "opacity 0.1s"
                        :: onTransitionEnd config.onClosed
                        :: attrs

        modalAttrs attrs =
            case state of
                Opening ->
                    Element.moveDown 0
                        :: attrs

                Open ->
                    Element.moveDown (2 * Ui.Space.level8)
                        :: style "transition" "transform 0.3s"
                        :: attrs

                Closing ->
                    Element.moveDown (2 * Ui.Space.level8)
                        :: attrs
    in
    Element.el
        ([ Element.width Element.fill
         , Element.height Element.fill
         , Background.color Ui.Color.grayDark
         ]
            |> backdropAttrs
        )
        (Element.column
            ([ Element.centerX
             , Element.alignTop
             , Element.width (Element.px width)
             , Border.width 1
             , Border.rounded 4
             , Border.color Ui.Color.black
             , Background.color Ui.Color.white
             , attribute "role" "dialog"
             , attribute "aria-modal" "true"
             , attribute "aria-labelledby" "dialog--title"
             , attribute "aria-describedby" "dialog--body"

             -- TODO: fix display error in Chromium with scale-factor=1.5
             , style "padding-left" "1px"
             , style "padding-right" "1px"
             ]
                |> modalAttrs
            )
            [ Element.row
                [ Element.width Element.fill
                , Element.padding Ui.Space.level2
                , Background.color Ui.Color.secondary
                , Border.roundEach
                    { topLeft = 4
                    , topRight = 4
                    , bottomLeft = 0
                    , bottomRight = 0
                    }
                ]
                [ Element.el
                    [ attributeId "dialog--title"
                    , Element.centerX
                    ]
                    (Ui.Typography.bodyBold config.title)
                , Element.el [ Element.alignRight ] <|
                    Ui.Atom.btnIcon
                        { id = "modal-cancel-btn"
                        , onPress = Just config.onCancelPress
                        , icon = "times"
                        }
                ]
            , Element.el
                [ Element.width Element.fill
                , Element.padding Ui.Space.level3
                ]
                config.content
            , Element.row
                [ Element.width Element.fill
                , Element.padding Ui.Space.level2
                , Border.widthEach
                    { top = 1
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                , Border.color Ui.Color.secondary
                ]
                config.actions
            ]
        )


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute (Html.Attributes.style name value)


attribute : String -> String -> Element.Attribute msg
attribute name value =
    Element.htmlAttribute (Html.Attributes.attribute name value)


attributeId : String -> Element.Attribute msg
attributeId id =
    Element.htmlAttribute (Html.Attributes.id id)


onTransitionEnd : msg -> Element.Attribute msg
onTransitionEnd msg =
    Element.htmlAttribute (Html.Events.on "transitionend" (Decode.succeed msg))
