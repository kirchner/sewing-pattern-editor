module View.Modal exposing
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
import Design
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import View.Icon
import View.Input


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


wide :
    State
    ->
        { onCancelPress : msg
        , onClosed : msg
        , title : String
        , content : Element msg
        , actions : List (Element msg)
        }
    -> Element msg
wide =
    custom 600


small :
    State
    ->
        { onCancelPress : msg
        , onClosed : msg
        , title : String
        , content : Element msg
        , actions : List (Element msg)
        }
    -> Element msg
small =
    custom 500


custom :
    Int
    -> State
    ->
        { onCancelPress : msg
        , onClosed : msg
        , title : String
        , content : Element msg
        , actions : List (Element msg)
        }
    -> Element msg
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
                    Element.moveDown (Design.large + Design.normal)
                        :: style "transition" "transform 0.3s"
                        :: attrs

                Closing ->
                    Element.moveDown (Design.large + Design.normal)
                        :: attrs
    in
    Element.el
        ([ Element.width Element.fill
         , Element.height Element.fill
         , Background.color Design.grayDark
         ]
            |> backdropAttrs
        )
        (Element.column
            ([ Element.centerX
             , Element.alignTop
             , Element.width (Element.px width)
             , Border.width 1
             , Border.rounded 4
             , Border.color Design.black
             , Design.fontNormal
             , Background.color Design.white
             , Element.htmlAttribute (Html.Attributes.attribute "role" "dialog")
             , Element.htmlAttribute (Html.Attributes.attribute "aria-modal" "true")
             , Element.htmlAttribute <|
                Html.Attributes.attribute "aria-labelledby" "dialog--title"
             , Element.htmlAttribute <|
                Html.Attributes.attribute "aria-describedby" "dialog--body"

             -- TODO: fix display error in Chromium with scale-factor=1.5
             , Element.htmlAttribute (Html.Attributes.style "padding-left" "1px")
             , Element.htmlAttribute (Html.Attributes.style "padding-right" "1px")
             ]
                |> modalAttrs
            )
            [ Element.row
                [ Element.width Element.fill
                , Element.padding Design.small
                , Background.color Design.secondary
                , Border.roundEach
                    { topLeft = 4
                    , topRight = 4
                    , bottomLeft = 0
                    , bottomRight = 0
                    }
                ]
                [ Element.el
                    [ Element.htmlAttribute (Html.Attributes.id "dialog--title")
                    , Element.centerX
                    , Font.bold
                    ]
                    (Element.text config.title)
                , Element.el [ Element.alignRight ] <|
                    View.Input.btnIcon
                        { onPress = Just config.onCancelPress
                        , icon = "times"
                        }
                ]
            , Element.el
                [ Element.width Element.fill
                , Element.padding Design.small
                ]
                config.content
            , Element.row
                [ Element.width Element.fill
                , Element.padding Design.small
                , Border.widthEach
                    { top = 1
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                , Border.color Design.secondary
                ]
                config.actions
            ]
        )


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute <|
        Html.Attributes.style name value


onTransitionEnd : msg -> Element.Attribute msg
onTransitionEnd msg =
    Element.htmlAttribute <|
        Html.Events.on "transitionend" (Decode.succeed msg)
