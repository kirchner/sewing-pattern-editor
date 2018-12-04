module View.Modal exposing (small)

import Design
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import View.Icon
import View.Input


small :
    { onCancelPress : msg
    , title : String
    , content : Element msg
    , actions : List (Element msg)
    }
    -> Element msg
small { onCancelPress, title, content, actions } =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Design.grayDark
        ]
        (Element.column
            [ Element.centerX
            , Element.centerY
            , Element.width (Element.px 400)
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
                    (Element.text title)
                , Element.el [ Element.alignRight ] <|
                    View.Input.btnIcon
                        { onPress = Just onCancelPress
                        , icon = "times"
                        }
                ]
            , Element.el
                [ Element.width Element.fill
                , Element.padding Design.small
                ]
                content
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
                actions
            ]
        )
