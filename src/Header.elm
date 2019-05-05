module Header exposing (view)

import Design
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


view : { headerHeight : Int, label : String, actions : Element msg } -> Element msg
view { headerHeight, label, actions } =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px headerHeight)
        , Element.paddingXY Design.large Design.normal
        , Background.color Design.secondary
        , Border.widthEach
            { top = 0
            , bottom = 2
            , left = 0
            , right = 0
            }
        , Border.color Design.primary
        ]
        [ Element.el
            [ Font.color Design.primaryDark
            , Font.size 36
            ]
            (Element.text label)
        , Element.el
            [ Element.alignRight ]
            actions
        ]
