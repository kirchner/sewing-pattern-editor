module Header exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Ui.Color
import Ui.Space


view : { headerHeight : Int, label : String, actions : Element msg } -> Element msg
view { headerHeight, label, actions } =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px headerHeight)
        , Element.paddingXY Ui.Space.level4 Ui.Space.level3
        , Background.color Ui.Color.secondary
        , Border.widthEach
            { top = 0
            , bottom = 2
            , left = 0
            , right = 0
            }
        , Border.color Ui.Color.primary
        ]
        [ Element.el
            [ Font.color Ui.Color.primaryDark
            , Font.size 36
            ]
            (Element.text label)
        , Element.el
            [ Element.alignRight ]
            actions
        ]
