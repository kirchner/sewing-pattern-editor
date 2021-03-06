module Header exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Ui.Theme.Color
import Ui.Theme.Spacing


view : { headerHeight : Int, label : String, actions : Element msg } -> Element msg
view { headerHeight, label, actions } =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px headerHeight)
        , Element.paddingXY Ui.Theme.Spacing.level4 Ui.Theme.Spacing.level3
        , Background.color Ui.Theme.Color.secondary
        , Border.widthEach
            { top = 0
            , bottom = 2
            , left = 0
            , right = 0
            }
        , Border.color Ui.Theme.Color.primary
        ]
        [ Element.el
            [ Font.color Ui.Theme.Color.primaryDark
            , Font.size 36
            ]
            (Element.text label)
        , Element.el
            [ Element.alignRight ]
            actions
        ]
