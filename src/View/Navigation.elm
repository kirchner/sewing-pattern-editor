module View.Navigation exposing (link)

import Element exposing (Element)
import Element.Font as Font
import View.Design as Design


link : { url : String, label : String } -> Element msg
link { url, label } =
    Element.link []
        { url = url
        , label =
            Element.el
                [ Design.fontNormal
                , Font.underline
                , Font.color Design.primary
                , Element.mouseOver
                    [ Font.color Design.primaryDark ]
                ]
                (Element.text label)
        }
