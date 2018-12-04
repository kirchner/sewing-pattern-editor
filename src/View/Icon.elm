module View.Icon exposing
    ( dev
    , fa
    , faLarge
    , faMedium
    )

import Element
import Html
import Html.Attributes as Attributes


dev name =
    Element.el [] <|
        Element.html <|
            Html.i
                [ Attributes.class ("devicon-" ++ name)
                , Attributes.style "font-size" "24px"
                , Attributes.style "color" "inherit"
                ]
                []


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
