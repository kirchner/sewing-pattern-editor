module View.Icon exposing
    ( dev
    , fa
    , faLarge
    , faMedium
    )

import Css
import Element
import Html.Styled as Html
import Html.Styled.Attributes as Attributes


dev name =
    Element.html <|
        Html.toUnstyled <|
            Html.i
                [ Attributes.class ("devicon-" ++ name)
                , Attributes.css
                    [ Css.fontSize (Css.px 24)
                    , Css.color Css.inherit
                    ]
                ]
                []


fa name =
    Element.el [] <|
        Element.html <|
            Html.toUnstyled <|
                Html.i
                    [ Attributes.class "fas"
                    , Attributes.class ("fa-" ++ name)
                    , Attributes.css
                        [ Css.fontSize (Css.px 12)
                        , Css.color Css.inherit
                        ]
                    ]
                    []


faMedium name =
    Element.html <|
        Html.toUnstyled <|
            Html.i
                [ Attributes.class "fas"
                , Attributes.class ("fa-" ++ name)
                , Attributes.css
                    [ Css.fontSize (Css.px 18)
                    , Css.color Css.inherit
                    ]
                ]
                []


faLarge name =
    Element.html <|
        Html.toUnstyled <|
            Html.i
                [ Attributes.class "fas"
                , Attributes.class ("fa-" ++ name)
                , Attributes.css
                    [ Css.fontSize (Css.px 24)
                    , Css.color Css.inherit
                    ]
                ]
                []
