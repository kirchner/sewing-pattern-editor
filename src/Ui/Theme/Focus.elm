module Ui.Theme.Focus exposing
    ( outline
    , outlineTop, outlineBottom, outlineLeft, outlineRight
    , outlineTopBottom
    )

{-|

@docs outline
@docs outlineTop, outlineBottom, outlineLeft, outlineRight
@docs outlineTopBottom

-}

import Element exposing (Element)
import Element.Border as Border
import Ui.Theme.Color


{-| -}
outline : Element msg -> Element msg
outline element =
    Element.el
        [ Element.width Element.fill
        , Border.width 3
        , Border.rounded 3
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.padding 4
            , Element.width Element.fill
            ]
            element
        )


{-| -}
outlineTop : Element msg -> Element msg
outlineTop element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 0
            , left = 3
            , right = 3
            }
        , Border.roundEach
            { topLeft = 3
            , topRight = 3
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 0
                , left = 4
                , right = 4
                }
            , Element.width Element.fill
            ]
            element
        )


{-| -}
outlineBottom : Element msg -> Element msg
outlineBottom element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 0
            , bottom = 3
            , left = 3
            , right = 3
            }
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 3
            , bottomRight = 3
            }
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 0
                , bottom = 4
                , left = 4
                , right = 4
                }
            , Element.width Element.fill
            ]
            element
        )


{-| -}
outlineLeft : Element msg -> Element msg
outlineLeft element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 3
            , left = 3
            , right = 0
            }
        , Border.roundEach
            { topLeft = 3
            , topRight = 0
            , bottomLeft = 3
            , bottomRight = 0
            }
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 4
                , left = 4
                , right = 0
                }
            , Element.width Element.fill
            ]
            element
        )


{-| -}
outlineRight : Element msg -> Element msg
outlineRight element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 3
            , left = 0
            , right = 3
            }
        , Border.roundEach
            { topLeft = 0
            , topRight = 3
            , bottomLeft = 0
            , bottomRight = 3
            }
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 4
                , left = 0
                , right = 4
                }
            , Element.width Element.fill
            ]
            element
        )


{-| -}
outlineTopBottom : Element msg -> Element msg
outlineTopBottom element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 3
            , left = 0
            , right = 0
            }
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 4
                , left = 0
                , right = 0
                }
            , Element.width Element.fill
            ]
            element
        )
