module Ui.Atom exposing
    ( LinkConfig, link
    , withFocusOutline
    , withFocusOutlineTop, withFocusOutlineBottom, withFocusOutlineLeft, withFocusOutlineRight
    )

{-|


# Links

@docs LinkConfig, link

@docs withFocusOutline
@docs withFocusOutlineTop, withFocusOutlineBottom, withFocusOutlineLeft, withFocusOutlineRight

-}

import Element exposing (Attr, Attribute, Decoration, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Label)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- LINKS


{-| -}
type alias LinkConfig msg =
    { id : String
    , onPress : Maybe msg
    , label : String
    }


{-| -}
link : LinkConfig msg -> Element msg
link { id, onPress, label } =
    Input.button
        [ attributeId id
        , Font.underline
        , Font.color Ui.Theme.Color.primary
        , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
        , Element.htmlAttribute <|
            Html.Attributes.style "transition" "color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label = Element.text label
        }



---- WITH FOCUS OUTLINE


{-| -}
withFocusOutline : Element msg -> Element msg
withFocusOutline element =
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
withFocusOutlineTop : Element msg -> Element msg
withFocusOutlineTop element =
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
withFocusOutlineBottom : Element msg -> Element msg
withFocusOutlineBottom element =
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
withFocusOutlineLeft : Element msg -> Element msg
withFocusOutlineLeft element =
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
withFocusOutlineRight : Element msg -> Element msg
withFocusOutlineRight element =
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


focusShadow : Attr never msg
focusShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Theme.Color.primary
        }


dangerShadow : Attr never msg
dangerShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Theme.Color.danger
        }


sansSerif : Attribute msg
sansSerif =
    Font.family
        [ Font.external
            { name = "Rubik"
            , url = "https://fonts.googleapis.com/css?family=Rubik:300"
            }
        , Font.sansSerif
        ]


attributeId : String -> Attribute msg
attributeId id =
    Element.htmlAttribute (Html.Attributes.id id)


borderWidthAppended : Attribute msg
borderWidthAppended =
    Border.widthEach
        { top = 0
        , bottom = 1
        , left = 1
        , right = 1
        }


borderRoundAppended : Attribute msg
borderRoundAppended =
    Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = 3
        , bottomRight = 3
        }


backgroundColorEaseInOut : Attribute msg
backgroundColorEaseInOut =
    Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")


fontColorEaseInOut : Attribute msg
fontColorEaseInOut =
    Element.htmlAttribute (Html.Attributes.style "transition" "color 0.2s ease-in-out 0s")
