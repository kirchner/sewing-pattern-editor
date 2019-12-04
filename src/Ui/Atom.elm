module Ui.Atom exposing
    ( LinkConfig, link
    , fa, faBody, faLarge, faBrandLarge
    , iconPoint, iconAxis, iconCircle, iconCurve, iconDetail
    , withFocusOutline
    , withFocusOutlineTop, withFocusOutlineBottom, withFocusOutlineLeft, withFocusOutlineRight
    )

{-|


# Links

@docs LinkConfig, link


# Icons

@docs fa, faBody, faLarge, faBrandLarge
@docs iconPoint, iconAxis, iconCircle, iconCurve, iconDetail

@docs withFocusOutline
@docs withFocusOutlineTop, withFocusOutlineBottom, withFocusOutlineLeft, withFocusOutlineRight

-}

import Circle2d
import Element exposing (Attr, Attribute, Decoration, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Label)
import Geometry.Svg as Svg
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import LineSegment2d
import List.Extra as List
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Pixels exposing (pixels)
import Point2d
import Polygon2d
import QuadraticSpline2d
import Svg
import Svg.Attributes
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Typography



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



---- ICONS


{-| -}
fa : String -> Element msg
fa name =
    faHelp "fas" 14 name


{-| -}
faBody : String -> Element msg
faBody name =
    faHelp "fas" 16 name


{-| -}
faLarge : String -> Element msg
faLarge name =
    faHelp "fas" 24 name


{-| -}
faBrandLarge : String -> Element msg
faBrandLarge name =
    faHelp "fab" 24 name


faHelp : String -> Int -> String -> Element msg
faHelp class size name =
    let
        sizePx =
            String.fromInt size ++ "px"
    in
    Element.el
        [ Element.centerX
        , Element.centerY
        ]
        (Element.el [] <|
            Element.html <|
                Html.i
                    [ Html.Attributes.class class
                    , Html.Attributes.class ("fa-" ++ name)
                    , Html.Attributes.style "font-size" sizePx
                    , Html.Attributes.style "width" sizePx
                    , Html.Attributes.style "height" sizePx
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "color" "inherit"
                    ]
                    []
        )


{-| -}
iconPoint : Element msg
iconPoint =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.circle2d
                    [ Svg.Attributes.fill "black"
                    ]
                    (Circle2d.withRadius
                        (pixels 2)
                        Point2d.origin
                    )
                ]


{-| -}
iconAxis : Element msg
iconAxis =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.lineSegment2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "3"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from
                        (Point2d.pixels -5 0)
                        (Point2d.pixels 5 0)
                    )
                ]


{-| -}
iconCircle : Element msg
iconCircle =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.circle2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (Circle2d.withRadius
                        (pixels 5)
                        Point2d.origin
                    )
                ]


{-| -}
iconCurve : Element msg
iconCurve =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.quadraticSpline2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (QuadraticSpline2d.fromControlPoints
                        (Point2d.pixels -5 5)
                        (Point2d.pixels 5 5)
                        (Point2d.pixels 5 -5)
                    )
                ]


{-| -}
iconDetail : Element msg
iconDetail =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.polygon2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke "black"
                    ]
                    (Polygon2d.singleLoop
                        [ Point2d.pixels -5 5
                        , Point2d.pixels 5 5
                        , Point2d.pixels 5 -5
                        , Point2d.pixels -5 -3
                        ]
                    )
                ]



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
