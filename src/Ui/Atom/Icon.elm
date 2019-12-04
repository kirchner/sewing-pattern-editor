module Ui.Atom.Icon exposing
    ( fa, faBody, faLarge, faBrandLarge
    , point, axis, circle, curve, detail
    )

{-|


# Icons

@docs fa, faBody, faLarge, faBrandLarge
@docs point, axis, circle, curve, detail

-}

import Circle2d
import Element exposing (Element)
import Geometry.Svg as Svg
import Html
import Html.Attributes
import LineSegment2d
import Pixels exposing (pixels)
import Point2d
import Polygon2d
import QuadraticSpline2d
import Svg
import Svg.Attributes


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
point : Element msg
point =
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
axis : Element msg
axis =
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
circle : Element msg
circle =
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
curve : Element msg
curve =
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
detail : Element msg
detail =
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
