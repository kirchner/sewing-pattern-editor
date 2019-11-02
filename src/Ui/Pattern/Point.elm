module Ui.Pattern.Point exposing (Config, draw)

{-|

@docs Config, draw

-}

import Circle2d
import Element exposing (Color)
import Geometry.Svg as Svg
import Length exposing (Meters)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Svg exposing (Svg)
import Svg.Attributes
import Ui.Color
import Vector2d


type alias Config =
    { focused : Bool
    , hovered : Bool
    , label : String
    }


draw : Quantity Float (Rate Pixels Meters) -> Config -> Point2d Meters coordinates -> Svg msg
draw resolution cfg point =
    let
        labelPosition =
            Point2d.at resolution point
                |> Point2d.translateBy (Vector2d.pixels 17 -13)
                |> Point2d.toPixels
    in
    Svg.g []
        [ if cfg.focused then
            Svg.circle2d
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke (toColor Ui.Color.primary)
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.strokeDasharray "4 2"
                ]
                (Circle2d.withRadius (pixels 8) (Point2d.at resolution point))

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            Svg.text_
                [ Svg.Attributes.x (String.fromFloat labelPosition.x)
                , Svg.Attributes.y (String.fromFloat labelPosition.y)
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.style <|
                    "font-size: "
                        ++ String.fromFloat 14
                        ++ "px; font-family: \"Rubik\";"
                , Svg.Attributes.fill (toColor Ui.Color.primary)
                ]
                [ Svg.text cfg.label ]

          else
            Svg.text ""
        , Svg.circle2d
            [ Svg.Attributes.fill <|
                if cfg.hovered then
                    toColor Ui.Color.primary

                else
                    "none"
            , Svg.Attributes.stroke <|
                if cfg.hovered then
                    toColor Ui.Color.primary

                else
                    toColor Ui.Color.black
            , Svg.Attributes.strokeWidth "1"
            ]
            (Circle2d.withRadius (pixels 5) (Point2d.at resolution point))
        ]


toColor : Color -> String
toColor color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color
    in
    String.concat
        [ "rgba("
        , String.fromInt (floor (255 * red))
        , ","
        , String.fromInt (floor (255 * green))
        , ","
        , String.fromInt (floor (255 * blue))
        , ","
        , String.fromFloat alpha
        , ")"
        ]
