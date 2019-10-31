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


type alias Config =
    { focused : Bool
    , hovered : Bool
    }


draw : Quantity Float (Rate Pixels Meters) -> Config -> Point2d Meters coordinates -> Svg msg
draw resolution cfg point =
    Svg.g []
        [ if cfg.focused then
            Svg.circle2d
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke (toColor Ui.Color.primary)
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.strokeDasharray "2 1"
                ]
                (Circle2d.withRadius (pixels 7) (Point2d.at resolution point))

          else
            Svg.g [] []
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
