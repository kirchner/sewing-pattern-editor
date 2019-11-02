module Ui.Pattern.Point exposing (Config, Info(..), draw)

{-|

@docs Config, Info, draw

-}

import Angle
import Circle2d
import Direction2d
import Element exposing (Color)
import Geometry.Svg as Svg
import Length exposing (Meters)
import LineSegment2d
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Ui.Color
import Vector2d


type alias Config coordinates =
    { focused : Bool
    , hovered : Bool
    , label : String
    , point : Point2d Meters coordinates
    , info : Info coordinates
    }


type Info coordinates
    = Origin
    | FromOnePoint
        { basePoint : Point2d Meters coordinates
        , distance : String
        }


draw : Quantity Float (Rate Pixels Meters) -> Config coordinates -> Svg msg
draw resolution cfg =
    let
        labelPosition =
            point
                |> Point2d.translateBy (Vector2d.pixels 17 -13)
                |> Point2d.toPixels

        point =
            Point2d.at resolution cfg.point
    in
    Svg.g []
        [ if cfg.focused then
            Svg.circle2d
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke (toColor Ui.Color.primary)
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.strokeDasharray "4 6"
                , Svg.Attributes.strokeLinecap "round"
                ]
                (Circle2d.withRadius (pixels 8) point)

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
        , if cfg.focused then
            case cfg.info of
                Origin ->
                    Svg.text ""

                FromOnePoint info ->
                    let
                        basePoint =
                            Point2d.at resolution info.basePoint

                        offsetBasePoint =
                            case Direction2d.from basePoint point of
                                Nothing ->
                                    basePoint

                                Just direction ->
                                    basePoint
                                        |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                        offsetPoint =
                            case Direction2d.from point basePoint of
                                Nothing ->
                                    point

                                Just direction ->
                                    point
                                        |> Point2d.translateBy (Vector2d.withLength (pixels 8.5) direction)

                        length =
                            Vector2d.from offsetPoint offsetBasePoint
                                |> Vector2d.length
                                |> Pixels.inPixels

                        distancePosition =
                            basePoint
                                |> Point2d.translateBy
                                    (Vector2d.from basePoint point
                                        |> Vector2d.scaleBy 0.5
                                    )
                                |> Point2d.toPixels

                        distanceTransforms attrs =
                            Vector2d.from basePoint point
                                |> Vector2d.direction
                                |> Maybe.map (Direction2d.toAngle >> Angle.inDegrees)
                                |> Maybe.map
                                    (\angle ->
                                        Svg.Attributes.transform
                                            (String.concat
                                                [ "rotate("
                                                , String.fromFloat angle
                                                , " "
                                                , String.fromFloat distancePosition.x
                                                , " "
                                                , String.fromFloat distancePosition.y
                                                , ")"
                                                ]
                                            )
                                            :: attrs
                                    )
                                |> Maybe.withDefault attrs
                    in
                    Svg.g []
                        [ Svg.circle2d
                            [ Svg.Attributes.fill (toColor Ui.Color.primary) ]
                            (Circle2d.withRadius (pixels 3) basePoint)
                        , Svg.lineSegment2d
                            [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                            , Svg.Attributes.strokeWidth "1.5"
                            , strokeDasharray length 8 10
                            , Svg.Attributes.strokeLinecap "round"
                            ]
                            (LineSegment2d.from offsetPoint offsetBasePoint)
                        , Svg.text_
                            ([ Svg.Attributes.x (String.fromFloat distancePosition.x)
                             , Svg.Attributes.y (String.fromFloat distancePosition.y)
                             , Svg.Attributes.dy (String.fromFloat -10)
                             , Svg.Attributes.textAnchor "middle"
                             , Svg.Attributes.style <|
                                "font-size: "
                                    ++ String.fromFloat 14
                                    ++ "px; font-family: \"Rubik\";"
                             , Svg.Attributes.fill (toColor Ui.Color.primary)
                             ]
                                |> distanceTransforms
                            )
                            [ Svg.text info.distance ]
                        ]

          else if cfg.hovered then
            case cfg.info of
                Origin ->
                    Svg.text ""

                FromOnePoint info ->
                    let
                        basePoint =
                            Point2d.at resolution info.basePoint

                        offsetBasePoint =
                            case Direction2d.from basePoint point of
                                Nothing ->
                                    basePoint

                                Just direction ->
                                    basePoint
                                        |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                        offsetPoint =
                            case Direction2d.from point basePoint of
                                Nothing ->
                                    point

                                Just direction ->
                                    point
                                        |> Point2d.translateBy (Vector2d.withLength (pixels 5) direction)

                        length =
                            Vector2d.from offsetPoint offsetBasePoint
                                |> Vector2d.length
                                |> Pixels.inPixels

                        distancePosition =
                            basePoint
                                |> Point2d.translateBy
                                    (Vector2d.from basePoint point
                                        |> Vector2d.scaleBy 0.5
                                    )
                                |> Point2d.toPixels

                        distanceTransforms attrs =
                            Vector2d.from basePoint point
                                |> Vector2d.direction
                                |> Maybe.map (Direction2d.toAngle >> Angle.inDegrees)
                                |> Maybe.map
                                    (\angle ->
                                        Svg.Attributes.transform
                                            (String.concat
                                                [ "rotate("
                                                , String.fromFloat angle
                                                , " "
                                                , String.fromFloat distancePosition.x
                                                , " "
                                                , String.fromFloat distancePosition.y
                                                , ")"
                                                ]
                                            )
                                            :: attrs
                                    )
                                |> Maybe.withDefault attrs
                    in
                    Svg.g []
                        [ Svg.circle2d
                            [ Svg.Attributes.fill (toColor Ui.Color.primary) ]
                            (Circle2d.withRadius (pixels 3) basePoint)
                        , Svg.lineSegment2d
                            [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                            , Svg.Attributes.strokeWidth "1"
                            , strokeDasharray length 8 10
                            , Svg.Attributes.strokeLinecap "round"
                            ]
                            (LineSegment2d.from offsetPoint offsetBasePoint)
                        , Svg.text_
                            ([ Svg.Attributes.x (String.fromFloat distancePosition.x)
                             , Svg.Attributes.y (String.fromFloat distancePosition.y)
                             , Svg.Attributes.dy (String.fromFloat -10)
                             , Svg.Attributes.textAnchor "middle"
                             , Svg.Attributes.style <|
                                "font-size: "
                                    ++ String.fromFloat 14
                                    ++ "px; font-family: \"Rubik\";"
                             , Svg.Attributes.fill (toColor Ui.Color.primary)
                             ]
                                |> distanceTransforms
                            )
                            [ Svg.text info.distance ]
                        ]

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
            (Circle2d.withRadius (pixels 5) point)
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


strokeDasharray : Float -> Int -> Int -> Attribute msg
strokeDasharray length strokeLength strokeGap =
    let
        ---- DASH ARRAY
        actualStrokeGap =
            toFloat strokeGap + lengthRest / toFloat strokeCount

        lengthRest =
            length - 8 - toFloat strokeCount * toFloat (strokeLength + strokeGap)

        strokeCount =
            floor (length - toFloat strokeLength) // (strokeLength + strokeGap)
    in
    Svg.Attributes.strokeDasharray ("8 " ++ String.fromFloat actualStrokeGap)
