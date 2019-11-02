module Ui.Pattern.Point exposing
    ( Config, Info(..), draw
    , Object2d(..)
    )

{-|

@docs Config, Info, draw
@docs Object2d

-}

import Angle
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
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
        , label : String
        }
    | BetweenTwoPoints
        { basePointA : Point2d Meters coordinates
        , basePointB : Point2d Meters coordinates
        , label : String
        }
    | Intersection
        { objectA : Object2d Meters coordinates
        , objectB : Object2d Meters coordinates
        }


type Object2d units coordinates
    = Axis2d (Axis2d units coordinates)
    | Circle2d (Circle2d units coordinates)


type alias Resolution =
    Quantity Float (Rate Pixels Meters)


draw : Resolution -> Config coordinates -> Svg msg
draw resolution cfg =
    let
        point =
            Point2d.at resolution cfg.point
    in
    Svg.g []
        [ if cfg.focused || cfg.hovered then
            pointLabel point cfg.label

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            pointInfo resolution cfg

          else
            Svg.text ""
        , if cfg.focused then
            focusOutline point

          else
            Svg.text ""
        , actualPoint cfg.hovered point
        ]


focusOutline : Point2d Pixels coordinates -> Svg msg
focusOutline point =
    Svg.circle2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke (toColor Ui.Color.primary)
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeDasharray "4 6"
        , Svg.Attributes.strokeLinecap "round"
        ]
        (Circle2d.withRadius (pixels 8) point)


pointLabel : Point2d Pixels coordinates -> String -> Svg msg
pointLabel point label =
    let
        labelPosition =
            point
                |> Point2d.translateBy (Vector2d.pixels 23 17)
                |> Point2d.toPixels
    in
    Svg.text_
        [ Svg.Attributes.x (String.fromFloat labelPosition.x)
        , Svg.Attributes.y (String.fromFloat labelPosition.y)
        , Svg.Attributes.textAnchor "middle"
        , font
        , Svg.Attributes.fill (toColor Ui.Color.primary)
        ]
        [ Svg.text label ]


pointInfo : Resolution -> Config coordinates -> Svg msg
pointInfo resolution cfg =
    let
        point =
            Point2d.at resolution cfg.point
    in
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
                                |> Point2d.translateBy
                                    (Vector2d.withLength (pixels offsetPointFactor) direction)

                offsetPointFactor =
                    if cfg.focused then
                        8.5

                    else
                        5

                length =
                    Vector2d.from offsetPoint offsetBasePoint
                        |> Vector2d.length
                        |> Pixels.inPixels
            in
            Svg.g []
                [ Svg.circle2d
                    [ Svg.Attributes.fill (toColor Ui.Color.primary) ]
                    (Circle2d.withRadius (pixels 3) basePoint)
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                    , Svg.Attributes.strokeWidth <|
                        if cfg.focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray length 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPoint offsetBasePoint)
                , lineLabel basePoint point info.label
                ]

        BetweenTwoPoints info ->
            let
                -- A
                basePointA =
                    Point2d.at resolution info.basePointA

                offsetBasePointA =
                    case Direction2d.from basePointA point of
                        Nothing ->
                            basePointA

                        Just direction ->
                            basePointA
                                |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                offsetPointA =
                    case Direction2d.from point basePointA of
                        Nothing ->
                            point

                        Just direction ->
                            point
                                |> Point2d.translateBy
                                    (Vector2d.withLength (pixels offsetPointFactor) direction)

                lengthA =
                    Vector2d.from offsetPointA offsetBasePointA
                        |> Vector2d.length
                        |> Pixels.inPixels

                -- B
                basePointB =
                    Point2d.at resolution info.basePointB

                offsetBasePointB =
                    case Direction2d.from basePointB point of
                        Nothing ->
                            basePointB

                        Just direction ->
                            basePointB
                                |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                offsetPointB =
                    case Direction2d.from point basePointB of
                        Nothing ->
                            point

                        Just direction ->
                            point
                                |> Point2d.translateBy
                                    (Vector2d.withLength (pixels offsetPointFactor) direction)

                lengthB =
                    Vector2d.from offsetPointB offsetBasePointB
                        |> Vector2d.length
                        |> Pixels.inPixels

                offsetPointFactor =
                    if cfg.focused then
                        8.5

                    else
                        5
            in
            Svg.g []
                [ Svg.circle2d
                    [ Svg.Attributes.fill (toColor Ui.Color.primary) ]
                    (Circle2d.withRadius (pixels 3) basePointA)
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                    , Svg.Attributes.strokeWidth <|
                        if cfg.focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray lengthA 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointA offsetBasePointA)
                , lineLabel basePointA point info.label
                , Svg.circle2d
                    [ Svg.Attributes.fill (toColor Ui.Color.primary) ]
                    (Circle2d.withRadius (pixels 3) basePointB)
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                    , Svg.Attributes.strokeWidth <|
                        if cfg.focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray lengthB 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointB offsetBasePointB)
                ]

        Intersection info ->
            let
                drawAxis axis =
                    Svg.lineSegment2d
                        [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                        , Svg.Attributes.strokeWidth <|
                            if cfg.focused then
                                "1.5"

                            else
                                "1"
                        ]
                        (LineSegment2d.along (Axis2d.at resolution axis)
                            (pixels -1000)
                            (pixels 1000)
                        )
            in
            Svg.g []
                [ case info.objectA of
                    Axis2d axisA ->
                        drawAxis axisA

                    Circle2d _ ->
                        Svg.text ""
                , case info.objectB of
                    Axis2d axisB ->
                        drawAxis axisB

                    Circle2d _ ->
                        Svg.text ""
                , if cfg.focused then
                    Svg.circle2d
                        [ Svg.Attributes.fill (toColor Ui.Color.white) ]
                        (Circle2d.withRadius (pixels 8) point)

                  else
                    Svg.text ""
                ]


lineLabel : Point2d Pixels coordinates -> Point2d Pixels coordinates -> String -> Svg msg
lineLabel basePoint point label =
    let
        position =
            basePoint
                |> Point2d.translateBy (Vector2d.from basePoint point |> Vector2d.scaleBy 0.5)
                |> Point2d.toPixels

        addTransform attrs =
            Vector2d.from basePoint point
                |> Vector2d.direction
                |> Maybe.map (Direction2d.toAngle >> Angle.inDegrees >> addRotateTransform attrs)
                |> Maybe.withDefault attrs

        addRotateTransform attrs angle =
            Svg.Attributes.transform
                (String.concat
                    [ "rotate("
                    , String.fromFloat (fixAngle angle)
                    , " "
                    , String.fromFloat position.x
                    , " "
                    , String.fromFloat position.y
                    , ")"
                    ]
                )
                :: attrs

        fixAngle angle =
            if 90 < angle then
                angle + 180

            else
                angle
    in
    Svg.text_
        ([ Svg.Attributes.x (String.fromFloat position.x)
         , Svg.Attributes.y (String.fromFloat position.y)
         , Svg.Attributes.dy (String.fromFloat -10)
         , Svg.Attributes.textAnchor "middle"
         , font
         , Svg.Attributes.fill (toColor Ui.Color.primary)
         ]
            |> addTransform
        )
        [ Svg.text label ]


actualPoint : Bool -> Point2d Pixels coordinates -> Svg msg
actualPoint hovered point =
    Svg.circle2d
        [ Svg.Attributes.fill <|
            if hovered then
                toColor Ui.Color.primary

            else
                "none"
        , Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
        , Svg.Attributes.strokeWidth "1"
        ]
        (Circle2d.withRadius (pixels 5) point)



---- HELPER


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


font : Attribute msg
font =
    Svg.Attributes.style <|
        "font-size: "
            ++ String.fromFloat 14
            ++ "px; font-family: \"Rubik\";"
