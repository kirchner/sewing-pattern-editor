module Ui.Pattern exposing
    ( Point, PointInfo(..)
    , Intersectable(..)
    , Axis, AxisInfo(..)
    , PointConfig, drawPoint
    , AxisConfig, drawAxis
    )

{-|

@docs Point, PointInfo
@docs Intersectable
@docs Axis, AxisInfo

@docs PointConfig, drawPoint
@docs AxisConfig, drawAxis

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


type alias Resolution =
    Quantity Float (Rate Pixels Meters)


type alias Point coordinates =
    { point2d : Point2d Meters coordinates
    , info : Maybe (PointInfo coordinates)
    }


type PointInfo coordinates
    = Origin
    | FromOnePoint
        { basePoint : Point coordinates
        , label : String
        }
    | BetweenTwoPoints
        { basePointA : Point coordinates
        , basePointB : Point coordinates
        , label : String
        }
    | Intersection
        { intersectableA : Intersectable coordinates
        , intersectableB : Intersectable coordinates
        }


type Intersectable coordinates
    = IntersectableAxis (Axis coordinates)
    | IntersectableCircle (Circle coordinates)


type alias Axis coordinates =
    { axis2d : Axis2d Meters coordinates
    , info : Maybe (AxisInfo coordinates)
    }


type AxisInfo coordinates
    = ThroughOnePoint
        { point : Point coordinates
        }
    | ThroughTwoPoints
        { pointA : Point coordinates
        , pointB : Point coordinates
        }


type alias Circle coordinates =
    { circle2d : Circle2d Meters coordinates
    , info : Maybe (CircleInfo coordinates)
    }


type CircleInfo coordinates
    = WithRadius
        { centerPoint : Point coordinates
        , label : String
        }
    | ThroughThreePoints
        { pointA : Point coordinates
        , pointB : Point coordinates
        , pointC : Point coordinates
        }



---- POINT


type alias PointConfig =
    { focused : Bool
    , hovered : Bool
    , name : String
    }


drawPoint : Resolution -> PointConfig -> Point coordinates -> Svg msg
drawPoint resolution cfg point =
    let
        point2d =
            Point2d.at resolution point.point2d
    in
    Svg.g []
        [ if cfg.focused || cfg.hovered then
            pointLabel point2d cfg.name

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            pointInfo resolution cfg point

          else
            Svg.text ""
        , if cfg.focused then
            focusOutline point2d

          else
            Svg.text ""
        , actualPoint cfg.hovered point2d
        ]


pointLabel : Point2d Pixels coordinates -> String -> Svg msg
pointLabel point2d label =
    let
        labelPosition =
            point2d
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


pointInfo : Resolution -> PointConfig -> Point coordinates -> Svg msg
pointInfo resolution cfg point =
    let
        point2d =
            Point2d.at resolution point.point2d
    in
    case point.info of
        Nothing ->
            Svg.text ""

        Just Origin ->
            Svg.text ""

        Just (FromOnePoint info) ->
            let
                basePoint2d =
                    Point2d.at resolution info.basePoint.point2d

                offsetBasePoint2d =
                    case Direction2d.from basePoint2d point2d of
                        Nothing ->
                            basePoint2d

                        Just direction ->
                            basePoint2d
                                |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                offsetPoint2d =
                    case Direction2d.from point2d basePoint2d of
                        Nothing ->
                            point2d

                        Just direction ->
                            point2d
                                |> Point2d.translateBy
                                    (Vector2d.withLength (pixels offsetPointFactor) direction)

                offsetPointFactor =
                    if cfg.focused then
                        9.5

                    else
                        5

                length =
                    Vector2d.from offsetPoint2d offsetBasePoint2d
                        |> Vector2d.length
                        |> Pixels.inPixels

                color =
                    toColor <|
                        if cfg.hovered then
                            Ui.Color.primary

                        else
                            Ui.Color.black
            in
            Svg.g []
                [ Svg.circle2d
                    [ Svg.Attributes.fill color ]
                    (Circle2d.withRadius (pixels 3) basePoint2d)
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.strokeWidth <|
                        if cfg.focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray length 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPoint2d offsetBasePoint2d)
                , lineLabel cfg.hovered basePoint2d point2d info.label
                ]

        Just (BetweenTwoPoints info) ->
            let
                -- A
                basePointA2d =
                    Point2d.at resolution info.basePointA.point2d

                offsetBasePointA2d =
                    case Direction2d.from basePointA2d point2d of
                        Nothing ->
                            basePointA2d

                        Just direction ->
                            basePointA2d
                                |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                offsetPointA2d =
                    case Direction2d.from point2d basePointA2d of
                        Nothing ->
                            point2d

                        Just direction ->
                            point2d
                                |> Point2d.translateBy
                                    (Vector2d.withLength (pixels offsetPointFactor) direction)

                lengthA =
                    Vector2d.from offsetPointA2d offsetBasePointA2d
                        |> Vector2d.length
                        |> Pixels.inPixels

                -- B
                basePointB2d =
                    Point2d.at resolution info.basePointB.point2d

                offsetBasePointB2d =
                    case Direction2d.from basePointB2d point2d of
                        Nothing ->
                            basePointB2d

                        Just direction ->
                            basePointB2d
                                |> Point2d.translateBy (Vector2d.withLength (pixels 3) direction)

                offsetPointB2d =
                    case Direction2d.from point2d basePointB2d of
                        Nothing ->
                            point2d

                        Just direction ->
                            point2d
                                |> Point2d.translateBy
                                    (Vector2d.withLength (pixels offsetPointFactor) direction)

                lengthB =
                    Vector2d.from offsetPointB2d offsetBasePointB2d
                        |> Vector2d.length
                        |> Pixels.inPixels

                offsetPointFactor =
                    if cfg.focused then
                        9.5

                    else
                        5

                color =
                    toColor <|
                        if cfg.hovered then
                            Ui.Color.primary

                        else
                            Ui.Color.black
            in
            Svg.g []
                [ Svg.circle2d
                    [ Svg.Attributes.fill color ]
                    (Circle2d.withRadius (pixels 3) basePointA2d)
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.strokeWidth <|
                        if cfg.focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray lengthA 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointA2d offsetBasePointA2d)
                , lineLabel cfg.hovered basePointA2d point2d info.label
                , Svg.circle2d
                    [ Svg.Attributes.fill color ]
                    (Circle2d.withRadius (pixels 3) basePointB2d)
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.strokeWidth <|
                        if cfg.focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray lengthB 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointB2d offsetBasePointB2d)
                ]

        Just (Intersection info) ->
            let
                drawAxisHelp { axis2d } =
                    Svg.lineSegment2d
                        [ Svg.Attributes.stroke <|
                            if cfg.hovered then
                                toColor Ui.Color.primary

                            else
                                toColor Ui.Color.black
                        , Svg.Attributes.strokeWidth <|
                            if cfg.focused then
                                "1.5"

                            else
                                "1"
                        ]
                        (LineSegment2d.along (Axis2d.at resolution axis2d)
                            (pixels -1000)
                            (pixels 1000)
                        )
            in
            Svg.g []
                [ case info.intersectableA of
                    IntersectableAxis axisA ->
                        drawAxisHelp axisA

                    IntersectableCircle _ ->
                        Svg.text ""
                , case info.intersectableB of
                    IntersectableAxis axisB ->
                        drawAxisHelp axisB

                    IntersectableCircle _ ->
                        Svg.text ""
                , if cfg.focused then
                    Svg.circle2d
                        [ Svg.Attributes.fill (toColor Ui.Color.white) ]
                        (Circle2d.withRadius (pixels 8) point2d)

                  else
                    Svg.text ""
                ]


lineLabel : Bool -> Point2d Pixels coordinates -> Point2d Pixels coordinates -> String -> Svg msg
lineLabel hovered basePoint point label =
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
         , Svg.Attributes.fill <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
         ]
            |> addTransform
        )
        [ Svg.text label ]


focusOutline : Point2d Pixels coordinates -> Svg msg
focusOutline point2d =
    Svg.circle2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke (toColor Ui.Color.primary)
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeDasharray "4 7"
        , Svg.Attributes.strokeLinecap "round"
        ]
        (Circle2d.withRadius (pixels 9) point2d)


actualPoint : Bool -> Point2d Pixels coordinates -> Svg msg
actualPoint hovered point2d =
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
        (Circle2d.withRadius (pixels 5) point2d)



---- AXIS


type alias AxisConfig =
    { focused : Bool
    , hovered : Bool
    , name : String
    }


drawAxis : Resolution -> AxisConfig -> Axis coordinates -> Svg msg
drawAxis resolution cfg axis =
    let
        axis2d =
            Axis2d.at resolution axis.axis2d
    in
    Svg.g []
        [ if cfg.focused || cfg.hovered then
            axisLabel axis2d cfg.name

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            axisInfo resolution cfg axis

          else
            Svg.text ""
        , if cfg.focused then
            axisFocusOutline axis2d

          else
            Svg.text ""
        , actualAxis cfg.hovered axis2d
        ]


axisLabel : Axis2d Pixels coordinates -> String -> Svg msg
axisLabel axis2d label =
    let
        labelPosition =
            axis2d
                |> Axis2d.originPoint
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


axisInfo : Resolution -> AxisConfig -> Axis coordinates -> Svg msg
axisInfo resolution cfg axis =
    case axis.info of
        Nothing ->
            Svg.text ""

        Just (ThroughOnePoint info) ->
            let
                point2d =
                    Point2d.at resolution info.point.point2d
            in
            Svg.circle2d
                [ Svg.Attributes.fill <|
                    if cfg.hovered then
                        toColor Ui.Color.primary

                    else
                        toColor Ui.Color.black
                ]
                (Circle2d.withRadius (pixels 3) point2d)

        Just (ThroughTwoPoints info) ->
            Svg.text ""


axisFocusOutline : Axis2d Pixels coordinates -> Svg msg
axisFocusOutline axis2d =
    let
        outline offset =
            Svg.lineSegment2d
                [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.strokeDasharray "4 7"
                , Svg.Attributes.strokeLinecap "round"
                ]
                (LineSegment2d.along
                    (Axis2d.translateBy
                        (Direction2d.perpendicularTo (Axis2d.direction axis2d)
                            |> Direction2d.toVector
                            |> Vector2d.at (pixels offset |> Quantity.per (Quantity.float 1))
                        )
                        axis2d
                    )
                    (pixels -1000)
                    (pixels 1000)
                )
    in
    Svg.g []
        [ outline 5
        , outline -5
        ]


actualAxis : Bool -> Axis2d Pixels coordinates -> Svg msg
actualAxis hovered axis2d =
    Svg.lineSegment2d
        [ Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
        , Svg.Attributes.strokeWidth "1"
        ]
        (LineSegment2d.along axis2d
            (pixels -1000)
            (pixels 1000)
        )



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
