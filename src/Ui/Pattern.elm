module Ui.Pattern exposing
    ( Point, PointInfo(..)
    , Intersectable(..)
    , Axis, AxisInfo(..)
    , Circle, CircleInfo(..)
    , Curve(..), LineSegmentData, QuadraticSplineData, CubicSplineData
    , LineSegmentInfo, QuadraticSplineInfo, CubicSplineInfo
    , PointConfig, drawPoint
    , AxisConfig, drawAxis
    , CircleConfig, drawCircle
    , CurveConfig, drawCurve
    )

{-|

@docs Point, PointInfo
@docs Intersectable
@docs Axis, AxisInfo
@docs Circle, CircleInfo
@docs Curve, LineSegmentData, QuadraticSplineData, CubicSplineData
@docs LineSegmentInfo, QuadraticSplineInfo, CubicSplineInfo

@docs PointConfig, drawPoint
@docs AxisConfig, drawAxis
@docs CircleConfig, drawCircle
@docs CurveConfig, drawCurve

-}

import Angle
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d
import Element exposing (Color)
import Geometry.Svg as Svg
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity, Rate)
import Rectangle2d
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


type Curve coordinates
    = LineSegment (LineSegmentData coordinates)
    | QuadraticSpline (QuadraticSplineData coordinates)
    | CubicSpline (CubicSplineData coordinates)


type alias LineSegmentData coordinates =
    { lineSegment2d : LineSegment2d Meters coordinates
    , info : Maybe (LineSegmentInfo coordinates)
    }


type alias QuadraticSplineData coordinates =
    { quadraticSpline2d : QuadraticSpline2d Meters coordinates
    , info : Maybe (QuadraticSplineInfo coordinates)
    }


type alias CubicSplineData coordinates =
    { cubicSpline2d : CubicSpline2d Meters coordinates
    , info : Maybe (CubicSplineInfo coordinates)
    }


type alias LineSegmentInfo coordinates =
    { startPoint : Point coordinates
    , endPoint : Point coordinates
    }


type alias QuadraticSplineInfo coordinates =
    { firstControlPoint : Point coordinates
    , secondControlPoint : Point coordinates
    , thirdControlPoint : Point coordinates
    }


type alias CubicSplineInfo coordinates =
    { firstControlPoint : Point coordinates
    , secondControlPoint : Point coordinates
    , thirdControlPoint : Point coordinates
    , fourthControlPoint : Point coordinates
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

        offsetPointFactor =
            if cfg.focused then
                9.5

            else
                5
    in
    Svg.g []
        [ if cfg.focused || cfg.hovered then
            pointLabel point2d cfg.name

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            pointInfo resolution cfg.hovered cfg.focused offsetPointFactor point

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
                |> Point2d.translateBy (Vector2d.pixels 23 19)
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


pointInfo : Resolution -> Bool -> Bool -> Float -> Point coordinates -> Svg msg
pointInfo resolution hovered focused offsetPointFactor point =
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

                length =
                    Vector2d.from offsetPoint2d offsetBasePoint2d
                        |> Vector2d.length
                        |> Pixels.inPixels

                color =
                    toColor <|
                        if hovered then
                            Ui.Color.primary

                        else
                            Ui.Color.black
            in
            Svg.g []
                [ pointInfo resolution hovered focused 3 info.basePoint
                , actualInfoPoint hovered basePoint2d
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.strokeWidth <|
                        if focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray length 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPoint2d offsetBasePoint2d)
                , lineLabel hovered basePoint2d point2d info.label
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

                color =
                    toColor <|
                        if hovered then
                            Ui.Color.primary

                        else
                            Ui.Color.black
            in
            Svg.g []
                [ -- BASE POINT A
                  pointInfo resolution hovered focused 3 info.basePointA
                , actualInfoPoint hovered basePointA2d
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.strokeWidth <|
                        if focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray lengthA 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointA2d offsetBasePointA2d)
                , lineLabel hovered basePointA2d point2d info.label

                -- BASE POINT B
                , pointInfo resolution hovered focused 3 info.basePointB
                , actualInfoPoint hovered basePointB2d
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.strokeWidth <|
                        if focused then
                            "1.5"

                        else
                            "1"
                    , strokeDasharray lengthB 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointB2d offsetBasePointB2d)
                ]

        Just (Intersection info) ->
            Svg.g []
                [ case info.intersectableA of
                    IntersectableAxis axisA ->
                        Svg.g []
                            [ axisInfo resolution hovered axisA
                            , actualAxis hovered focused (Axis2d.at resolution axisA.axis2d)
                            ]

                    IntersectableCircle _ ->
                        Svg.text ""
                , case info.intersectableB of
                    IntersectableAxis axisB ->
                        Svg.g []
                            [ axisInfo resolution hovered axisB
                            , actualAxis hovered focused (Axis2d.at resolution axisB.axis2d)
                            ]

                    IntersectableCircle _ ->
                        Svg.text ""
                , if focused then
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


actualInfoPoint : Bool -> Point2d Pixels coordinates -> Svg msg
actualInfoPoint hovered point2d =
    Svg.circle2d
        [ Svg.Attributes.fill <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
        ]
        (Circle2d.withRadius (pixels 3) point2d)



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
            axisInfo resolution cfg.hovered axis

          else
            Svg.text ""
        , if cfg.focused then
            axisFocusOutline axis2d

          else
            Svg.text ""
        , actualAxis cfg.hovered cfg.focused axis2d
        ]


axisLabel : Axis2d Pixels coordinates -> String -> Svg msg
axisLabel axis2d label =
    let
        labelPosition =
            axis2d
                |> Axis2d.originPoint
                |> Point2d.translateBy (Vector2d.pixels 23 19)
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


axisInfo : Resolution -> Bool -> Axis coordinates -> Svg msg
axisInfo resolution hovered axis =
    case axis.info of
        Nothing ->
            Svg.text ""

        Just (ThroughOnePoint info) ->
            actualInfoPoint hovered (Point2d.at resolution info.point.point2d)

        Just (ThroughTwoPoints info) ->
            Svg.g []
                [ actualInfoPoint hovered (Point2d.at resolution info.pointA.point2d)
                , actualInfoPoint hovered (Point2d.at resolution info.pointB.point2d)
                ]


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


actualAxis : Bool -> Bool -> Axis2d Pixels coordinates -> Svg msg
actualAxis hovered focused axis2d =
    Svg.lineSegment2d
        [ Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
        , Svg.Attributes.strokeWidth <|
            if focused then
                "1.5"

            else
                "1"
        ]
        (LineSegment2d.along axis2d
            (pixels -1000)
            (pixels 1000)
        )



---- CIRCLE


type alias CircleConfig =
    { focused : Bool
    , hovered : Bool
    , name : String
    }


drawCircle : Resolution -> CircleConfig -> Circle coordinates -> Svg msg
drawCircle resolution cfg circle =
    let
        circle2d =
            Circle2d.at resolution circle.circle2d
    in
    Svg.g []
        [ if cfg.focused || cfg.hovered then
            circleLabel circle2d cfg.name

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            circleInfo resolution cfg.hovered cfg.focused circle

          else
            Svg.text ""
        , if cfg.focused then
            circleFocusOutline circle2d

          else
            Svg.text ""
        , actualCircle cfg.hovered cfg.focused circle2d
        ]


circleLabel : Circle2d Pixels coordinates -> String -> Svg msg
circleLabel circle2d label =
    let
        labelPosition =
            circle2d
                |> Circle2d.centerPoint
                |> Point2d.translateBy
                    (Vector2d.rTheta
                        (Circle2d.radius circle2d)
                        (Angle.degrees 30)
                    )
                |> Point2d.translateBy (Vector2d.pixels 23 19)
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


circleInfo : Resolution -> Bool -> Bool -> Circle coordinates -> Svg msg
circleInfo resolution hovered focused circle =
    case circle.info of
        Nothing ->
            Svg.text ""

        Just (WithRadius info) ->
            let
                centerPoint2d =
                    Point2d.at resolution info.centerPoint.point2d
            in
            actualInfoPoint hovered centerPoint2d

        Just (ThroughThreePoints info) ->
            let
                pointA2d =
                    Point2d.at resolution info.pointA.point2d

                pointB2d =
                    Point2d.at resolution info.pointB.point2d

                pointC2d =
                    Point2d.at resolution info.pointC.point2d
            in
            Svg.g []
                [ pointInfo resolution hovered focused 3 info.pointA
                , actualInfoPoint hovered pointA2d
                , pointInfo resolution hovered focused 3 info.pointB
                , actualInfoPoint hovered pointB2d
                , pointInfo resolution hovered focused 3 info.pointC
                , actualInfoPoint hovered pointC2d
                ]


circleFocusOutline : Circle2d Pixels coordinates -> Svg msg
circleFocusOutline circle2d =
    Svg.circle2d
        [ Svg.Attributes.stroke (toColor Ui.Color.primary)
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeDasharray "4 7"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.fill "none"
        ]
        (Circle2d.withRadius
            (Quantity.plus (pixels 5) (Circle2d.radius circle2d))
            (Circle2d.centerPoint circle2d)
        )


actualCircle : Bool -> Bool -> Circle2d Pixels coordinates -> Svg msg
actualCircle hovered focused circle2d =
    Svg.circle2d
        [ Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
        , Svg.Attributes.strokeWidth <|
            if focused then
                "1.5"

            else
                "1"
        , Svg.Attributes.fill "none"
        ]
        circle2d



---- CURVE


type alias CurveConfig =
    { focused : Bool
    , hovered : Bool
    , name : String
    }


drawCurve : Resolution -> CurveConfig -> Curve coordinates -> Svg msg
drawCurve resolution cfg curve =
    case curve of
        LineSegment stuff ->
            drawLineSegment resolution cfg stuff

        QuadraticSpline stuff ->
            drawQuadraticSpline resolution cfg stuff

        CubicSpline stuff ->
            drawCubicSpline resolution cfg stuff


drawLineSegment : Resolution -> CurveConfig -> LineSegmentData coordinates -> Svg msg
drawLineSegment resolution cfg lineSegment =
    let
        lineSegment2d =
            LineSegment2d.at resolution lineSegment.lineSegment2d
    in
    Svg.g []
        [ if cfg.focused || cfg.hovered then
            lineSegmentLabel lineSegment2d cfg.name

          else
            Svg.text ""
        , if cfg.focused || cfg.hovered then
            lineSegmentInfo resolution cfg.hovered lineSegment

          else
            Svg.text ""
        , if cfg.focused then
            lineSegmentFocusOutline lineSegment2d

          else
            Svg.text ""
        , actualLineSegment cfg.hovered lineSegment2d
        ]


lineSegmentLabel : LineSegment2d Pixels coordinates -> String -> Svg msg
lineSegmentLabel lineSegment2d label =
    let
        labelPosition =
            lineSegment2d
                |> LineSegment2d.midpoint
                |> Point2d.translateBy (Vector2d.pixels 23 19)
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


lineSegmentInfo : Resolution -> Bool -> LineSegmentData coordinates -> Svg msg
lineSegmentInfo resolution hovered lineSegment =
    case lineSegment.info of
        Nothing ->
            Svg.text ""

        Just info ->
            Svg.g []
                [ actualInfoPoint hovered (Point2d.at resolution info.startPoint.point2d)
                , actualInfoPoint hovered (Point2d.at resolution info.endPoint.point2d)
                ]


lineSegmentFocusOutline : LineSegment2d Pixels coordinates -> Svg msg
lineSegmentFocusOutline lineSegment2d =
    case LineSegment2d.direction lineSegment2d of
        Nothing ->
            Svg.text ""

        Just direction ->
            Svg.rectangle2d
                [ Svg.Attributes.stroke (toColor Ui.Color.primary)
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.strokeDasharray "4 7"
                , Svg.Attributes.strokeLinecap "round"
                , Svg.Attributes.fill "none"
                ]
                (Rectangle2d.withDimensions
                    ( Quantity.plus (pixels 10) (LineSegment2d.length lineSegment2d)
                    , pixels 10
                    )
                    (Direction2d.toAngle direction)
                    (LineSegment2d.midpoint lineSegment2d)
                )


actualLineSegment : Bool -> LineSegment2d Pixels coordinates -> Svg msg
actualLineSegment hovered lineSegment2d =
    Svg.lineSegment2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Color.primary

            else
                toColor Ui.Color.black
        , Svg.Attributes.strokeWidth "1"
        ]
        lineSegment2d


drawQuadraticSpline : Resolution -> CurveConfig -> QuadraticSplineData coordinates -> Svg msg
drawQuadraticSpline resolution cfg quadraticSpline =
    Svg.g [] []


drawCubicSpline : Resolution -> CurveConfig -> CubicSplineData coordinates -> Svg msg
drawCubicSpline resolution cfg cubicSpline =
    Svg.g [] []



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
