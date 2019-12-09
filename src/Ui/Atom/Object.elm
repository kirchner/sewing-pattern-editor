module Ui.Atom.Object exposing
    ( Point, PointInfo(..)
    , Intersectable(..)
    , Axis, AxisInfo(..)
    , Circle, CircleInfo(..)
    , Curve, CurveInfo(..)
    , Detail
    , Config, Resolution
    , drawPoint, drawAxis, drawCircle, drawCurve, drawDetail
    )

{-|

@docs Point, PointInfo
@docs Intersectable
@docs Axis, AxisInfo
@docs Circle, CircleInfo
@docs Curve, CurveInfo
@docs Detail

@docs Config, Resolution
@docs drawPoint, drawAxis, drawCircle, drawCurve, drawDetail

-}

import Angle
import Axis2d exposing (Axis2d)
import BoundingBox2d
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Curve2d exposing (Curve2d)
import Detail2d exposing (Detail2d, LastCurve2d(..), NextCurve2d(..))
import Direction2d
import Element exposing (Color)
import Geometry.Svg as Svg
import Geometry.Svg.Extra as Svg
import Json.Decode as Decode
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity, Rate)
import Rectangle2d
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Events
import Ui.Theme.Color
import Vector2d
import VirtualDom


{-| -}
type alias Config msg =
    { onHover : msg
    , onLeave : msg
    , onSelect : msg
    }


{-| -}
type alias Resolution =
    Quantity Float (Rate Pixels Meters)


type alias Layers msg =
    { inactive : Svg msg
    , active : Svg msg
    , outline : Svg msg
    , events : Svg msg
    }



---- POINT


{-| -}
type alias Point coordinates =
    { info : Maybe (PointInfo coordinates)
    , point2d : Point2d Meters coordinates
    }


{-| -}
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


{-| -}
type Intersectable coordinates
    = IntersectableAxis (Axis coordinates)
    | IntersectableCircle (Circle coordinates)



---- AXIS


{-| -}
type alias Axis coordinates =
    { info : Maybe (AxisInfo coordinates)
    , axis2d : Axis2d Meters coordinates
    }


{-| -}
type AxisInfo coordinates
    = ThroughOnePoint
        { point : Point coordinates
        }
    | ThroughTwoPoints
        { pointA : Point coordinates
        , pointB : Point coordinates
        }



---- CIRCLE


{-| -}
type alias Circle coordinates =
    { info : Maybe (CircleInfo coordinates)
    , circle2d : Circle2d Meters coordinates
    }


{-| -}
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



---- CURVE


{-| -}
type alias Curve coordinates =
    { info : Maybe (CurveInfo coordinates)
    , curve2d : Curve2d Meters coordinates
    }


{-| -}
type CurveInfo coordinates
    = LineSegment
        { startPoint : Point coordinates
        , endPoint : Point coordinates
        }
    | QuadraticSpline
        { firstControlPoint : Point coordinates
        , secondControlPoint : Point coordinates
        , thirdControlPoint : Point coordinates
        }
    | CubicSpline
        { firstControlPoint : Point coordinates
        , secondControlPoint : Point coordinates
        , thirdControlPoint : Point coordinates
        , fourthControlPoint : Point coordinates
        }



---- DETAIL


{-| -}
type alias Detail coordinates =
    { points : List (Point coordinates)
    , curves : List (Curve coordinates)
    , detail2d : Detail2d Meters coordinates
    }



---- POINT


{-| -}
drawPoint : Config msg -> String -> Point coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawPoint cfg name point resolution focused hovered selected =
    let
        point2d =
            Point2d.at resolution point.point2d

        offsetPointFactor =
            if focused then
                9.5

            else
                5
    in
    { inactive =
        if not selected && not hovered then
            pointInactive point2d

        else
            Svg.text ""
    , active =
        if selected then
            Svg.g []
                [ pointLabel name point2d
                , pointSelected point2d
                ]

        else if hovered then
            Svg.g []
                [ pointLabel name point2d
                , pointHovered point2d
                ]

        else
            Svg.text ""
    , outline =
        if focused then
            pointOutline point2d

        else
            Svg.text ""
    , events = pointEvents cfg point2d
    }


pointLabel : String -> Point2d Pixels coordinates -> Svg msg
pointLabel label point2d =
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
        ]
        [ Svg.text label ]


pointInfo : Float -> Point coordinates -> Resolution -> Bool -> Bool -> Svg msg
pointInfo offsetPointFactor point resolution focused hovered =
    let
        point2d =
            Point2d.at resolution point.point2d

        color =
            if hovered then
                Ui.Theme.Color.primary

            else
                Ui.Theme.Color.black

        strokeWidth =
            if focused then
                "1.5"

            else
                "1"
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
            in
            Svg.g []
                [ pointInfo 3 info.basePoint resolution focused hovered
                , pointReferenced basePoint2d hovered
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke (toColor color)
                    , Svg.Attributes.strokeWidth strokeWidth
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
            in
            Svg.g []
                [ -- BASE POINT A
                  pointInfo 3 info.basePointA resolution focused hovered
                , pointReferenced basePointA2d hovered
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke (toColor color)
                    , Svg.Attributes.strokeWidth strokeWidth
                    , strokeDasharray lengthA 8 10
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from offsetPointA2d offsetBasePointA2d)
                , lineLabel hovered basePointA2d point2d info.label

                -- BASE POINT B
                , pointInfo 3 info.basePointB resolution focused hovered
                , pointReferenced basePointB2d hovered
                , Svg.lineSegment2d
                    [ Svg.Attributes.stroke (toColor color)
                    , Svg.Attributes.strokeWidth strokeWidth
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
                            [ axisInfo axisA resolution focused hovered
                            , axisReferenced (Axis2d.at resolution axisA.axis2d) focused hovered
                            ]

                    IntersectableCircle _ ->
                        Svg.text ""
                , case info.intersectableB of
                    IntersectableAxis axisB ->
                        Svg.g []
                            [ axisInfo axisB resolution focused hovered
                            , axisReferenced (Axis2d.at resolution axisB.axis2d) focused hovered
                            ]

                    IntersectableCircle _ ->
                        Svg.text ""
                , if focused then
                    Svg.circle2d
                        [ Svg.Attributes.fill (toColor Ui.Theme.Color.white) ]
                        (Circle2d.withRadius (pixels 8) point2d)

                  else
                    Svg.text ""
                ]


lineLabel : Bool -> Point2d Pixels coordinates -> Point2d Pixels coordinates -> String -> Svg msg
lineLabel hovered basePoint2d point2d label =
    let
        position =
            basePoint2d
                |> Point2d.translateBy (Vector2d.from basePoint2d point2d |> Vector2d.scaleBy 0.5)
                |> Point2d.toPixels

        addTransform attrs =
            Vector2d.from basePoint2d point2d
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
                toColor Ui.Theme.Color.primary

            else
                toColor Ui.Theme.Color.black
         ]
            |> addTransform
        )
        [ Svg.text label ]


pointOutline : Point2d Pixels coordinates -> Svg msg
pointOutline point2d =
    Svg.circle2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
        , Svg.Attributes.strokeWidth "1"
        ]
        (Circle2d.withRadius (pixels 8) point2d)


pointHovered : Point2d Pixels coordinates -> Svg msg
pointHovered point2d =
    Svg.g []
        [ Svg.circle2d
            [ Svg.Attributes.fill (toColor Ui.Theme.Color.secondary) ]
            (Circle2d.withRadius (pixels 9) point2d)
        , Svg.circle2d
            [ Svg.Attributes.fill (toColor Ui.Theme.Color.black) ]
            (Circle2d.withRadius (pixels 3) point2d)
        ]


pointSelected : Point2d Pixels coordinates -> Svg msg
pointSelected point2d =
    Svg.g []
        [ Svg.circle2d
            [ Svg.Attributes.fill (toColor Ui.Theme.Color.primary)
            ]
            (Circle2d.withRadius (pixels 9) point2d)
        , Svg.circle2d
            [ Svg.Attributes.fill (toColor Ui.Theme.Color.white) ]
            (Circle2d.withRadius (pixels 3) point2d)
        ]


pointInactive : Point2d Pixels coordinates -> Svg msg
pointInactive point2d =
    Svg.circle2d
        [ Svg.Attributes.fill (toColor Ui.Theme.Color.black) ]
        (Circle2d.withRadius (pixels 3) point2d)


pointReferenced : Point2d Pixels coordinates -> Bool -> Svg msg
pointReferenced point2d hovered =
    let
        color =
            if hovered then
                Ui.Theme.Color.primary

            else
                Ui.Theme.Color.black
    in
    Svg.circle2d
        [ Svg.Attributes.fill (toColor color) ]
        (Circle2d.withRadius (pixels 3) point2d)


pointEvents : Config msg -> Point2d Pixels coordinates -> Svg msg
pointEvents cfg point2d =
    Svg.circle2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.onClick cfg.onSelect
        ]
        (Circle2d.withRadius (pixels 8) point2d)



---- AXIS


{-| -}
drawAxis : Config msg -> String -> Axis coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawAxis cfg name axis resolution focused hovered selected =
    let
        axis2d =
            Axis2d.at resolution axis.axis2d
    in
    case ( focused, hovered ) of
        ( False, False ) ->
            { inactive = axisReferenced axis2d False False
            , active = Svg.text ""
            , outline = Svg.text ""
            , events = axisEvents cfg axis2d
            }

        ( True, False ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ axisInfo axis resolution focused hovered
                    , axisLabel name axis2d
                    , axisReferenced axis2d True False
                    ]
            , outline = axisOutline axis2d
            , events = axisEvents cfg axis2d
            }

        ( False, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ axisInfo axis resolution focused hovered
                    , axisLabel name axis2d
                    , axisReferenced axis2d False True
                    ]
            , outline = Svg.text ""
            , events = axisEvents cfg axis2d
            }

        ( True, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ axisInfo axis resolution focused hovered
                    , axisLabel name axis2d
                    , axisReferenced axis2d True True
                    ]
            , outline = axisOutline axis2d
            , events = axisEvents cfg axis2d
            }


axisLabel : String -> Axis2d Pixels coordinates -> Svg msg
axisLabel label axis2d =
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
        , Svg.Attributes.fill (toColor Ui.Theme.Color.primary)
        ]
        [ Svg.text label ]


axisInfo : Axis coordinates -> Resolution -> Bool -> Bool -> Svg msg
axisInfo axis resolution focused hovered =
    case axis.info of
        Nothing ->
            Svg.text ""

        Just (ThroughOnePoint info) ->
            Svg.g []
                [ pointInfo 3 info.point resolution focused hovered
                , pointReferenced (Point2d.at resolution info.point.point2d) hovered
                ]

        Just (ThroughTwoPoints info) ->
            Svg.g []
                [ pointInfo 3 info.pointA resolution focused hovered
                , pointInfo 3 info.pointB resolution focused hovered
                , pointReferenced (Point2d.at resolution info.pointA.point2d) hovered
                , pointReferenced (Point2d.at resolution info.pointB.point2d) hovered
                ]


axisOutline : Axis2d Pixels coordinates -> Svg msg
axisOutline axis2d =
    let
        outline offset =
            Svg.lineSegment2d
                [ Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
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
        [ outline 8
        , outline -8
        ]


axisReferenced : Axis2d Pixels coordinates -> Bool -> Bool -> Svg msg
axisReferenced axis2d focused hovered =
    Svg.lineSegment2d
        [ Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Theme.Color.primary

            else
                toColor Ui.Theme.Color.black
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


axisEvents : Config msg -> Axis2d Pixels coordinates -> Svg msg
axisEvents cfg axis2d =
    Svg.lineSegment2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.onClick cfg.onSelect
        ]
        (LineSegment2d.along axis2d
            (pixels -1000)
            (pixels 1000)
        )



---- CIRCLE


{-| -}
drawCircle : Config msg -> String -> Circle coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawCircle cfg name circle resolution focused hovered selected =
    let
        circle2d =
            Circle2d.at resolution circle.circle2d
    in
    case ( focused, hovered ) of
        ( False, False ) ->
            { inactive = circleReferenced circle2d False False
            , active = Svg.text ""
            , outline = Svg.text ""
            , events = circleEvents cfg circle2d
            }

        ( True, False ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ circleInfo circle resolution focused hovered
                    , circleLabel name circle2d
                    , circleReferenced circle2d True False
                    ]
            , outline = circleOutline circle2d
            , events = circleEvents cfg circle2d
            }

        ( False, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ circleInfo circle resolution focused hovered
                    , circleLabel name circle2d
                    , circleReferenced circle2d False True
                    ]
            , outline = Svg.text ""
            , events = circleEvents cfg circle2d
            }

        ( True, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ circleInfo circle resolution focused hovered
                    , circleLabel name circle2d
                    , circleReferenced circle2d True True
                    ]
            , outline = circleOutline circle2d
            , events = circleEvents cfg circle2d
            }


circleLabel : String -> Circle2d Pixels coordinates -> Svg msg
circleLabel label circle2d =
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
        , Svg.Attributes.fill (toColor Ui.Theme.Color.primary)
        ]
        [ Svg.text label ]


circleInfo : Circle coordinates -> Resolution -> Bool -> Bool -> Svg msg
circleInfo circle resolution focused hovered =
    case circle.info of
        Nothing ->
            Svg.text ""

        Just (WithRadius info) ->
            let
                centerPoint2d =
                    Point2d.at resolution info.centerPoint.point2d
            in
            pointReferenced centerPoint2d hovered

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
                [ pointInfo 3 info.pointA resolution focused hovered
                , pointInfo 3 info.pointB resolution focused hovered
                , pointInfo 3 info.pointC resolution focused hovered
                , pointReferenced pointA2d hovered
                , pointReferenced pointB2d hovered
                , pointReferenced pointC2d hovered
                ]


circleOutline : Circle2d Pixels coordinates -> Svg msg
circleOutline circle2d =
    Svg.circle2d
        [ Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeDasharray "4 7"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.fill "none"
        ]
        (Circle2d.withRadius
            (Quantity.plus (pixels 8) (Circle2d.radius circle2d))
            (Circle2d.centerPoint circle2d)
        )


circleReferenced : Circle2d Pixels coordinates -> Bool -> Bool -> Svg msg
circleReferenced circle2d focused hovered =
    Svg.circle2d
        [ Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Theme.Color.primary

            else
                toColor Ui.Theme.Color.black
        , Svg.Attributes.strokeWidth <|
            if focused then
                "1.5"

            else
                "1"
        , Svg.Attributes.fill "none"
        ]
        circle2d


circleEvents : Config msg -> Circle2d Pixels coordinates -> Svg msg
circleEvents cfg circle2d =
    Svg.circle2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.onClick cfg.onSelect
        ]
        circle2d



---- CURVE


{-| -}
drawCurve : Config msg -> String -> Curve coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawCurve cfg name curve resolution focused hovered selected =
    let
        curve2d =
            Curve2d.at resolution curve.curve2d
    in
    case ( focused, hovered ) of
        ( False, False ) ->
            { inactive = curveInactive curve2d
            , active = Svg.text ""
            , outline = Svg.text ""
            , events = curveEvents cfg curve2d
            }

        ( True, False ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ curveLabel name curve2d
                    , curveFocused curve2d
                    ]
            , outline = Svg.text ""
            , events = curveEvents cfg curve2d
            }

        ( False, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ curveLabel name curve2d
                    , curveHovered curve2d
                    ]
            , outline = Svg.text ""
            , events = curveEvents cfg curve2d
            }

        ( True, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ curveLabel name curve2d
                    , curveFocused curve2d
                    ]
            , outline = Svg.text ""
            , events = curveEvents cfg curve2d
            }


curveLabel : String -> Curve2d Pixels coordinates -> Svg msg
curveLabel label curve2d =
    let
        labelPosition =
            curve2d
                |> Curve2d.boundingBox
                |> BoundingBox2d.extrema
    in
    Svg.text_
        [ Svg.Attributes.x (String.fromFloat (Pixels.inPixels labelPosition.maxX))
        , Svg.Attributes.y (String.fromFloat (20 + Pixels.inPixels labelPosition.maxY))
        , Svg.Attributes.textAnchor "middle"
        , font
        , Svg.Attributes.fill (toColor Ui.Theme.Color.black)
        ]
        [ Svg.text label ]


curveInfo : Curve coordinates -> Resolution -> Bool -> Bool -> Svg msg
curveInfo curve resolution focused hovered =
    case curve.info of
        Nothing ->
            Svg.text ""

        Just (LineSegment info) ->
            Svg.g []
                [ pointInfo 3 info.startPoint resolution focused hovered
                , pointInfo 3 info.endPoint resolution focused hovered
                , pointReferenced (Point2d.at resolution info.startPoint.point2d) hovered
                , pointReferenced (Point2d.at resolution info.endPoint.point2d) hovered
                ]

        Just (QuadraticSpline info) ->
            Svg.g []
                [ pointInfo 3 info.firstControlPoint resolution focused hovered
                , pointInfo 3 info.secondControlPoint resolution focused hovered
                , pointInfo 3 info.thirdControlPoint resolution focused hovered
                , pointReferenced (Point2d.at resolution info.firstControlPoint.point2d) hovered
                , pointReferenced (Point2d.at resolution info.secondControlPoint.point2d) hovered
                , pointReferenced (Point2d.at resolution info.thirdControlPoint.point2d) hovered
                ]

        Just (CubicSpline info) ->
            Svg.g []
                [ pointInfo 3 info.firstControlPoint resolution focused hovered
                , pointInfo 3 info.secondControlPoint resolution focused hovered
                , pointInfo 3 info.thirdControlPoint resolution focused hovered
                , pointInfo 3 info.fourthControlPoint resolution focused hovered
                , pointReferenced (Point2d.at resolution info.firstControlPoint.point2d) hovered
                , pointReferenced (Point2d.at resolution info.secondControlPoint.point2d) hovered
                , pointReferenced (Point2d.at resolution info.thirdControlPoint.point2d) hovered
                , pointReferenced (Point2d.at resolution info.fourthControlPoint.point2d) hovered
                ]


curveOutline : Curve2d Pixels coordinates -> Svg msg
curveOutline curve2d =
    Svg.boundingBox2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeDasharray "4 7"
        , Svg.Attributes.strokeLinecap "round"
        ]
        (Curve2d.boundingBox curve2d
            |> BoundingBox2d.expandBy (pixels 8)
        )


curveInactive : Curve2d Pixels coordinates -> Svg msg
curveInactive curve2d =
    Svg.curve2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
        , Svg.Attributes.strokeWidth "1"
        ]
        curve2d


curveHovered : Curve2d Pixels coordinates -> Svg msg
curveHovered curve2d =
    Svg.g []
        [ Svg.curve2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
            , Svg.Attributes.strokeWidth "8"
            , Svg.Attributes.strokeLinecap "round"
            ]
            curve2d
        , Svg.curve2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke (toColor Ui.Theme.Color.secondary)
            , Svg.Attributes.strokeWidth "6"
            , Svg.Attributes.strokeLinecap "round"
            ]
            curve2d
        , Svg.curve2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
            , Svg.Attributes.strokeWidth "1"
            ]
            curve2d
        ]


curveFocused : Curve2d Pixels coordinates -> Svg msg
curveFocused curve2d =
    Svg.g []
        [ Svg.curve2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
            , Svg.Attributes.strokeWidth "8"
            , Svg.Attributes.strokeLinecap "round"
            ]
            curve2d
        , Svg.curve2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke (toColor Ui.Theme.Color.white)
            , Svg.Attributes.strokeWidth "1"
            ]
            curve2d
        ]


curveReferenced : Curve2d Pixels coordinates -> Bool -> Svg msg
curveReferenced curve2d hovered =
    Svg.curve2d
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Theme.Color.primary

            else
                toColor Ui.Theme.Color.black
        , Svg.Attributes.strokeWidth "1"
        ]
        curve2d


curveEvents : Config msg -> Curve2d Pixels coordinates -> Svg msg
curveEvents cfg curve2d =
    Svg.curve2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.onClick cfg.onSelect
        ]
        curve2d



---- DETAIL


{-| -}
drawDetail : Config msg -> String -> Detail coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawDetail cfg name detail resolution focused hovered selected =
    let
        detail2d =
            Detail2d.at resolution detail.detail2d
    in
    case ( focused, hovered ) of
        ( False, False ) ->
            { inactive = detailInactive detail2d
            , active = Svg.text ""
            , outline = Svg.text ""
            , events = detailEvents cfg detail2d
            }

        ( True, False ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ detailLabel name detail2d
                    , detailFocused detail2d
                    ]
            , outline = Svg.text ""
            , events = detailEvents cfg detail2d
            }

        ( False, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ detailLabel name detail2d
                    , detailHovered detail2d
                    ]
            , outline = Svg.text ""
            , events = detailEvents cfg detail2d
            }

        ( True, True ) ->
            { inactive = Svg.text ""
            , active =
                Svg.g []
                    [ detailLabel name detail2d
                    , detailFocused detail2d
                    ]
            , outline = Svg.text ""
            , events = detailEvents cfg detail2d
            }


detailLabel : String -> Detail2d Pixels coordinates -> Svg msg
detailLabel label detail2d =
    let
        labelPosition =
            Point2d.origin
                |> Point2d.toPixels
    in
    Svg.text_
        [ Svg.Attributes.x (String.fromFloat labelPosition.x)
        , Svg.Attributes.y (String.fromFloat labelPosition.y)
        , Svg.Attributes.textAnchor "middle"
        , font
        , Svg.Attributes.fill (toColor Ui.Theme.Color.primary)
        ]
        [ Svg.text label ]


detailInfo : Detail coordinates -> Resolution -> Bool -> Bool -> Svg msg
detailInfo detail resolution focused hovered =
    let
        detailPointInfo point =
            [ pointInfo 3 point resolution focused hovered
            , pointReferenced (Point2d.at resolution point.point2d) hovered
            ]

        detailCurveInfo curve =
            [ Svg.text "TODO" ]
    in
    Svg.g []
        [ Svg.g [] (List.concatMap detailPointInfo detail.points)
        , Svg.g [] (List.concatMap detailCurveInfo detail.curves)
        ]


detailOutline : Detail2d Pixels coordinates -> Svg msg
detailOutline detail2d =
    case Detail2d.boundingBox detail2d of
        Nothing ->
            Svg.text ""

        Just boundingBox2d ->
            Svg.boundingBox2d
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.strokeDasharray "4 7"
                , Svg.Attributes.strokeLinecap "round"
                ]
                (BoundingBox2d.expandBy (pixels 8) boundingBox2d)


detailInactive : Detail2d Pixels coordinates -> Svg msg
detailInactive detail2d =
    Svg.detail2d
        [ Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
        , Svg.Attributes.strokeWidth "1"
        , Svg.Attributes.fill "none"
        ]
        detail2d


detailHovered : Detail2d Pixels coordinates -> Svg msg
detailHovered detail2d =
    Svg.g []
        [ Svg.detail2d
            [ Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
            , Svg.Attributes.strokeWidth "8"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.fill "none"
            ]
            detail2d
        , Svg.detail2d
            [ Svg.Attributes.stroke (toColor Ui.Theme.Color.secondary)
            , Svg.Attributes.strokeWidth "6"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.fill "none"
            ]
            detail2d
        , Svg.detail2d
            [ Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "none"
            ]
            detail2d
        ]


detailFocused : Detail2d Pixels coordinates -> Svg msg
detailFocused detail2d =
    Svg.g []
        [ Svg.detail2d
            [ Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
            , Svg.Attributes.strokeWidth "8"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.fill "none"
            ]
            detail2d
        , Svg.detail2d
            [ Svg.Attributes.stroke (toColor Ui.Theme.Color.white)
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "none"
            ]
            detail2d
        ]


detailReferenced : Detail2d Pixels coordinates -> Bool -> Bool -> Svg msg
detailReferenced detail2d focused hovered =
    Svg.detail2d
        [ Svg.Attributes.stroke <|
            if hovered then
                toColor Ui.Theme.Color.primary

            else
                toColor Ui.Theme.Color.black
        , Svg.Attributes.strokeWidth <|
            if focused then
                "1.5"

            else
                "1"
        , Svg.Attributes.fill "none"
        ]
        detail2d


detailBackground : Detail2d Pixels coordinates -> Bool -> Bool -> Svg msg
detailBackground detail2d focused hovered =
    Svg.detail2d
        [ Svg.Attributes.fill <|
            if hovered then
                toColor Ui.Theme.Color.secondaryDark

            else
                toColor Ui.Theme.Color.secondary
        ]
        detail2d


detailEvents : Config msg -> Detail2d Pixels coordinates -> Svg msg
detailEvents cfg detail2d =
    Svg.detail2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.onClick cfg.onSelect
        ]
        detail2d



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