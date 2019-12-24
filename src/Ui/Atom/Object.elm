module Ui.Atom.Object exposing
    ( Point, PointInfo(..)
    , Intersectable(..)
    , Axis, AxisInfo(..)
    , Circle, CircleInfo(..)
    , Curve, CurveInfo(..)
    , Detail
    , Config, Resolution
    , drawPoint, drawAxis, drawCircle, drawCurve, drawDetail
    , pointEvents, axisEvents, circleEvents, curveEvents, detailEvents
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
@docs pointEvents, axisEvents, circleEvents, curveEvents, detailEvents

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
    { background : Svg msg
    , selected : Svg msg
    , active : Svg msg
    }


inLayer : Svg msg -> Bool -> Bool -> Bool -> Layers msg
inLayer object focused hovered selected =
    if focused || hovered then
        { background = Svg.text ""
        , selected = Svg.text ""
        , active = object
        }

    else if selected then
        { background = Svg.text ""
        , selected = object
        , active = Svg.text ""
        }

    else
        { background = object
        , selected = Svg.text ""
        , active = Svg.text ""
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
pointEvents : Config msg -> Point coordinates -> Resolution -> Svg msg
pointEvents cfg point resolution =
    let
        point2d =
            Point2d.at resolution point.point2d
    in
    Svg.circle2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.stopPropagationOn "click"
            (Decode.succeed ( cfg.onSelect, True ))
        ]
        (Circle2d.withRadius (pixels 8) point2d)


{-| -}
drawPoint : String -> Point coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawPoint name point resolution focused hovered selected =
    let
        point2d =
            Point2d.at resolution point.point2d
    in
    inLayer
        (Svg.g []
            [ if focused then
                Svg.circle2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
                    , Svg.Attributes.strokeWidth "1"
                    ]
                    (Circle2d.withRadius (pixels 9) point2d)

              else
                Svg.text ""
            , if hovered || selected then
                Svg.circle2d
                    [ Svg.Attributes.fill (auraColor hovered selected) ]
                    (Circle2d.withRadius (pixels 9) point2d)

              else
                Svg.text ""
            , Svg.circle2d
                [ Svg.Attributes.fill (objectColor selected) ]
                (Circle2d.withRadius (pixels 3) point2d)
            , if focused || hovered then
                pointLabel name point2d

              else
                Svg.text ""
            ]
        )
        focused
        hovered
        selected


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



---- AXIS


{-| -}
axisEvents : Config msg -> Axis coordinates -> Resolution -> Svg msg
axisEvents cfg axis resolution =
    let
        axis2d =
            Axis2d.at resolution axis.axis2d
    in
    Svg.lineSegment2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.stopPropagationOn "click"
            (Decode.succeed ( cfg.onSelect, True ))
        ]
        (LineSegment2d.along axis2d
            (pixels -1000)
            (pixels 1000)
        )


{-| -}
drawAxis : String -> Axis coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawAxis name axis resolution focused hovered selected =
    let
        axis2d =
            Axis2d.at resolution axis.axis2d

        lineSegment2d =
            LineSegment2d.along axis2d
                (pixels -1000)
                (pixels 1000)
    in
    inLayer
        (strokeBased
            (\attrs -> Svg.lineSegment2d attrs lineSegment2d)
            (\label -> axisLabel label axis2d)
            name
            focused
            hovered
            selected
        )
        focused
        hovered
        selected


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



---- CIRCLE


{-| -}
circleEvents : Config msg -> Circle coordinates -> Resolution -> Svg msg
circleEvents cfg circle resolution =
    let
        circle2d =
            Circle2d.at resolution circle.circle2d
    in
    Svg.circle2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.stopPropagationOn "click"
            (Decode.succeed ( cfg.onSelect, True ))
        ]
        circle2d


{-| -}
drawCircle : String -> Circle coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawCircle name circle resolution focused hovered selected =
    let
        circle2d =
            Circle2d.at resolution circle.circle2d
    in
    inLayer
        (strokeBased
            (\attrs -> Svg.circle2d attrs circle2d)
            (\label -> circleLabel label circle2d)
            name
            focused
            hovered
            selected
        )
        focused
        hovered
        selected


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



---- CURVE


{-| -}
curveEvents : Config msg -> Curve coordinates -> Resolution -> Svg msg
curveEvents cfg curve resolution =
    let
        curve2d =
            Curve2d.at resolution curve.curve2d
    in
    Svg.curve2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.stopPropagationOn "click"
            (Decode.succeed ( cfg.onSelect, True ))
        ]
        curve2d


{-| -}
drawCurve : String -> Curve coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawCurve name curve resolution focused hovered selected =
    let
        curve2d =
            Curve2d.at resolution curve.curve2d
    in
    inLayer
        (strokeBased
            (\attrs -> Svg.curve2d attrs curve2d)
            (\label -> curveLabel label curve2d)
            name
            focused
            hovered
            selected
        )
        focused
        hovered
        selected


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



---- DETAIL


{-| -}
detailEvents : Config msg -> Detail coordinates -> Resolution -> Svg msg
detailEvents cfg detail resolution =
    let
        detail2d =
            Detail2d.at resolution detail.detail2d
    in
    Svg.detail2d
        [ Svg.Attributes.stroke "transparent"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.style "outline: none;"
        , Svg.Events.onMouseOver cfg.onHover
        , Svg.Events.onMouseOut cfg.onLeave
        , Svg.Events.stopPropagationOn "click"
            (Decode.succeed ( cfg.onSelect, True ))
        ]
        detail2d


{-| -}
drawDetail : String -> Detail coordinates -> Resolution -> Bool -> Bool -> Bool -> Layers msg
drawDetail name detail resolution focused hovered selected =
    let
        detail2d =
            Detail2d.at resolution detail.detail2d
    in
    inLayer
        (strokeBased
            (\attrs -> Svg.detail2d attrs detail2d)
            (\label -> detailLabel label detail2d)
            name
            focused
            hovered
            selected
        )
        focused
        hovered
        selected


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


auraColor : Bool -> Bool -> String
auraColor hovered selected =
    case ( hovered, selected ) of
        ( False, False ) ->
            "none"

        ( False, True ) ->
            toColor Ui.Theme.Color.primary

        ( True, False ) ->
            toColor Ui.Theme.Color.secondary

        ( True, True ) ->
            toColor Ui.Theme.Color.primaryLight


objectColor : Bool -> String
objectColor selected =
    if selected then
        toColor Ui.Theme.Color.white

    else
        toColor Ui.Theme.Color.black


strokeBased :
    (List (Svg.Attribute msg) -> Svg msg)
    -> (String -> Svg msg)
    -> String
    -> Bool
    -> Bool
    -> Bool
    -> Svg msg
strokeBased object2d label name focused hovered selected =
    Svg.g []
        [ if focused then
            Svg.g []
                [ object2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke (toColor Ui.Theme.Color.black)
                    , Svg.Attributes.strokeWidth "11"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                , object2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke (toColor Ui.Theme.Color.white)
                    , Svg.Attributes.strokeWidth "9"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                ]

          else
            Svg.text ""
        , if hovered || selected then
            object2d
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke (auraColor hovered selected)
                , Svg.Attributes.strokeWidth "9"
                , Svg.Attributes.strokeLinecap "round"
                ]

          else
            Svg.text ""
        , object2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke (objectColor selected)
            , Svg.Attributes.strokeWidth "1"
            ]
        , if focused || hovered then
            label name

          else
            Svg.text ""
        ]
