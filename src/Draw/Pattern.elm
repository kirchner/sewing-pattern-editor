module Draw.Pattern exposing (draw)

{-
   Sewing pattern editor
   Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

import Axis2d exposing (Axis2d)
import Axis2d.Extra as Axis2d
import Circle2d exposing (Circle2d)
import CubicSpline2d
import Direction2d
import Frame2d
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import Pattern
    exposing
        ( A
        , Axis
        , Circle
        , Curve
        , Curve2d(..)
        , Detail
        , Detail2d
        , LastCurve2d(..)
        , NextCurve2d(..)
        , Pattern
        , Point
        , PointInfo(..)
        )
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import QuadraticSpline2d
import State exposing (State)
import StateResult
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Triple


draw :
    { preview : Bool
    , zoom : Float
    , pointHovered : Maybe (A Point) -> msg
    , hoveredPoint : Maybe (A Point)
    }
    -> State Pattern (Svg msg)
draw { preview, zoom, pointHovered, hoveredPoint } =
    let
        center =
            Point2d.origin
    in
    State.get
        |> State.andThen
            (\pattern ->
                State.map (Svg.g []) <|
                    State.traverse identity
                        [ case hoveredPoint of
                            Nothing ->
                                State.state (Svg.g [] [])

                            Just aPoint ->
                                drawPointInfo preview center zoom aPoint
                        , Pattern.details pattern
                            |> State.traverse (drawDetail preview center zoom)
                            |> State.map (Svg.g [])
                        , Pattern.axes pattern
                            |> State.traverse (drawAxis preview center zoom)
                            |> State.map (Svg.g [])
                        , Pattern.circles pattern
                            |> State.traverse (drawCircle preview center zoom)
                            |> State.map (Svg.g [])
                        , Pattern.curves pattern
                            |> State.traverse (drawCurve preview center zoom)
                            |> State.map (Svg.g [])
                        , Pattern.points pattern
                            |> State.traverse (drawPoint pointHovered preview center zoom)
                            |> State.map (Svg.g [])
                        ]
            )


drawPointInfo : Bool -> Point2d -> Float -> A Point -> State Pattern (Svg msg)
drawPointInfo preview center zoom aPoint =
    let
        pointRadius =
            if preview then
                1

            else
                5
    in
    State.embed (Pattern.pointInfo aPoint)
        |> State.andThen
            (\maybePointInfo ->
                case maybePointInfo of
                    Nothing ->
                        State.state (Svg.g [] [])

                    Just (Origin _) ->
                        State.state (Svg.g [] [])

                    Just (FromOnePoint stuff) ->
                        let
                            toSvg thisPoint2d basePoint2d =
                                Svg.g []
                                    [ Svg.circle2d
                                        [ fill Blue
                                        , stroke Blue
                                        ]
                                        (Circle2d.withRadius pointRadius <|
                                            Point2d.scaleAbout center zoom thisPoint2d
                                        )
                                    , Svg.circle2d
                                        [ fill Blue
                                        , stroke Blue
                                        ]
                                        (Circle2d.withRadius pointRadius <|
                                            Point2d.scaleAbout center zoom basePoint2d
                                        )
                                    , Svg.lineSegment2d
                                        [ Svg.Attributes.fill "none"
                                        , stroke Blue
                                        , dashArrayShort
                                        ]
                                        (LineSegment2d.scaleAbout center zoom <|
                                            LineSegment2d.from thisPoint2d basePoint2d
                                        )
                                    ]
                        in
                        StateResult.ok toSvg
                            |> StateResult.with (Pattern.point2d aPoint)
                            |> StateResult.with (Pattern.point2d stuff.basePoint)
                            |> StateResult.withDefault (Svg.g [] [])

                    Just (BetweenRatio stuff) ->
                        State.state (Svg.g [] [])

                    Just (BetweenLength stuff) ->
                        State.state (Svg.g [] [])

                    Just (Intersection stuff) ->
                        State.state (Svg.g [] [])

                    Just (TransformedPoint stuff) ->
                        State.state (Svg.g [] [])
            )


drawPoint :
    (Maybe (A Point) -> msg)
    -> Bool
    -> Point2d
    -> Float
    -> A Point
    -> State Pattern (Svg msg)
drawPoint pointHovered preview center zoom aPoint =
    let
        selected =
            False

        _ =
            \_ ->
                Debug.todo "implement selected"
    in
    Pattern.point2d aPoint
        |> StateResult.map
            (Point2d.scaleAbout center zoom
                >> drawPointHelp pointHovered preview selected aPoint
            )
        |> StateResult.withDefault (Svg.g [] [])


drawPointHelp :
    (Maybe (A Point) -> msg)
    -> Bool
    -> Bool
    -> A Point
    -> Point2d
    -> Svg msg
drawPointHelp pointHovered preview selected aPoint point2d =
    let
        pointRadius =
            if preview then
                1

            else
                5

        ( x, y ) =
            Point2d.coordinates point2d

        maybeName =
            Pattern.name aPoint

        drawName name =
            if preview then
                Svg.text ""

            else
                Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (x - 10))
                    , Svg.Attributes.y (String.fromFloat y)
                    , Svg.Attributes.dy (String.fromFloat -10)
                    , Svg.Attributes.textAnchor "middle"
                    , fontNormal
                    , Svg.Attributes.fill <|
                        if selected then
                            "green"

                        else
                            "black"
                    ]
                    [ Svg.text name ]
    in
    Svg.g []
        [ if selected then
            Svg.g
                []
                [ Svg.circle2d
                    [ stroke Green
                    , Svg.Attributes.fill "none"
                    , strokeWidthBold
                    ]
                    (Circle2d.withRadius 10 point2d)
                ]

          else
            Svg.text ""
        , Svg.circle2d
            [ Svg.Attributes.fill "none"
            , stroke Black
            , if preview then
                strokeWidthThin

              else
                strokeWidthNormal
            ]
            (Circle2d.withRadius pointRadius point2d)
        , Svg.circle2d
            [ Svg.Attributes.fill "transparent"
            , Svg.Events.onMouseOver (pointHovered (Just aPoint))
            , Svg.Events.onMouseOut (pointHovered Nothing)
            ]
            (Circle2d.withRadius (pointRadius + 4) point2d)
        , maybeName
            |> Maybe.map drawName
            |> Maybe.withDefault (Svg.text "")
        ]


drawAxis : Bool -> Point2d -> Float -> A Axis -> State Pattern (Svg msg)
drawAxis preview center zoom aAxis =
    let
        selected =
            False

        _ =
            \_ ->
                Debug.todo "implement selected"
    in
    Pattern.axis2d aAxis
        |> StateResult.map
            (Axis2d.scaleAbout center zoom
                >> drawAxisHelp preview selected aAxis
            )
        |> StateResult.withDefault (Svg.g [] [])


drawAxisHelp : Bool -> Bool -> A Axis -> Axis2d -> Svg msg
drawAxisHelp preview selected aAxis axis2d =
    Svg.lineSegment2d
        (if selected then
            [ stroke Green
            , Svg.Attributes.opacity "1"
            , strokeWidthBold
            ]

         else
            [ stroke Black
            , Svg.Attributes.opacity "0.1"
            , dashArrayNormal preview
            , if preview then
                strokeWidthThin

              else
                strokeWidthNormal
            ]
        )
        (LineSegment2d.fromEndpoints
            ( Point2d.along axis2d -10000
            , Point2d.along axis2d 10000
            )
        )


drawCircle : Bool -> Point2d -> Float -> A Circle -> State Pattern (Svg msg)
drawCircle preview center zoom aCircle =
    let
        selected =
            False

        _ =
            \_ ->
                Debug.todo "implement selected"
    in
    Pattern.circle2d aCircle
        |> StateResult.map
            (Circle2d.scaleAbout center zoom
                >> drawCircleHelp preview selected aCircle
            )
        |> StateResult.withDefault (Svg.g [] [])


drawCircleHelp : Bool -> Bool -> A Circle -> Circle2d -> Svg msg
drawCircleHelp preview selected aCircle circle2d =
    Svg.circle2d
        [ stroke Black
        , Svg.Attributes.opacity "0.1"
        , dashArrayNormal preview
        , strokeWidthNormal
        , Svg.Attributes.fill "transparent"
        ]
        circle2d


drawCurve : Bool -> Point2d -> Float -> A Curve -> State Pattern (Svg msg)
drawCurve preview center zoom aCurve =
    let
        selected =
            False

        _ =
            \_ ->
                Debug.todo "implement selected"
    in
    Pattern.curve2d aCurve
        |> StateResult.map
            (curve2dScaleAbout center zoom
                >> drawCurveHelp preview selected aCurve
            )
        |> StateResult.withDefault (Svg.g [] [])


drawCurveHelp : Bool -> Bool -> A Curve -> Curve2d -> Svg msg
drawCurveHelp preview selected aCurve curve2d =
    let
        attributes =
            [ stroke Black
            , if preview then
                strokeWidthThin

              else
                strokeWidthNormal
            , Svg.Attributes.fill "transparent"
            ]
    in
    case curve2d of
        LineSegment2d lineSegment2d ->
            Svg.lineSegment2d attributes lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            Svg.quadraticSpline2d attributes quadraticSpline2d

        CubicSpline2d cubicSpline2d ->
            Svg.cubicSpline2d attributes cubicSpline2d


drawDetail : Bool -> Point2d -> Float -> A Detail -> State Pattern (Svg msg)
drawDetail preview center zoom aDetail =
    let
        selected =
            False

        _ =
            \_ ->
                Debug.todo "implement selected"
    in
    Pattern.detail2d aDetail
        |> StateResult.map
            (detail2dScaleAbout center zoom
                >> drawDetailHelp preview selected aDetail
            )
        |> StateResult.withDefault (Svg.g [] [])


drawDetailHelp : Bool -> Bool -> A Detail -> Detail2d -> Svg msg
drawDetailHelp preview selected aDetail detail2d =
    let
        step nextCurve2d =
            case nextCurve2d of
                NextLineSegment2d stuff ->
                    "L " ++ coord stuff.endPoint

                NextQuadraticSpline2d stuff ->
                    "Q "
                        ++ coord stuff.controlPoint
                        ++ ", "
                        ++ coord stuff.endPoint

                NextCubicSpline2d stuff ->
                    "C "
                        ++ coord stuff.startControlPoint
                        ++ ", "
                        ++ coord stuff.endControlPoint
                        ++ ", "
                        ++ coord stuff.endPoint

        lastStep =
            case detail2d.lastCurve of
                LastLineSegment2d ->
                    "L " ++ coord detail2d.firstPoint

                LastQuadraticSpline2d stuff ->
                    "Q "
                        ++ coord stuff.controlPoint
                        ++ ", "
                        ++ coord detail2d.firstPoint

                LastCubicSpline2d stuff ->
                    "C "
                        ++ coord stuff.startControlPoint
                        ++ ", "
                        ++ coord stuff.endControlPoint
                        ++ ", "
                        ++ coord detail2d.firstPoint

        coord point2d =
            Point2d.coordinates point2d
                |> coordHelp

        coordHelp ( x, y ) =
            String.fromFloat x ++ " " ++ String.fromFloat y
    in
    Svg.path
        [ Svg.Attributes.fill "hsla(240, 2%, 80%, 0.5)"
        , strokeWidthNormal
        , stroke <|
            if selected then
                Blue

            else
                Black
        , Svg.Attributes.d <|
            String.join " "
                [ "M " ++ coord detail2d.firstPoint
                , String.join " " (List.map step detail2d.nextCurves)
                , " " ++ lastStep
                ]
        ]
        []



---- SCALE


curve2dScaleAbout : Point2d -> Float -> Curve2d -> Curve2d
curve2dScaleAbout center zoom curve =
    case curve of
        LineSegment2d lineSegment2d ->
            LineSegment2d <|
                LineSegment2d.scaleAbout center zoom lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d <|
                QuadraticSpline2d.scaleAbout center zoom quadraticSpline2d

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d <|
                CubicSpline2d.scaleAbout center zoom cubicSpline2d


detail2dScaleAbout : Point2d -> Float -> Detail2d -> Detail2d
detail2dScaleAbout center zoom detail =
    let
        scaleNextCurve nextCurve =
            case nextCurve of
                NextLineSegment2d stuff ->
                    NextLineSegment2d
                        { endPoint = Point2d.scaleAbout center zoom stuff.endPoint }

                NextQuadraticSpline2d stuff ->
                    NextQuadraticSpline2d
                        { controlPoint = Point2d.scaleAbout center zoom stuff.controlPoint
                        , endPoint = Point2d.scaleAbout center zoom stuff.endPoint
                        }

                NextCubicSpline2d stuff ->
                    NextCubicSpline2d
                        { startControlPoint =
                            Point2d.scaleAbout center zoom stuff.startControlPoint
                        , endControlPoint =
                            Point2d.scaleAbout center zoom stuff.endControlPoint
                        , endPoint = Point2d.scaleAbout center zoom stuff.endPoint
                        }
    in
    { detail
        | firstPoint = Point2d.scaleAbout center zoom detail.firstPoint
        , nextCurves = List.map scaleNextCurve detail.nextCurves
        , lastCurve =
            case detail.lastCurve of
                LastLineSegment2d ->
                    detail.lastCurve

                LastQuadraticSpline2d stuff ->
                    LastQuadraticSpline2d
                        { controlPoint = Point2d.scaleAbout center zoom stuff.controlPoint }

                LastCubicSpline2d stuff ->
                    LastCubicSpline2d
                        { startControlPoint =
                            Point2d.scaleAbout center zoom stuff.startControlPoint
                        , endControlPoint =
                            Point2d.scaleAbout center zoom stuff.endControlPoint
                        }
    }



---- HELPER


type Color
    = Blue
    | Black
    | Green


stroke color =
    Svg.Attributes.stroke <|
        case color of
            Blue ->
                "blue"

            Black ->
                "black"

            Green ->
                "green"


fill color =
    Svg.Attributes.fill <|
        case color of
            Blue ->
                "blue"

            Black ->
                "black"

            Green ->
                "green"


strokeWidthThin =
    Svg.Attributes.strokeWidth <|
        String.fromFloat 0.5


strokeWidthNormal =
    Svg.Attributes.strokeWidth <|
        String.fromFloat 1


strokeWidthBold =
    Svg.Attributes.strokeWidth <|
        String.fromFloat 2


dashArrayShort =
    Svg.Attributes.strokeDasharray <|
        String.fromFloat 20
            ++ " "
            ++ String.fromFloat 10


dashArrayNormal preview =
    Svg.Attributes.strokeDasharray <|
        if preview then
            String.fromFloat 4 ++ " " ++ String.fromFloat 2

        else
            String.fromFloat 40 ++ " " ++ String.fromFloat 20


fontNormal =
    Svg.Attributes.style <|
        "font-size: "
            ++ String.fromFloat 12
            ++ "px; font-family: \"Roboto\";"
