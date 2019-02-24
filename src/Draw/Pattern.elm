module Draw.Pattern exposing
    ( HoveredObject
    , draw
    )

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
        , AxisInfo(..)
        , Circle
        , CircleInfo(..)
        , Curve
        , Curve2d(..)
        , CurveInfo(..)
        , Detail
        , Detail2d
        , DetailInfo
        , Direction(..)
        , FirstCurve(..)
        , Intersectable
        , Intersectable2d(..)
        , IntersectableInfo(..)
        , LastCurve(..)
        , LastCurve2d(..)
        , NextCurve(..)
        , NextCurve2d(..)
        , OneInTwo(..)
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
import Vector2d


type HoveredObject
    = HoveredPoint (A Point)
    | HoveredAxis (A Axis)
    | HoveredCircle (A Circle)
    | HoveredCurve (A Curve)
    | HoveredDetail (A Detail)


draw :
    { preview : Bool
    , zoom : Float
    , objectHovered : Maybe HoveredObject -> msg
    , hoveredObject : Maybe HoveredObject
    }
    -> State Pattern (Svg msg)
draw { preview, zoom, objectHovered, hoveredObject } =
    let
        center =
            Point2d.origin
    in
    State.get
        |> State.andThen
            (\pattern ->
                State.map (Svg.g []) <|
                    State.traverse identity
                        ([ Pattern.details pattern
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
                            |> State.traverse (drawPoint preview center zoom)
                            |> State.map (Svg.g [])
                         , case hoveredObject of
                            Nothing ->
                                State.state (Svg.g [] [])

                            Just (HoveredPoint aPoint) ->
                                drawHoveredPoint preview center zoom aPoint

                            Just (HoveredCurve aCurve) ->
                                drawHoveredCurve preview center zoom aCurve

                            Just (HoveredDetail aDetail) ->
                                drawHoveredDetail preview center zoom aDetail

                            Just _ ->
                                State.state (Svg.g [] [])
                         ]
                            ++ (if preview then
                                    []

                                else
                                    [ Pattern.details pattern
                                        |> State.traverse
                                            (msgAreaDetail objectHovered center zoom)
                                        |> State.map (Svg.g [])
                                    , Pattern.curves pattern
                                        |> State.traverse
                                            (msgAreaCurve objectHovered center zoom)
                                        |> State.map (Svg.g [])
                                    , Pattern.points pattern
                                        |> State.traverse
                                            (msgAreaPoint objectHovered center zoom)
                                        |> State.map (Svg.g [])
                                    ]
                               )
                        )
            )


drawHoveredPoint : Bool -> Point2d -> Float -> A Point -> State Pattern (Svg msg)
drawHoveredPoint preview center zoom aPoint =
    let
        toSvg info2d point2d =
            Svg.g []
                [ info2d
                , svgHoveredPoint center zoom point2d
                ]
    in
    StateResult.ok toSvg
        |> StateResult.with (State.map Ok (drawPointInfo center zoom aPoint))
        |> StateResult.with (Pattern.point2d aPoint)
        |> StateResult.withDefault (Svg.g [] [])


drawHoveredCurve : Bool -> Point2d -> Float -> A Curve -> State Pattern (Svg msg)
drawHoveredCurve preview center zoom aCurve =
    let
        toSvg info2d curve2d =
            Svg.g []
                [ info2d
                , svgHoveredCurve center zoom curve2d
                ]
    in
    StateResult.ok toSvg
        |> StateResult.with (State.map Ok (drawCurveInfo center zoom aCurve))
        |> StateResult.with (Pattern.curve2d aCurve)
        |> StateResult.withDefault (Svg.g [] [])


drawHoveredDetail : Bool -> Point2d -> Float -> A Detail -> State Pattern (Svg msg)
drawHoveredDetail preview center zoom aDetail =
    let
        toSvg info2d detail2d =
            Svg.g []
                [ info2d
                , svgHoveredDetail center zoom detail2d
                ]
    in
    StateResult.ok toSvg
        |> StateResult.with (State.map Ok (drawDetailInfo center zoom aDetail))
        |> StateResult.with (Pattern.detail2d aDetail)
        |> StateResult.withDefault (Svg.g [] [])


drawPointInfo : Point2d -> Float -> A Point -> State Pattern (Svg msg)
drawPointInfo center zoom aPoint =
    State.embed (Pattern.pointInfo aPoint)
        |> State.andThen
            (\maybePointInfo ->
                case maybePointInfo of
                    Nothing ->
                        State.state (Svg.g [] [])

                    Just (Origin _) ->
                        State.state (Svg.g [] [])

                    Just (FromOnePoint stuff) ->
                        [ StateResult.ok (svgLabeledLineSegment center zoom stuff.distance)
                            |> StateResult.with (Pattern.point2d aPoint)
                            |> StateResult.with (Pattern.point2d stuff.basePoint)
                        , State.map Ok (drawReferencedPoint center zoom stuff.basePoint)
                        ]
                            |> StateResult.combine
                            |> StateResult.map (Svg.g [])
                            |> StateResult.withDefault (Svg.g [] [])

                    Just (BetweenRatio stuff) ->
                        [ StateResult.ok (svgLabeledLineSegment center zoom stuff.ratio)
                            |> StateResult.with (Pattern.point2d aPoint)
                            |> StateResult.with (Pattern.point2d stuff.basePointA)
                        , StateResult.ok (svgLabeledLineSegment center zoom "")
                            |> StateResult.with (Pattern.point2d aPoint)
                            |> StateResult.with (Pattern.point2d stuff.basePointB)
                        , State.map Ok (drawReferencedPoint center zoom stuff.basePointA)
                        , State.map Ok (drawReferencedPoint center zoom stuff.basePointB)
                        ]
                            |> StateResult.combine
                            |> StateResult.map (Svg.g [])
                            |> StateResult.withDefault (Svg.g [] [])

                    Just (BetweenLength stuff) ->
                        [ case stuff.from of
                            FirstInTwo ->
                                [ StateResult.ok
                                    (svgLabeledLineSegment center zoom stuff.distance)
                                    |> StateResult.with (Pattern.point2d aPoint)
                                    |> StateResult.with (Pattern.point2d stuff.basePointA)
                                , StateResult.ok
                                    (svgLabeledLineSegment center zoom "")
                                    |> StateResult.with (Pattern.point2d aPoint)
                                    |> StateResult.with (Pattern.point2d stuff.basePointB)
                                ]

                            SecondInTwo ->
                                [ StateResult.ok
                                    (svgLabeledLineSegment center zoom "")
                                    |> StateResult.with (Pattern.point2d aPoint)
                                    |> StateResult.with (Pattern.point2d stuff.basePointA)
                                , StateResult.ok
                                    (svgLabeledLineSegment center zoom stuff.distance)
                                    |> StateResult.with (Pattern.point2d aPoint)
                                    |> StateResult.with (Pattern.point2d stuff.basePointB)
                                ]
                        , [ State.map Ok (drawReferencedPoint center zoom stuff.basePointA)
                          , State.map Ok (drawReferencedPoint center zoom stuff.basePointB)
                          ]
                        ]
                            |> List.concat
                            |> StateResult.combine
                            |> StateResult.map (Svg.g [])
                            |> StateResult.withDefault (Svg.g [] [])

                    Just (Intersection stuff) ->
                        [ State.map Ok
                            (drawReferencedIntersectable center zoom stuff.objectA)
                        , State.map Ok
                            (drawReferencedIntersectable center zoom stuff.objectB)
                        ]
                            |> StateResult.combine
                            |> StateResult.map (Svg.g [])
                            |> StateResult.withDefault (Svg.g [] [])

                    Just (TransformedPoint stuff) ->
                        State.state (Svg.g [] [])
            )


drawCurveInfo : Point2d -> Float -> A Curve -> State Pattern (Svg msg)
drawCurveInfo center zoom aCurve =
    State.embed (Pattern.curveInfo aCurve)
        |> State.andThen
            (Maybe.map (drawCurveInfoHelp center zoom)
                >> Maybe.withDefault (State.state (Svg.g [] []))
            )


drawDetailInfo : Point2d -> Float -> A Detail -> State Pattern (Svg msg)
drawDetailInfo center zoom aDetail =
    State.embed (Pattern.detailInfo aDetail)
        |> State.andThen
            (Maybe.map (drawDetailInfoHelp center zoom)
                >> Maybe.withDefault (State.state (Svg.g [] []))
            )


drawIntersectableInfo : Point2d -> Float -> A Intersectable -> State Pattern (Svg msg)
drawIntersectableInfo center zoom aIntersectable =
    State.embed (Pattern.intersectableInfo aIntersectable)
        |> State.andThen
            (\maybeIntersectableInfo ->
                case maybeIntersectableInfo of
                    Nothing ->
                        State.state (Svg.g [] [])

                    Just (AxisInfo info) ->
                        drawAxisInfoHelp center zoom info

                    Just (CircleInfo info) ->
                        drawCircleInfoHelp center zoom info

                    Just (CurveInfo info) ->
                        drawCurveInfoHelp center zoom info
            )


drawAxisInfoHelp : Point2d -> Float -> AxisInfo -> State Pattern (Svg msg)
drawAxisInfoHelp center zoom info =
    case info of
        ThroughOnePoint stuff ->
            drawReferencedPoint center zoom stuff.point

        ThroughTwoPoints stuff ->
            [ drawReferencedPoint center zoom stuff.pointA
            , drawReferencedPoint center zoom stuff.pointB
            ]
                |> State.combine
                |> State.map (Svg.g [])

        TransformedAxis stuff ->
            State.state (Svg.g [] [])


drawCircleInfoHelp : Point2d -> Float -> CircleInfo -> State Pattern (Svg msg)
drawCircleInfoHelp center zoom info =
    case info of
        WithRadius stuff ->
            drawReferencedPoint center zoom stuff.centerPoint

        ThroughThreePoints stuff ->
            [ drawReferencedPoint center zoom stuff.pointA
            , drawReferencedPoint center zoom stuff.pointB
            , drawReferencedPoint center zoom stuff.pointC
            ]
                |> State.combine
                |> State.map (Svg.g [])

        TransformedCircle stuff ->
            State.state (Svg.g [] [])


drawCurveInfoHelp : Point2d -> Float -> CurveInfo -> State Pattern (Svg msg)
drawCurveInfoHelp center zoom info =
    case info of
        Straight stuff ->
            [ drawReferencedPoint center zoom stuff.startPoint
            , drawReferencedPoint center zoom stuff.endPoint
            ]
                |> State.combine
                |> State.map (Svg.g [])

        Quadratic stuff ->
            [ drawReferencedPoint center zoom stuff.startPoint
            , drawReferencedPoint center zoom stuff.controlPoint
            , drawReferencedPoint center zoom stuff.endPoint
            ]
                |> State.combine
                |> State.map (Svg.g [])

        Cubic stuff ->
            [ drawReferencedPoint center zoom stuff.startPoint
            , drawReferencedPoint center zoom stuff.startControlPoint
            , drawReferencedPoint center zoom stuff.endControlPoint
            , drawReferencedPoint center zoom stuff.endPoint
            ]
                |> State.combine
                |> State.map (Svg.g [])

        TransformedCurve stuff ->
            State.state (Svg.g [] [])


drawDetailInfoHelp : Point2d -> Float -> DetailInfo -> State Pattern (Svg msg)
drawDetailInfoHelp center zoom info =
    let
        pointsFirstCurve =
            case info.firstCurve of
                FirstStraight stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.startPoint
                        , stuff.endPoint
                        ]

                FirstQuadratic stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.startPoint
                        , stuff.controlPoint
                        , stuff.endPoint
                        ]

                FirstCubic stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.startPoint
                        , stuff.startControlPoint
                        , stuff.endControlPoint
                        , stuff.endPoint
                        ]

                FirstReferencedCurve stuff ->
                    [ drawReferencedCurve center zoom stuff.curve ]

        pointsNextCurves =
            List.concatMap pointsNextCurve info.nextCurves

        pointsNextCurve nextCurve =
            case nextCurve of
                NextStraight stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.endPoint
                        ]

                NextQuadratic stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.controlPoint
                        , stuff.endPoint
                        ]

                NextCubic stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.startControlPoint
                        , stuff.endControlPoint
                        , stuff.endPoint
                        ]

                NextReferencedCurve stuff ->
                    [ drawReferencedCurve center zoom stuff.curve ]

        pointsLastCurve =
            case info.lastCurve of
                LastStraight ->
                    []

                LastQuadratic stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.controlPoint ]

                LastCubic stuff ->
                    List.map (drawReferencedPoint center zoom)
                        [ stuff.startControlPoint
                        , stuff.endControlPoint
                        ]

                LastReferencedCurve stuff ->
                    [ drawReferencedCurve center zoom stuff.curve ]
    in
    (pointsFirstCurve ++ pointsNextCurves ++ pointsLastCurve)
        |> State.combine
        |> State.map (Svg.g [])


drawReferencedPoint : Point2d -> Float -> A Point -> State Pattern (Svg msg)
drawReferencedPoint center zoom aPoint =
    let
        toSvg info2d point2d =
            Svg.g []
                [ svgReferencedPoint center zoom point2d
                , info2d
                ]
    in
    StateResult.ok toSvg
        |> StateResult.with
            (if Pattern.inlined aPoint then
                drawPointInfo center zoom aPoint
                    |> State.map Ok

             else
                StateResult.ok (Svg.g [] [])
            )
        |> StateResult.with (Pattern.point2d aPoint)
        |> StateResult.withDefault (Svg.g [] [])


drawReferencedCurve : Point2d -> Float -> A Curve -> State Pattern (Svg msg)
drawReferencedCurve center zoom aCurve =
    let
        toSvg info2d curve2d =
            Svg.g []
                [ svgReferencedCurve center zoom curve2d
                , info2d
                ]
    in
    StateResult.ok toSvg
        |> StateResult.with
            (if Pattern.inlined aCurve then
                drawCurveInfo center zoom aCurve
                    |> State.map Ok

             else
                StateResult.ok (Svg.g [] [])
            )
        |> StateResult.with (Pattern.curve2d aCurve)
        |> StateResult.withDefault (Svg.g [] [])


drawReferencedIntersectable :
    Point2d
    -> Float
    -> A Intersectable
    -> State Pattern (Svg msg)
drawReferencedIntersectable center zoom aIntersectable =
    let
        toSvg info2d intersectable2d =
            Svg.g []
                [ svgReferencedIntersectable center zoom intersectable2d
                , info2d
                ]
    in
    StateResult.ok toSvg
        |> StateResult.with
            (if Pattern.inlined aIntersectable then
                drawIntersectableInfo center zoom aIntersectable
                    |> State.map Ok

             else
                StateResult.ok (Svg.g [] [])
            )
        |> StateResult.with (Pattern.intersectable2d aIntersectable)
        |> StateResult.withDefault (Svg.g [] [])


msgAreaPoint :
    (Maybe HoveredObject -> msg)
    -> Point2d
    -> Float
    -> A Point
    -> State Pattern (Svg msg)
msgAreaPoint objectHovered center zoom aPoint =
    Pattern.point2d aPoint
        |> StateResult.map
            (Point2d.scaleAbout center zoom
                >> Circle2d.withRadius 10
                >> Svg.circle2d
                    [ Svg.Attributes.fill "transparent"
                    , Svg.Events.onMouseOver (objectHovered (Just (HoveredPoint aPoint)))
                    , Svg.Events.onMouseOut (objectHovered Nothing)
                    ]
            )
        |> StateResult.withDefault (Svg.g [] [])


msgAreaCurve :
    (Maybe HoveredObject -> msg)
    -> Point2d
    -> Float
    -> A Curve
    -> State Pattern (Svg msg)
msgAreaCurve objectHovered center zoom aCurve =
    let
        attributes =
            [ Svg.Attributes.stroke "transparent"
            , Svg.Attributes.strokeWidth "10"
            , Svg.Attributes.fill "none"
            , Svg.Events.onMouseOver (objectHovered (Just (HoveredCurve aCurve)))
            , Svg.Events.onMouseOut (objectHovered Nothing)
            ]

        msgAreaCurveHelp curve2d =
            case curve2d of
                LineSegment2d lineSegment2d ->
                    Svg.lineSegment2d attributes lineSegment2d

                QuadraticSpline2d quadraticSpline2d ->
                    Svg.quadraticSpline2d attributes quadraticSpline2d

                CubicSpline2d cubicSpline2d ->
                    Svg.cubicSpline2d attributes cubicSpline2d
    in
    Pattern.curve2d aCurve
        |> StateResult.map (curve2dScaleAbout center zoom >> msgAreaCurveHelp)
        |> StateResult.withDefault (Svg.g [] [])


msgAreaDetail :
    (Maybe HoveredObject -> msg)
    -> Point2d
    -> Float
    -> A Detail
    -> State Pattern (Svg msg)
msgAreaDetail objectHovered center zoom aDetail =
    let
        attributes =
            [ Svg.Attributes.stroke "transparent"
            , Svg.Attributes.fill "transparent"
            , Svg.Events.onMouseOver (objectHovered (Just (HoveredDetail aDetail)))
            , Svg.Events.onMouseOut (objectHovered Nothing)
            ]
    in
    Pattern.detail2d aDetail
        |> StateResult.map
            (detail2dScaleAbout center zoom
                >> drawDetailHelp attributes False
            )
        |> StateResult.withDefault (Svg.g [] [])


drawPoint :
    Bool
    -> Point2d
    -> Float
    -> A Point
    -> State Pattern (Svg msg)
drawPoint preview center zoom aPoint =
    Pattern.point2d aPoint
        |> StateResult.map
            (Point2d.scaleAbout center zoom
                >> drawPointHelp preview aPoint
            )
        |> StateResult.withDefault (Svg.g [] [])


drawPointHelp :
    Bool
    -> A Point
    -> Point2d
    -> Svg msg
drawPointHelp preview aPoint point2d =
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
                    , Svg.Attributes.fill "black"
                    ]
                    [ Svg.text name ]
    in
    Svg.g []
        [ Svg.circle2d
            [ Svg.Attributes.fill "none"
            , stroke Black
            , if preview then
                strokeWidthThin

              else
                strokeWidthNormal
            ]
            (Circle2d.withRadius pointRadius point2d)
        , maybeName
            |> Maybe.map drawName
            |> Maybe.withDefault (Svg.text "")
        ]


drawAxis : Bool -> Point2d -> Float -> A Axis -> State Pattern (Svg msg)
drawAxis preview center zoom aAxis =
    Pattern.axis2d aAxis
        |> StateResult.map
            (Axis2d.scaleAbout center zoom
                >> drawAxisHelp preview aAxis
            )
        |> StateResult.withDefault (Svg.g [] [])


drawAxisHelp : Bool -> A Axis -> Axis2d -> Svg msg
drawAxisHelp preview aAxis axis2d =
    Svg.lineSegment2d
        [ stroke Black
        , Svg.Attributes.opacity "0.1"
        , dashArrayNormal preview
        , if preview then
            strokeWidthThin

          else
            strokeWidthNormal
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.along axis2d -10000
            , Point2d.along axis2d 10000
            )
        )


drawCircle : Bool -> Point2d -> Float -> A Circle -> State Pattern (Svg msg)
drawCircle preview center zoom aCircle =
    Pattern.circle2d aCircle
        |> StateResult.map
            (Circle2d.scaleAbout center zoom
                >> drawCircleHelp preview aCircle
            )
        |> StateResult.withDefault (Svg.g [] [])


drawCircleHelp : Bool -> A Circle -> Circle2d -> Svg msg
drawCircleHelp preview aCircle circle2d =
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
    Pattern.curve2d aCurve
        |> StateResult.map
            (curve2dScaleAbout center zoom
                >> drawCurveHelp preview aCurve
            )
        |> StateResult.withDefault (Svg.g [] [])


drawCurveHelp : Bool -> A Curve -> Curve2d -> Svg msg
drawCurveHelp preview aCurve curve2d =
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
    Pattern.detail2d aDetail
        |> StateResult.map
            (detail2dScaleAbout center zoom
                >> drawDetailHelp
                    [ Svg.Attributes.fill "rgba(239,238,234,0.5)"
                    , strokeWidthNormal
                    , stroke Black
                    ]
                    preview
            )
        |> StateResult.withDefault (Svg.g [] [])


drawDetailHelp : List (Svg.Attribute msg) -> Bool -> Detail2d -> Svg msg
drawDetailHelp attrs preview detail2d =
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
        ((Svg.Attributes.d <|
            String.join " "
                [ "M " ++ coord detail2d.firstPoint
                , String.join " " (List.map step detail2d.nextCurves)
                , " " ++ lastStep
                ]
         )
            :: attrs
        )
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



---- DRAW


svgHoveredPoint : Point2d -> Float -> Point2d -> Svg msg
svgHoveredPoint center zoom point2d =
    Svg.circle2d
        [ Svg.Attributes.fill "rgb(217,215,205)"
        , stroke Blue
        ]
        (Circle2d.withRadius 5 <|
            Point2d.scaleAbout center zoom point2d
        )


svgHoveredCurve : Point2d -> Float -> Curve2d -> Svg msg
svgHoveredCurve center zoom curve2d =
    case curve2d of
        LineSegment2d lineSegment2d ->
            Svg.lineSegment2d
                [ stroke Blue
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (LineSegment2d.scaleAbout center zoom <|
                    lineSegment2d
                )

        QuadraticSpline2d quadraticSpline2d ->
            Svg.quadraticSpline2d
                [ stroke Blue
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (QuadraticSpline2d.scaleAbout center zoom <|
                    quadraticSpline2d
                )

        CubicSpline2d cubicSpline2d ->
            Svg.cubicSpline2d
                [ stroke Blue
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (CubicSpline2d.scaleAbout center zoom <|
                    cubicSpline2d
                )


svgHoveredDetail : Point2d -> Float -> Detail2d -> Svg msg
svgHoveredDetail center zoom detail2d =
    drawDetailHelp
        [ stroke Blue
        , Svg.Attributes.stroke "none"
        , Svg.Attributes.fill "rgba(220,134,63,0.1)"
        ]
        False
        (detail2dScaleAbout center zoom detail2d)


svgReferencedPoint : Point2d -> Float -> Point2d -> Svg msg
svgReferencedPoint center zoom point2d =
    Svg.circle2d
        [ fill Blue
        , stroke Blue
        ]
        (Circle2d.withRadius 3 <|
            Point2d.scaleAbout center zoom point2d
        )


svgReferencedCurve : Point2d -> Float -> Curve2d -> Svg msg
svgReferencedCurve center zoom curve2d =
    case curve2d of
        LineSegment2d lineSegment2d ->
            Svg.lineSegment2d
                [ stroke Blue
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (LineSegment2d.scaleAbout center zoom <|
                    lineSegment2d
                )

        QuadraticSpline2d quadraticSpline2d ->
            Svg.quadraticSpline2d
                [ stroke Blue
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (QuadraticSpline2d.scaleAbout center zoom <|
                    quadraticSpline2d
                )

        CubicSpline2d cubicSpline2d ->
            Svg.cubicSpline2d
                [ stroke Blue
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (CubicSpline2d.scaleAbout center zoom <|
                    cubicSpline2d
                )


svgReferencedIntersectable : Point2d -> Float -> Intersectable2d -> Svg msg
svgReferencedIntersectable center zoom intersectable2d =
    case intersectable2d of
        Axis2d axis2d ->
            Svg.lineSegment2d
                [ stroke Blue ]
                (LineSegment2d.scaleAbout center zoom <|
                    LineSegment2d.fromEndpoints
                        ( Point2d.along axis2d -10000
                        , Point2d.along axis2d 10000
                        )
                )

        Circle2d circle2d ->
            Svg.circle2d
                [ Svg.Attributes.fill "none"
                , stroke Blue
                ]
                (Circle2d.scaleAbout center zoom circle2d)

        Curve2d curve2d ->
            let
                attributes =
                    [ Svg.Attributes.fill "none"
                    , stroke Black
                    ]
            in
            case curve2d of
                LineSegment2d lineSegment2d ->
                    Svg.lineSegment2d attributes <|
                        LineSegment2d.scaleAbout center zoom lineSegment2d

                QuadraticSpline2d quadraticSpline2d ->
                    Svg.quadraticSpline2d attributes <|
                        QuadraticSpline2d.scaleAbout center zoom quadraticSpline2d

                CubicSpline2d cubicSpline2d ->
                    Svg.cubicSpline2d attributes <|
                        CubicSpline2d.scaleAbout center zoom cubicSpline2d


svgLabeledLineSegment : Point2d -> Float -> String -> Point2d -> Point2d -> Svg msg
svgLabeledLineSegment center zoom text startPoint2d endPoint2d =
    let
        ( textX, textY ) =
            startPoint2d
                |> Point2d.translateBy
                    (Vector2d.from startPoint2d endPoint2d
                        |> Vector2d.scaleBy 0.5
                    )
                |> Point2d.scaleAbout center zoom
                |> Point2d.coordinates

        textTransforms attrs =
            Vector2d.from startPoint2d endPoint2d
                |> Vector2d.direction
                |> Maybe.map (Direction2d.toAngle >> toDegree >> String.fromInt)
                |> Maybe.map
                    (\angle ->
                        Svg.Attributes.transform
                            (String.concat
                                [ "rotate("
                                , angle
                                , " "
                                , String.fromFloat textX
                                , " "
                                , String.fromFloat textY
                                , ")"
                                ]
                            )
                            :: attrs
                    )
                |> Maybe.withDefault attrs

        toDegree radians =
            (180 * radians / pi - 180)
                |> floor
                |> remainderBy 180
    in
    Svg.g []
        [ Svg.lineSegment2d
            [ Svg.Attributes.fill "none"
            , stroke Blue
            , dashArrayShort
            ]
            (LineSegment2d.scaleAbout center zoom <|
                LineSegment2d.from startPoint2d endPoint2d
            )
        , Svg.text_
            ([ Svg.Attributes.x (String.fromFloat textX)
             , Svg.Attributes.y (String.fromFloat textY)
             , Svg.Attributes.dy (String.fromFloat 20)
             , Svg.Attributes.textAnchor "middle"
             , fontNormal
             , fill Blue
             ]
                |> textTransforms
            )
            [ Svg.text text ]
        ]



---- HELPER


type Color
    = Blue
    | Black
    | Green


stroke color =
    Svg.Attributes.stroke <|
        case color of
            Blue ->
                "rgb(220,134,63)"

            Black ->
                "black"

            Green ->
                "green"


fill color =
    Svg.Attributes.fill <|
        case color of
            Blue ->
                "rgb(220,134,63)"

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
