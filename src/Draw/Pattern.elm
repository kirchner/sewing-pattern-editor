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
import Direction2d
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import Pattern exposing (Circle, Detail, Line, LineSegment, Pattern, Point, Segment)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import QuadraticSpline2d
import Svg exposing (Svg)
import Svg.Attributes
import That exposing (That)
import Those exposing (Those)
import Triple


draw :
    { points : Those Point
    , lines : Those Line
    , lineSegments : Those LineSegment
    , details : Those Detail
    }
    -> Bool
    -> Point2d
    -> Float
    -> Maybe (That Point)
    -> Pattern
    -> Svg msg
draw selected preview center zoom hoveredPoint pattern =
    let
        ( { details, lines, lineSegments, circles, points }, _ ) =
            Pattern.geometry pattern
    in
    Svg.g [] <|
        List.concat
            [ List.map
                (Triple.mapThird
                    (List.map
                        (\segment ->
                            case segment of
                                Pattern.LineSegment lineSegment2d ->
                                    lineSegment2d
                                        |> LineSegment2d.scaleAbout center zoom
                                        |> Pattern.LineSegment

                                Pattern.QuadraticSpline quadraticSpline2d ->
                                    quadraticSpline2d
                                        |> QuadraticSpline2d.scaleAbout center zoom
                                        |> Pattern.QuadraticSpline
                        )
                    )
                    >> drawDetail selected.details
                )
                details
            , List.map
                (Triple.mapThird (Axis2d.scaleAbout center zoom)
                    >> drawLine preview selected.lines
                )
                lines
            , List.map
                (Triple.mapThird (LineSegment2d.scaleAbout center zoom)
                    >> drawLineSegment selected.lineSegments
                )
                lineSegments
            , List.map
                (Triple.mapThird (Circle2d.scaleAbout center zoom)
                    >> drawCircle preview zoom
                )
                circles
            , List.map
                (Triple.mapThird (Point2d.scaleAbout center zoom)
                    >> drawPoint preview selected.points
                )
                points
            , [ Maybe.map (drawHoveredPoint pattern center zoom) hoveredPoint
                    |> Maybe.withDefault (Svg.text "")
              ]
            ]


drawHoveredPoint : Pattern -> Point2d -> Float -> That Point -> Svg msg
drawHoveredPoint pattern center zoom thatHoveredPoint =
    let
        drawFullPoint point2d =
            Svg.circle2d
                [ Svg.Attributes.fill "blue"
                , stroke Blue
                , strokeWidthNormal
                ]
                (Circle2d.withRadius 6 point2d)

        drawPointFilling point2d =
            Svg.circle2d
                [ Svg.Attributes.fill "blue" ]
                (Circle2d.withRadius 3 point2d)

        drawConnectingLine point2dA point2dB =
            Svg.lineSegment2d
                [ stroke Blue
                , dashArrayShort
                , strokeWidthNormal
                ]
                (LineSegment2d.fromEndpoints ( point2dA, point2dB ))

        drawCircleHighlight circle2d =
            Svg.circle2d
                [ stroke Blue
                , strokeWidthNormal
                , Svg.Attributes.fill "transparent"
                ]
                circle2d

        drawLineHighlight axis2d =
            Svg.lineSegment2d
                [ stroke Blue
                , strokeWidthNormal
                ]
                (LineSegment2d.fromEndpoints
                    ( Point2d.along axis2d -10000
                    , Point2d.along axis2d 10000
                    )
                )

        map func thatPoint =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map func
                    (Pattern.point2d pattern thatPoint
                        |> Maybe.map (Point2d.scaleAbout center zoom)
                    )

        map2 func thatPointA thatPointB =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map2 func
                    (Pattern.point2d pattern thatPointA
                        |> Maybe.map (Point2d.scaleAbout center zoom)
                    )
                    (Pattern.point2d pattern thatPointB
                        |> Maybe.map (Point2d.scaleAbout center zoom)
                    )

        mapCircle func thatCircle =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map func
                    (Pattern.circle2d pattern thatCircle
                        |> Maybe.map (Circle2d.scaleAbout center zoom)
                    )

        mapLine func thatLine =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map func
                    (Pattern.axis2d pattern thatLine
                        |> Maybe.map (Axis2d.scaleAbout center zoom)
                    )
    in
    Svg.g []
        [ map drawFullPoint thatHoveredPoint
        , case
            Maybe.map (Point2d.scaleAbout center zoom >> Point2d.coordinates)
                (Pattern.point2d pattern thatHoveredPoint)
          of
            Nothing ->
                Svg.text ""

            Just ( x, y ) ->
                Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (x - 10))
                    , Svg.Attributes.y (String.fromFloat y)
                    , Svg.Attributes.dy (String.fromFloat -10)
                    , Svg.Attributes.textAnchor "middle"
                    , fontNormal
                    , Svg.Attributes.fill "blue"
                    ]
                    [ Maybe.andThen .name (Pattern.getPoint pattern thatHoveredPoint)
                        |> Maybe.withDefault ""
                        |> Svg.text
                    ]
        , case Maybe.map .value (Pattern.getPoint pattern thatHoveredPoint) of
            Just (Pattern.Origin x y) ->
                drawPointFilling
                    (Point2d.scaleAbout center zoom <|
                        Point2d.fromCoordinates ( x, y )
                    )

            Just (Pattern.LeftOf thatAnchorPoint _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPoint
                    , map2 drawConnectingLine thatAnchorPoint thatHoveredPoint
                    ]

            Just (Pattern.RightOf thatAnchorPoint _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPoint
                    , map2 drawConnectingLine thatAnchorPoint thatHoveredPoint
                    ]

            Just (Pattern.Above thatAnchorPoint _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPoint
                    , map2 drawConnectingLine thatAnchorPoint thatHoveredPoint
                    ]

            Just (Pattern.Below thatAnchorPoint _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPoint
                    , map2 drawConnectingLine thatAnchorPoint thatHoveredPoint
                    ]

            Just (Pattern.AtAngle thatAnchorPoint _ _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPoint
                    , map2 drawConnectingLine thatAnchorPoint thatHoveredPoint
                    ]

            Just (Pattern.BetweenRatio thatAnchorPointA thatAnchorPointB _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPointA
                    , map drawPointFilling thatAnchorPointB
                    , map2 drawConnectingLine thatAnchorPointA thatHoveredPoint
                    , map2 drawConnectingLine thatAnchorPointB thatHoveredPoint
                    ]

            Just (Pattern.BetweenLength thatAnchorPointA thatAnchorPointB _) ->
                Svg.g []
                    [ map drawPointFilling thatAnchorPointA
                    , map drawPointFilling thatAnchorPointB
                    , map2 drawConnectingLine thatAnchorPointA thatHoveredPoint
                    , map2 drawConnectingLine thatAnchorPointB thatHoveredPoint
                    ]

            Just (Pattern.FirstCircleCircle thatCircleA thatCircleB) ->
                Svg.g []
                    [ mapCircle drawCircleHighlight thatCircleA
                    , mapCircle drawCircleHighlight thatCircleB
                    ]

            Just (Pattern.SecondCircleCircle thatCircleA thatCircleB) ->
                Svg.g []
                    [ mapCircle drawCircleHighlight thatCircleA
                    , mapCircle drawCircleHighlight thatCircleB
                    ]

            Just (Pattern.LineLine thatLineA thatLineB) ->
                Svg.g []
                    [ mapLine drawLineHighlight thatLineA
                    , mapLine drawLineHighlight thatLineB
                    ]

            Just (Pattern.FirstCircleLine thatCircle thatLine) ->
                Svg.g []
                    [ mapCircle drawCircleHighlight thatCircle
                    , mapLine drawLineHighlight thatLine
                    ]

            Just (Pattern.SecondCircleLine thatCircle thatLine) ->
                Svg.g []
                    [ mapCircle drawCircleHighlight thatCircle
                    , mapLine drawLineHighlight thatLine
                    ]

            _ ->
                Svg.text ""
        ]


drawPoint : Bool -> Those Point -> ( That Point, Maybe String, Point2d ) -> Svg msg
drawPoint preview selectedPoints ( thatPoint, maybeName, point2d ) =
    let
        pointRadius =
            if preview then
                1

            else
                5

        ( x, y ) =
            Point2d.coordinates point2d

        selected =
            Those.member thatPoint selectedPoints

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
        , maybeName
            |> Maybe.map drawName
            |> Maybe.withDefault (Svg.text "")
        ]


drawLine : Bool -> Those Line -> ( That Line, Maybe String, Axis2d ) -> Svg msg
drawLine preview selectedLines ( thatLine, maybeName, axis2d ) =
    Svg.lineSegment2d
        (if Those.member thatLine selectedLines then
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


drawLineSegment :
    Those LineSegment
    -> ( That LineSegment, Maybe String, LineSegment2d )
    -> Svg msg
drawLineSegment selectedLineSegments ( thatLineSegment, maybeName, lineSegment2d ) =
    let
        selected =
            Those.member thatLineSegment selectedLineSegments
    in
    Svg.lineSegment2d
        (if selected then
            [ stroke Green
            , Svg.Attributes.opacity "1"
            , strokeWidthBold
            ]

         else
            [ stroke Black
            , Svg.Attributes.opacity "0.1"
            , strokeWidthNormal
            ]
        )
        lineSegment2d


drawCircle : Bool -> Float -> ( That Circle, Maybe String, Circle2d ) -> Svg msg
drawCircle preview zoom ( thatCircle, maybeName, circle2d ) =
    Svg.circle2d
        [ stroke Black
        , Svg.Attributes.opacity "0.1"
        , dashArrayNormal preview
        , strokeWidthNormal
        , Svg.Attributes.fill "transparent"
        ]
        circle2d


drawDetail : Those Detail -> ( That Detail, Maybe String, List Segment ) -> Svg msg
drawDetail selectedDetails ( thatDetail, maybeName, segments ) =
    let
        selected =
            Those.member thatDetail selectedDetails
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
            case segments of
                [] ->
                    ""

                firstSegment :: rest ->
                    let
                        ( startX, startY ) =
                            case firstSegment of
                                Pattern.LineSegment lineSegment2d ->
                                    lineSegment2d
                                        |> LineSegment2d.startPoint
                                        |> Point2d.coordinates

                                Pattern.QuadraticSpline quadraticSpline2d ->
                                    quadraticSpline2d
                                        |> QuadraticSpline2d.startPoint
                                        |> Point2d.coordinates
                    in
                    String.join " "
                        [ "M " ++ String.fromFloat startX ++ " " ++ String.fromFloat startY
                        , String.join " " <|
                            List.map
                                (\segment ->
                                    case segment of
                                        Pattern.LineSegment lineSegment2d ->
                                            let
                                                ( x, y ) =
                                                    lineSegment2d
                                                        |> LineSegment2d.endPoint
                                                        |> Point2d.coordinates
                                            in
                                            String.concat
                                                [ "L "
                                                , String.fromFloat x
                                                , " "
                                                , String.fromFloat y
                                                ]

                                        Pattern.QuadraticSpline quadraticSpline2d ->
                                            let
                                                ( x, y ) =
                                                    quadraticSpline2d
                                                        |> QuadraticSpline2d.endPoint
                                                        |> Point2d.coordinates

                                                ( controlX, controlY ) =
                                                    quadraticSpline2d
                                                        |> QuadraticSpline2d.controlPoint
                                                        |> Point2d.coordinates
                                            in
                                            String.concat
                                                [ "Q "
                                                , String.fromFloat controlX
                                                , " "
                                                , String.fromFloat controlY
                                                , ", "
                                                , String.fromFloat x
                                                , " "
                                                , String.fromFloat y
                                                ]
                                )
                                segments
                        ]
        ]
        []



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
