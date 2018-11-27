module Draw.Pattern exposing (draw)

import Axis2d exposing (Axis2d)
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


draw :
    { points : Those Point
    , lines : Those Line
    , lineSegments : Those LineSegment
    , details : Those Detail
    }
    -> Bool
    -> Float
    -> Maybe (That Point)
    -> Pattern
    -> Svg msg
draw selected preview zoom hoveredPoint pattern =
    let
        ( geometry, problems ) =
            Pattern.geometry pattern
    in
    Svg.g [] <|
        List.concat
            [ [ Svg.defs []
                    [ Svg.marker
                        [ Svg.Attributes.id "arrow"
                        , Svg.Attributes.viewBox "0 0 10 10"
                        , Svg.Attributes.refX "5"
                        , Svg.Attributes.refY "5"
                        , Svg.Attributes.markerWidth "6"
                        , Svg.Attributes.markerHeight "6"
                        , Svg.Attributes.orient "auto-start-reverse"
                        , Svg.Attributes.fill "blue"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.d "M 0 0 L 10 5 L 0 10 z" ]
                            []
                        ]
                    ]
              ]
            , List.map (drawDetail zoom selected.details) geometry.details
            , List.map (drawLine zoom selected.lines) geometry.lines
            , List.map (drawLineSegment selected.lineSegments) geometry.lineSegments
            , List.map (drawCircle zoom) geometry.circles
            , List.map (drawPoint preview zoom pattern selected.points) geometry.points
            , [ Maybe.map (drawHoveredPoint pattern zoom) hoveredPoint
                    |> Maybe.withDefault (Svg.text "")
              ]
            ]


drawHoveredPoint : Pattern -> Float -> That Point -> Svg msg
drawHoveredPoint pattern zoom thatHoveredPoint =
    let
        drawFullPoint point2d =
            Svg.circle2d
                [ Svg.Attributes.fill "blue"
                , stroke Blue
                , strokeWidthNormal zoom
                ]
                (Circle2d.withRadius (6 / zoom) point2d)

        drawPointFilling point2d =
            Svg.circle2d
                [ Svg.Attributes.fill "blue" ]
                (Circle2d.withRadius (3 / zoom) point2d)

        drawConnectingLine point2dA point2dB =
            Svg.lineSegment2d
                [ stroke Blue
                , dashArrayShort zoom
                , strokeWidthNormal zoom
                ]
                (LineSegment2d.fromEndpoints ( point2dA, point2dB ))

        drawCircleHighlight circle2d =
            Svg.circle2d
                [ stroke Blue
                , strokeWidthNormal zoom
                , Svg.Attributes.fill "transparent"
                ]
                circle2d

        drawLineHighlight axis2d =
            Svg.lineSegment2d
                [ stroke Blue
                , strokeWidthNormal zoom
                ]
                (LineSegment2d.fromEndpoints
                    ( Point2d.along axis2d -10000
                    , Point2d.along axis2d 10000
                    )
                )

        map func thatPoint =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map func (Pattern.point2d pattern thatPoint)

        map2 func thatPointA thatPointB =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map2 func
                    (Pattern.point2d pattern thatPointA)
                    (Pattern.point2d pattern thatPointB)

        mapCircle func thatCircle =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map func (Pattern.circle2d pattern thatCircle)

        mapLine func thatLine =
            Maybe.withDefault (Svg.text "") <|
                Maybe.map func (Pattern.axis2d pattern thatLine)
    in
    Svg.g []
        [ map drawFullPoint thatHoveredPoint
        , case Maybe.map .value (Pattern.getPoint pattern thatHoveredPoint) of
            Just (Pattern.Origin x y) ->
                drawPointFilling (Point2d.fromCoordinates ( x, y ))

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
                    , mapCircle drawCircleHighlight thatCircleA
                    ]

            Just (Pattern.SecondCircleCircle thatCircleA thatCircleB) ->
                Svg.g []
                    [ mapCircle drawCircleHighlight thatCircleA
                    , mapCircle drawCircleHighlight thatCircleA
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


drawPoint :
    Bool
    -> Float
    -> Pattern
    -> Those Point
    -> ( That Point, Maybe String, Point2d )
    -> Svg msg
drawPoint preview zoom pattern selectedPoints ( thatPoint, maybeName, point2d ) =
    let
        pointRadius =
            if preview then
                3

            else
                1

        ( x, y ) =
            Point2d.coordinates point2d

        selected =
            Those.member thatPoint selectedPoints

        drawName name =
            if preview then
                Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (x - 10 / zoom))
                    , Svg.Attributes.y (String.fromFloat y)
                    , Svg.Attributes.dy (String.fromFloat (-10 / zoom))
                    , Svg.Attributes.textAnchor "middle"
                    , fontNormal zoom
                    ]
                    [ Svg.text name ]

            else
                Svg.text ""
    in
    Svg.g []
        [ if selected then
            Svg.circle2d
                [ stroke Blue
                , Svg.Attributes.fill "none"
                , strokeWidthNormal zoom
                ]
                (Circle2d.withRadius (10 / zoom) point2d)

          else
            Svg.circle2d
                [ Svg.Attributes.fill "none"
                , stroke Black
                , strokeWidthNormal zoom
                ]
                (Circle2d.withRadius (5 / zoom) point2d)
        , maybeName
            |> Maybe.map drawName
            |> Maybe.withDefault (Svg.text "")
        ]


drawLine : Float -> Those Line -> ( That Line, Maybe String, Axis2d ) -> Svg msg
drawLine zoom selectedLines ( thatLine, maybeName, axis2d ) =
    Svg.lineSegment2d
        (strokeWidthNormal zoom
            :: (if Those.member thatLine selectedLines then
                    [ stroke Blue
                    , Svg.Attributes.opacity "1"
                    ]

                else
                    [ stroke Black
                    , Svg.Attributes.opacity "0.1"
                    , dashArrayNormal zoom
                    ]
               )
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
            [ stroke Blue
            , Svg.Attributes.opacity "1"
            ]

         else
            [ stroke Black
            , Svg.Attributes.opacity "0.1"
            ]
        )
        lineSegment2d


drawCircle : Float -> ( That Circle, Maybe String, Circle2d ) -> Svg msg
drawCircle zoom ( thatCircle, maybeName, circle2d ) =
    Svg.circle2d
        [ stroke Black
        , Svg.Attributes.opacity "0.1"
        , dashArrayNormal zoom
        , strokeWidthNormal zoom
        , Svg.Attributes.fill "transparent"
        ]
        circle2d


drawDetail : Float -> Those Detail -> ( That Detail, Maybe String, List Segment ) -> Svg msg
drawDetail zoom selectedDetails ( thatDetail, maybeName, segments ) =
    let
        selected =
            Those.member thatDetail selectedDetails
    in
    Svg.path
        [ Svg.Attributes.fill "#EFEFEF"
        , strokeWidthNormal zoom
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


stroke color =
    Svg.Attributes.stroke <|
        case color of
            Blue ->
                "blue"

            Black ->
                "black"


strokeWidthNormal zoom =
    Svg.Attributes.strokeWidth <|
        String.fromFloat (1.5 / zoom)


dashArrayShort zoom =
    Svg.Attributes.strokeDasharray <|
        String.fromFloat (20 / zoom)
            ++ " "
            ++ String.fromFloat (10 / zoom)


dashArrayNormal zoom =
    Svg.Attributes.strokeDasharray <|
        String.fromFloat (40 / zoom)
            ++ " "
            ++ String.fromFloat (20 / zoom)


fontNormal zoom =
    Svg.Attributes.style <|
        "font: "
            ++ String.fromInt (Basics.floor (20 / zoom))
            ++ "px sans-serif; font-family: \"Roboto\";"
