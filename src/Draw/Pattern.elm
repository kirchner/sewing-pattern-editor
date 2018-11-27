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
            , List.map (drawPoint preview zoom pattern hoveredPoint selected.points) geometry.points
            ]


drawPoint :
    Bool
    -> Float
    -> Pattern
    -> Maybe (That Point)
    -> Those Point
    -> ( That Point, Maybe String, Point2d )
    -> Svg msg
drawPoint preview zoom pattern hoveredPoint selectedPoints data =
    let
        ( thatPoint, maybeName, point2d ) =
            data

        pointRadius =
            if preview then
                3

            else
                1

        ( x, y ) =
            Point2d.coordinates point2d

        hovered =
            hoveredPoint
                |> Maybe.map (That.areEqual thatPoint)
                |> Maybe.withDefault False

        selected =
            Those.member thatPoint selectedPoints

        helper =
            if hovered then
                Svg.g []
                    [ case
                        hoveredPoint
                            |> Maybe.andThen (Pattern.getPoint pattern)
                            |> Maybe.map .value
                      of
                        Just (Pattern.LeftOf thatAnchorPoint _) ->
                            drawAnchorLine thatAnchorPoint hoveredPoint

                        Just (Pattern.RightOf thatAnchorPoint _) ->
                            drawAnchorLine thatAnchorPoint hoveredPoint

                        Just (Pattern.Above thatAnchorPoint _) ->
                            drawAnchorLine thatAnchorPoint hoveredPoint

                        Just (Pattern.Below thatAnchorPoint _) ->
                            drawAnchorLine thatAnchorPoint hoveredPoint

                        Just (Pattern.AtAngle thatAnchorPoint _ _) ->
                            drawAnchorLine thatAnchorPoint hoveredPoint

                        _ ->
                            Svg.text ""
                    , case
                        hoveredPoint
                            |> Maybe.map (Pattern.getPointGeometries pattern)
                      of
                        Nothing ->
                            Svg.text ""

                        Just point2ds ->
                            drawPointChain point2ds
                    ]

            else
                Svg.text ""

        drawAnchorLine thatAnchorPoint maybeHoveredPoint =
            Maybe.map2
                (\point2dA point2dB ->
                    Svg.g
                        []
                        [ Svg.lineSegment2d
                            [ stroke Blue
                            , dashArrayShort zoom
                            , strokeWidthNormal zoom
                            ]
                            (LineSegment2d.fromEndpoints ( point2dA, point2dB ))
                        , Svg.circle2d
                            [ Svg.Attributes.fill "blue" ]
                            (Circle2d.withRadius (3 / zoom) point2dA)
                        ]
                )
                (Maybe.andThen (Pattern.point2d pattern) maybeHoveredPoint)
                (Pattern.point2d pattern thatAnchorPoint)
                |> Maybe.withDefault (Svg.text "")

        drawPointChain points =
            List.foldl drawLink ( Nothing, [] ) points
                |> Tuple.second
                |> Svg.g []

        drawLink point ( maybePreviousPoint, links ) =
            ( Just point
            , (case maybePreviousPoint of
                Nothing ->
                    Svg.g []
                        [ Svg.circle2d [ Svg.Attributes.fill "blue" ]
                            (Circle2d.withRadius (3 / zoom) point)
                        ]

                Just previousPoint ->
                    let
                        startPoint =
                            previousPoint

                        midpoint =
                            LineSegment2d.from previousPoint point
                                |> LineSegment2d.midpoint

                        controlPoint =
                            case Direction2d.from previousPoint point of
                                Nothing ->
                                    midpoint

                                Just direction ->
                                    Point2d.along
                                        (Axis2d.through midpoint (Direction2d.perpendicularTo direction))
                                        (Point2d.squaredDistanceFrom startPoint endPoint / 1500)

                        endPoint =
                            point

                        spline =
                            QuadraticSpline2d.with
                                { startPoint = startPoint
                                , controlPoint = controlPoint
                                , endPoint = endPoint
                                }

                        id =
                            midpoint
                                |> Point2d.coordinates
                                |> (\( s, t ) ->
                                        String.join "-"
                                            [ String.fromFloat s
                                            , String.fromFloat t
                                            ]
                                   )
                    in
                    Svg.g []
                        [ Svg.mask
                            [ Svg.Attributes.id ("circleMask-" ++ id) ]
                            [ Svg.boundingBox2d
                                [ Svg.Attributes.fill "white" ]
                                (QuadraticSpline2d.boundingBox spline)
                            , Svg.circle2d
                                [ Svg.Attributes.fill "black" ]
                                (Circle2d.withRadius (15 / zoom) startPoint)
                            , Svg.circle2d
                                [ Svg.Attributes.fill "black" ]
                                (Circle2d.withRadius (15 / zoom) endPoint)
                            ]
                        , Svg.quadraticSpline2d
                            [ stroke Blue
                            , dashArrayNormal zoom
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.markerEnd "url(#arrow)"
                            , Svg.Attributes.mask ("url(#circleMask-" ++ id ++ ")")
                            ]
                            spline
                        , Svg.circle2d [ Svg.Attributes.fill "blue" ]
                            (Circle2d.withRadius zoom point)
                        ]
              )
                :: links
            )
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
        , helper
        , maybeName
            |> Maybe.map
                (\name ->
                    if preview then
                        Svg.text_
                            [ Svg.Attributes.x (String.fromFloat x)
                            , Svg.Attributes.y (String.fromFloat y)
                            , Svg.Attributes.dy (String.fromFloat (-10 / zoom))
                            , Svg.Attributes.textAnchor "middle"
                            , fontNormal zoom
                            ]
                            [ Svg.text name ]

                    else
                        Svg.text ""
                )
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
        [ Svg.Attributes.stroke <|
            if selected then
                "blue"

            else
                "black"
        , Svg.Attributes.opacity <|
            if selected then
                "1"

            else
                "0.1"
        ]
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
