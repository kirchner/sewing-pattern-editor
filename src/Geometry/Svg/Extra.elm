module Geometry.Svg.Extra exposing (detail2d)

import Detail2d exposing (Detail2d, LastCurve2d(..), NextCurve2d(..))
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes


detail2d : List (Svg.Attribute msg) -> Detail2d Pixels coordinates -> Svg msg
detail2d attributes { firstPoint, nextCurves, lastCurve } =
    let
        step nextCurve2d =
            case nextCurve2d of
                NextLineSegment2d stuff ->
                    "L " ++ coordinatesString stuff.endPoint

                NextQuadraticSpline2d stuff ->
                    "Q "
                        ++ coordinatesString stuff.controlPoint
                        ++ ", "
                        ++ coordinatesString stuff.endPoint

                NextCubicSpline2d stuff ->
                    "C "
                        ++ coordinatesString stuff.startControlPoint
                        ++ ", "
                        ++ coordinatesString stuff.endControlPoint
                        ++ ", "
                        ++ coordinatesString stuff.endPoint

        lastStep =
            case lastCurve of
                LastLineSegment2d ->
                    "L " ++ coordinatesString firstPoint

                LastQuadraticSpline2d stuff ->
                    "Q "
                        ++ coordinatesString stuff.controlPoint
                        ++ ", "
                        ++ coordinatesString firstPoint

                LastCubicSpline2d stuff ->
                    "C "
                        ++ coordinatesString stuff.startControlPoint
                        ++ ", "
                        ++ coordinatesString stuff.endControlPoint
                        ++ ", "
                        ++ coordinatesString firstPoint
    in
    Svg.path
        ((Svg.Attributes.d <|
            String.join " "
                [ "M " ++ coordinatesString firstPoint
                , String.join " " (List.map step nextCurves)
                , " " ++ lastStep
                ]
         )
            :: attributes
        )
        []


coordinatesString : Point2d Pixels coordinates -> String
coordinatesString point =
    let
        { x, y } =
            Point2d.toPixels point
    in
    String.fromFloat x ++ "," ++ String.fromFloat y
