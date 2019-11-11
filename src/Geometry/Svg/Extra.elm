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
                NextLineSegment2d { endPoint } ->
                    "L " ++ coordinatesString endPoint

                NextQuadraticSpline2d { secondControlPoint, thirdControlPoint } ->
                    "Q "
                        ++ coordinatesString secondControlPoint
                        ++ ", "
                        ++ coordinatesString thirdControlPoint

                NextCubicSpline2d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
                    "C "
                        ++ coordinatesString secondControlPoint
                        ++ ", "
                        ++ coordinatesString thirdControlPoint
                        ++ ", "
                        ++ coordinatesString fourthControlPoint

        lastStep =
            case lastCurve of
                LastLineSegment2d ->
                    "L " ++ coordinatesString firstPoint

                LastQuadraticSpline2d { secondControlPoint } ->
                    "Q "
                        ++ coordinatesString secondControlPoint
                        ++ ", "
                        ++ coordinatesString firstPoint

                LastCubicSpline2d { secondControlPoint, thirdControlPoint } ->
                    "C "
                        ++ coordinatesString secondControlPoint
                        ++ ", "
                        ++ coordinatesString thirdControlPoint
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
