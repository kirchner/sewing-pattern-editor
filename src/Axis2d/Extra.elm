module Axis2d.Extra exposing (intersectionWithAxis)

import Axis2d exposing (Axis2d)
import Direction2d
import Point2d exposing (Point2d)
import Vector2d


intersectionWithAxis : Axis2d -> Axis2d -> Maybe Point2d
intersectionWithAxis axis1 axis2 =
    let
        ( x1, y1 ) =
            axis1
                |> Axis2d.originPoint
                |> Point2d.coordinates

        ( x2, y2 ) =
            Point2d.along axis1 1
                |> Point2d.coordinates

        ( x3, y3 ) =
            axis2
                |> Axis2d.originPoint
                |> Point2d.coordinates

        ( x4, y4 ) =
            Point2d.along axis2 1
                |> Point2d.coordinates

        nominator =
            det (x1 - x2) (y1 - y2) (x3 - x4) (y3 - y4)

        det a b c d =
            (a * d) - b * c
    in
    if nominator == 0 then
        Nothing

    else
        let
            denominatorX =
                det (det x1 y1 x2 y2) (x1 - x2) (det x3 y3 x4 y4) (x3 - x4)

            denominatorY =
                det (det x1 y1 x2 y2) (y1 - y2) (det x3 y3 x4 y4) (y3 - y4)
        in
        Just
            (Point2d.fromCoordinates
                ( denominatorX / nominator
                , denominatorY / nominator
                )
            )
