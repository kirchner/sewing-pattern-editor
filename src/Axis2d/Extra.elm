module Axis2d.Extra exposing (intersectionWithAxis)

import Axis2d exposing (Axis2d)
import Direction2d
import Point2d exposing (Point2d)
import Vector2d


intersectionWithAxis : Axis2d -> Axis2d -> Maybe Point2d
intersectionWithAxis axis1 axis2 =
    let
        ( a1, b1 ) =
            axis1
                |> Axis2d.direction
                |> Direction2d.rotateCounterclockwise
                |> Direction2d.components

        ( xOrigin1, yOrigin1 ) =
            axis1
                |> Axis2d.originPoint
                |> Point2d.coordinates

        c1 =
            Vector2d.dotProduct
                (Vector2d.fromComponents ( a1, b1 ))
                (Vector2d.fromComponents ( xOrigin1, yOrigin1 ))

        ( a2, b2 ) =
            axis2
                |> Axis2d.direction
                |> Direction2d.rotateCounterclockwise
                |> Direction2d.components

        ( xOrigin2, yOrigin2 ) =
            axis2
                |> Axis2d.originPoint
                |> Point2d.coordinates

        c2 =
            Vector2d.dotProduct
                (Vector2d.fromComponents ( a2, b2 ))
                (Vector2d.fromComponents ( xOrigin2, yOrigin2 ))

        determinant =
            a1 * b2 - b1 * a2
    in
    if determinant == 0 then
        Nothing

    else
        let
            inverseDeterminant =
                1 / determinant
        in
        Just
            (Point2d.fromCoordinates
                ( inverseDeterminant * (c1 * b2 - c1 * b1)
                , inverseDeterminant * (c2 * a1 - c2 * a2)
                )
            )
