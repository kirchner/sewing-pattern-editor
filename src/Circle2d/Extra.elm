module Circle2d.Extra exposing
    ( Intersection(..)
    , intersectionAxis
    , intersectionCircle
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
import Circle2d exposing (Circle2d)
import Direction2d
import Point2d exposing (Point2d)
import Vector2d


type Intersection
    = NoIntersection
    | OnePoint Point2d
    | TwoPoints Point2d Point2d


intersectionAxis : Circle2d -> Axis2d -> Intersection
intersectionAxis circle axis =
    let
        ( x, y ) =
            circle
                |> Circle2d.centerPoint
                |> Point2d.coordinates

        r =
            Circle2d.radius circle

        ( a, b ) =
            axis
                |> Axis2d.direction
                |> Direction2d.rotateCounterclockwise
                |> Direction2d.components

        ( xOrigin, yOrigin ) =
            axis
                |> Axis2d.originPoint
                |> Point2d.coordinates

        c =
            Vector2d.dotProduct
                (Vector2d.fromComponents ( a, b ))
                (Vector2d.fromComponents ( xOrigin, yOrigin ))

        cPrim =
            c - a * x - b * y

        underRoot =
            squared r * (squared a + squared b) - squared cPrim

        squared f =
            f * f
    in
    if underRoot > 0 then
        let
            root =
                sqrt underRoot

            denominator =
                squared a + squared b
        in
        TwoPoints
            (Point2d.fromCoordinates
                ( x + (a * cPrim + b * root) / denominator
                , y + (b * cPrim - a * root) / denominator
                )
            )
            (Point2d.fromCoordinates
                ( x + (a * cPrim - b * root) / denominator
                , y + (b * cPrim + a * root) / denominator
                )
            )

    else if underRoot == 0 then
        let
            denominator =
                squared a + squared b
        in
        OnePoint
            (Point2d.fromCoordinates
                ( x + a * cPrim / denominator
                , y + b * cPrim / denominator
                )
            )

    else
        NoIntersection


intersectionCircle : Circle2d -> Circle2d -> Intersection
intersectionCircle circle1 circle2 =
    let
        ( x1, y1 ) =
            circle1
                |> Circle2d.centerPoint
                |> Point2d.coordinates

        ( x2, y2 ) =
            circle2
                |> Circle2d.centerPoint
                |> Point2d.coordinates

        r1 =
            Circle2d.radius circle1

        r2 =
            Circle2d.radius circle2

        distance =
            Point2d.distanceFrom
                (Circle2d.centerPoint circle1)
                (Circle2d.centerPoint circle2)

        a =
            2 * (x2 - x1)

        b =
            2 * (y2 - y1)

        c =
            squared r1
                - squared x1
                - squared y1
                - squared r2
                + squared x2
                + squared y2

        cPrim =
            c - a * x1 - b * y1

        underRoot =
            squared r1 * (squared a + squared b) - squared cPrim

        squared f =
            f * f
    in
    if underRoot > 0 then
        let
            root =
                sqrt underRoot

            denominator =
                squared a + squared b
        in
        TwoPoints
            (Point2d.fromCoordinates
                ( x1 + (a * cPrim + b * root) / denominator
                , y1 + (b * cPrim - a * root) / denominator
                )
            )
            (Point2d.fromCoordinates
                ( x1 + (a * cPrim - b * root) / denominator
                , y1 + (b * cPrim + a * root) / denominator
                )
            )

    else if underRoot == 0 then
        let
            denominator =
                squared a + squared b
        in
        OnePoint
            (Point2d.fromCoordinates
                ( x1 + a * cPrim / denominator
                , y1 + b * cPrim / denominator
                )
            )

    else
        NoIntersection
