module Point2d.Extra exposing
    ( atAngle
    , betweenLength
    , betweenRatio
    , firstCircleAxis
    , firstCircleCircle
    , secondCircleAxis
    , secondCircleCircle
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
import Circle2d.Extra as Circle2d exposing (Intersection(..))
import Direction2d
import Point2d exposing (Point2d)
import Vector2d


atAngle : Point2d -> Float -> Float -> Point2d
atAngle anchor angle distance =
    Point2d.translateBy
        (Vector2d.fromPolarComponents
            ( distance, degrees angle )
        )
        anchor


betweenRatio : Point2d -> Point2d -> Float -> Point2d
betweenRatio anchorA anchorB ratio =
    Point2d.translateBy
        (Vector2d.from anchorA anchorB
            |> Vector2d.scaleBy ratio
        )
        anchorA


betweenLength : Point2d -> Point2d -> Float -> Maybe Point2d
betweenLength anchorA anchorB length =
    Direction2d.from anchorA anchorB
        |> Maybe.map
            (\direction ->
                Point2d.along
                    (Axis2d.through anchorA direction)
                    length
            )


firstCircleCircle : Circle2d -> Circle2d -> Maybe Point2d
firstCircleCircle circle2dA circle2dB =
    case Circle2d.intersectionCircle circle2dA circle2dB of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints point _ ->
            Just point


secondCircleCircle : Circle2d -> Circle2d -> Maybe Point2d
secondCircleCircle circle2dA circle2dB =
    case Circle2d.intersectionCircle circle2dA circle2dB of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints _ point ->
            Just point


firstCircleAxis : Circle2d -> Axis2d -> Maybe Point2d
firstCircleAxis circle axis =
    case Circle2d.intersectionAxis circle axis of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints point _ ->
            Just point


secondCircleAxis : Circle2d -> Axis2d -> Maybe Point2d
secondCircleAxis circle axis =
    case Circle2d.intersectionAxis circle axis of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints _ point ->
            Just point
