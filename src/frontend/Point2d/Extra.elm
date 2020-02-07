module Point2d.Extra exposing
    ( atAngle, betweenLength, betweenRatio
    , firstCircleCircle, secondCircleCircle
    , firstCircleAxis, secondCircleAxis
    )

{-|

@docs atAngle, betweenLength, betweenRatio
@docs firstCircleCircle, secondCircleCircle
@docs firstCircleAxis, secondCircleAxis

-}

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

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Circle2d.Extra as Circle2d exposing (Intersection(..))
import Direction2d
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Vector2d


{-| -}
atAngle : Point2d Meters c -> Angle -> Length -> Point2d Meters c
atAngle anchor angle distance =
    Point2d.translateBy (Vector2d.rTheta distance angle) anchor


{-| -}
betweenRatio : Point2d Meters c -> Point2d Meters c -> Float -> Point2d Meters c
betweenRatio anchorA anchorB ratio =
    Point2d.translateBy
        (Vector2d.from anchorA anchorB
            |> Vector2d.scaleBy ratio
        )
        anchorA


{-| -}
betweenLength : Point2d Meters c -> Point2d Meters c -> Length -> Maybe (Point2d Meters c)
betweenLength anchorA anchorB length =
    Direction2d.from anchorA anchorB
        |> Maybe.map
            (\direction ->
                Point2d.along
                    (Axis2d.through anchorA direction)
                    length
            )


{-| -}
firstCircleCircle : Circle2d Meters c -> Circle2d Meters c -> Maybe (Point2d Meters c)
firstCircleCircle circle2dA circle2dB =
    case Circle2d.intersectionCircle circle2dA circle2dB of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints point _ ->
            Just point


{-| -}
secondCircleCircle : Circle2d Meters c -> Circle2d Meters c -> Maybe (Point2d Meters c)
secondCircleCircle circle2dA circle2dB =
    case Circle2d.intersectionCircle circle2dA circle2dB of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints _ point ->
            Just point


{-| -}
firstCircleAxis : Circle2d Meters c -> Axis2d Meters c -> Maybe (Point2d Meters c)
firstCircleAxis circle axis =
    case Circle2d.intersectionAxis circle axis of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints point _ ->
            Just point


{-| -}
secondCircleAxis : Circle2d Meters c -> Axis2d Meters c -> Maybe (Point2d Meters c)
secondCircleAxis circle axis =
    case Circle2d.intersectionAxis circle axis of
        NoIntersection ->
            Nothing

        OnePoint point ->
            Just point

        TwoPoints _ point ->
            Just point
