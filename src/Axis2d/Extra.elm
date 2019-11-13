module Axis2d.Extra exposing
    ( throughTwoPoints
    , intersectionWithAxis
    , scaleAbout
    )

{-|

@docs throughTwoPoints
@docs intersectionWithAxis
@docs scaleAbout

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

import Angle
import Axis2d exposing (Axis2d)
import Direction2d
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Vector2d


{-| -}
throughTwoPoints : Point2d Meters c -> Point2d Meters c -> Maybe (Axis2d Meters c)
throughTwoPoints pointA pointB =
    Direction2d.from pointA pointB
        |> Maybe.map (Axis2d.through pointA)


{-| -}
intersectionWithAxis : Axis2d Meters c -> Axis2d Meters c -> Maybe (Point2d Meters c)
intersectionWithAxis axis1 axis2 =
    let
        ( x1, y1 ) =
            axis1
                |> Axis2d.originPoint
                |> Point2d.toTuple Length.inMeters

        ( x2, y2 ) =
            Point2d.along axis1 (Length.meters 1)
                |> Point2d.toTuple Length.inMeters

        ( x3, y3 ) =
            axis2
                |> Axis2d.originPoint
                |> Point2d.toTuple Length.inMeters

        ( x4, y4 ) =
            Point2d.along axis2 (Length.meters 1)
                |> Point2d.toTuple Length.inMeters

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
            (Point2d.fromTuple Length.meters
                ( denominatorX / nominator
                , denominatorY / nominator
                )
            )


{-| -}
scaleAbout : Point2d Meters c -> Float -> Axis2d Meters c -> Axis2d Meters c
scaleAbout point factor axis =
    Axis2d.originPoint axis
        |> Point2d.scaleAbout point factor
        |> Axis2d.withDirection (Axis2d.direction axis)
