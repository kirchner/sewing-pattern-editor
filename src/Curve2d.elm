module Curve2d exposing
    ( Curve2d(..)
    , at, reverse, reverseIf
    , boundingBox, midpoint
    , startPoint, endPoint
    )

{-|

@docs Curve2d
@docs at, reverse, reverseIf
@docs boundingBox, midpoint
@docs startPoint, endPoint

-}

import BoundingBox2d exposing (BoundingBox2d)
import CubicSpline2d exposing (CubicSpline2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity, Rate)


type Curve2d units coordinates
    = LineSegment2d (LineSegment2d units coordinates)
    | QuadraticSpline2d (QuadraticSpline2d units coordinates)
    | CubicSpline2d (CubicSpline2d units coordinates)


at : Quantity Float (Rate units2 units1) -> Curve2d units1 coordinates -> Curve2d units2 coordinates
at conversionFactor curve2d =
    case curve2d of
        LineSegment2d lineSegment2d ->
            LineSegment2d <|
                LineSegment2d.at conversionFactor lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d <|
                QuadraticSpline2d.at conversionFactor quadraticSpline2d

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d <|
                CubicSpline2d.at conversionFactor cubicSpline2d


boundingBox : Curve2d units coordinates -> BoundingBox2d units coordinates
boundingBox curve2d =
    case curve2d of
        LineSegment2d lineSegment2d ->
            LineSegment2d.boundingBox lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d.boundingBox quadraticSpline2d

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d.boundingBox cubicSpline2d


midpoint : Curve2d units coordinates -> Point2d units coordinates
midpoint curve2d =
    case curve2d of
        LineSegment2d lineSegment2d ->
            LineSegment2d.midpoint lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d.pointOn quadraticSpline2d 0.5

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d.pointOn cubicSpline2d 0.5


startPoint : Curve2d units coordinates -> Point2d units coordinates
startPoint curve =
    case curve of
        LineSegment2d lineSegment2d ->
            LineSegment2d.startPoint lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d.startPoint quadraticSpline2d

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d.startPoint cubicSpline2d


endPoint : Curve2d units coordinates -> Point2d units coordinates
endPoint curve =
    case curve of
        LineSegment2d lineSegment2d ->
            LineSegment2d.endPoint lineSegment2d

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d.endPoint quadraticSpline2d

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d.endPoint cubicSpline2d


reverse : Curve2d units coordinates -> Curve2d units coordinates
reverse curve =
    case curve of
        LineSegment2d lineSegment2d ->
            LineSegment2d (LineSegment2d.reverse lineSegment2d)

        QuadraticSpline2d quadraticSpline2d ->
            QuadraticSpline2d (QuadraticSpline2d.reverse quadraticSpline2d)

        CubicSpline2d cubicSpline2d ->
            CubicSpline2d (CubicSpline2d.reverse cubicSpline2d)


reverseIf : Bool -> Curve2d units coordinates -> Curve2d units coordinates
reverseIf bool curve =
    if bool then
        reverse curve

    else
        curve
