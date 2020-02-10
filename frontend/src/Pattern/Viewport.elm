module Pattern.Viewport exposing
    ( Viewport, Resolution, Dimensions
    , idealForBoundingBox, idealForPoints, idealForPattern
    )

{-|

@docs Viewport, Resolution, Dimensions
@docs idealForBoundingBox, idealForPoints, idealForPattern

-}

import BoundingBox2d exposing (BoundingBox2d)
import Length exposing (Meters)
import Pattern exposing (Pattern)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import State
import StateResult


{-| -}
type alias Viewport coordinates =
    { resolution : Resolution
    , center : Point2d Meters coordinates
    }


{-| -}
type alias Resolution =
    Quantity Float (Rate Pixels Meters)


{-| -}
type alias Dimensions =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }


idealForBoundingBox : Dimensions -> BoundingBox2d Meters coordinates -> Viewport coordinates
idealForBoundingBox dimensions boundingBox2d =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox2d

        width =
            Length.inMeters maxX - Length.inMeters minX

        height =
            Length.inMeters maxY - Length.inMeters minY

        idealHorizontalResolution =
            dimensions.height
                |> Quantity.per (Length.meters width)

        idealVerticalResolution =
            dimensions.width
                |> Quantity.per (Length.meters height)
    in
    { resolution = Quantity.min idealHorizontalResolution idealVerticalResolution
    , center = BoundingBox2d.centerPoint boundingBox2d
    }


{-| -}
idealForPoints :
    Dimensions
    -> Point2d Meters coordinates
    -> List (Point2d Meters coordinates)
    -> Viewport coordinates
idealForPoints dimensions point2d point2ds =
    idealForBoundingBox dimensions (BoundingBox2d.hull point2d point2ds)


{-| -}
idealForPattern : Dimensions -> Pattern coordinates -> Maybe (Viewport coordinates)
idealForPattern dimensions pattern =
    let
        point2ds =
            Pattern.points pattern
                |> StateResult.traverse Pattern.point2d
                |> State.finalValue pattern
    in
    case point2ds of
        Ok (first :: rest) ->
            Just (idealForBoundingBox dimensions (BoundingBox2d.hull first rest))

        _ ->
            Nothing
