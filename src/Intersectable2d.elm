module Intersectable2d exposing (Intersectable2d(..))

{-|

@docs Intersectable2d

-}

import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Curve2d exposing (Curve2d(..))


{-| -}
type Intersectable2d units coordinates
    = Axis2d (Axis2d units coordinates)
    | Circle2d (Circle2d units coordinates)
    | Curve2d (Curve2d units coordinates)
