module Detail2d exposing (Detail2d, LastCurve2d(..), NextCurve2d(..))

import Point2d exposing (Point2d)


type alias Detail2d u c =
    { firstPoint : Point2d u c
    , nextCurves : List (NextCurve2d u c)
    , lastCurve : LastCurve2d u c
    }


type NextCurve2d u c
    = NextLineSegment2d
        { endPoint : Point2d u c
        }
    | NextQuadraticSpline2d
        { controlPoint : Point2d u c
        , endPoint : Point2d u c
        }
    | NextCubicSpline2d
        { startControlPoint : Point2d u c
        , endControlPoint : Point2d u c
        , endPoint : Point2d u c
        }


type LastCurve2d u c
    = LastLineSegment2d
    | LastQuadraticSpline2d
        { controlPoint : Point2d u c
        }
    | LastCubicSpline2d
        { startControlPoint : Point2d u c
        , endControlPoint : Point2d u c
        }
