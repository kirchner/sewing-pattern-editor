module Detail2d exposing
    ( Detail2d, LastCurve2d(..), NextCurve2d(..)
    , at
    , boundingBox
    )

{-|

@docs Detail2d, LastCurve2d, NextCurve2d
@docs at
@docs boundingBox

-}

import BoundingBox2d exposing (BoundingBox2d)
import List.Extra as List
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)


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
        { secondControlPoint : Point2d u c
        , thirdControlPoint : Point2d u c
        }
    | NextCubicSpline2d
        { secondControlPoint : Point2d u c
        , thirdControlPoint : Point2d u c
        , fourthControlPoint : Point2d u c
        }


type LastCurve2d u c
    = LastLineSegment2d
    | LastQuadraticSpline2d
        { secondControlPoint : Point2d u c
        }
    | LastCubicSpline2d
        { secondControlPoint : Point2d u c
        , thirdControlPoint : Point2d u c
        }


at :
    Quantity Float (Rate units2 units1)
    -> Detail2d units1 coordinates
    -> Detail2d units2 coordinates
at conversionFactor { firstPoint, nextCurves, lastCurve } =
    { firstPoint = Point2d.at conversionFactor firstPoint
    , nextCurves = List.map (nextCurveAt conversionFactor) nextCurves
    , lastCurve = lastCurveAt conversionFactor lastCurve
    }


nextCurveAt :
    Quantity Float (Rate units2 units1)
    -> NextCurve2d units1 coordinates
    -> NextCurve2d units2 coordinates
nextCurveAt conversionFactor nextCurve2d =
    case nextCurve2d of
        NextLineSegment2d { endPoint } ->
            NextLineSegment2d
                { endPoint = Point2d.at conversionFactor endPoint }

        NextQuadraticSpline2d { secondControlPoint, thirdControlPoint } ->
            NextQuadraticSpline2d
                { secondControlPoint = Point2d.at conversionFactor secondControlPoint
                , thirdControlPoint = Point2d.at conversionFactor thirdControlPoint
                }

        NextCubicSpline2d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
            NextCubicSpline2d
                { secondControlPoint = Point2d.at conversionFactor secondControlPoint
                , thirdControlPoint = Point2d.at conversionFactor thirdControlPoint
                , fourthControlPoint = Point2d.at conversionFactor fourthControlPoint
                }


lastCurveAt :
    Quantity Float (Rate units2 units1)
    -> LastCurve2d units1 coordinates
    -> LastCurve2d units2 coordinates
lastCurveAt conversionFactor lastCurve2d =
    case lastCurve2d of
        LastLineSegment2d ->
            LastLineSegment2d

        LastQuadraticSpline2d { secondControlPoint } ->
            LastQuadraticSpline2d
                { secondControlPoint = Point2d.at conversionFactor secondControlPoint }

        LastCubicSpline2d { secondControlPoint, thirdControlPoint } ->
            LastCubicSpline2d
                { secondControlPoint = Point2d.at conversionFactor secondControlPoint
                , thirdControlPoint = Point2d.at conversionFactor thirdControlPoint
                }


boundingBox : Detail2d units coordinates -> Maybe (BoundingBox2d units coordinates)
boundingBox { firstPoint, nextCurves, lastCurve } =
    let
        ( xCoordinates, yCoordinates ) =
            List.foldl
                (\point2d ( xCollected, yCollected ) ->
                    ( Point2d.xCoordinate point2d :: xCollected
                    , Point2d.yCoordinate point2d :: yCollected
                    )
                )
                ( [], [] )
                allPoint2ds

        allPoint2ds =
            List.concat
                [ [ firstPoint ]
                , List.concatMap nextCurvePoint2ds nextCurves
                , lastCurvePoint2ds
                ]

        nextCurvePoint2ds nextCurve =
            case nextCurve of
                NextLineSegment2d { endPoint } ->
                    [ endPoint ]

                NextQuadraticSpline2d { secondControlPoint, thirdControlPoint } ->
                    [ secondControlPoint
                    , thirdControlPoint
                    ]

                NextCubicSpline2d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
                    [ secondControlPoint
                    , thirdControlPoint
                    , fourthControlPoint
                    ]

        lastCurvePoint2ds =
            case lastCurve of
                LastLineSegment2d ->
                    []

                LastQuadraticSpline2d { secondControlPoint } ->
                    [ secondControlPoint ]

                LastCubicSpline2d { secondControlPoint, thirdControlPoint } ->
                    [ secondControlPoint
                    , thirdControlPoint
                    ]
    in
    Maybe.map4
        (\minX maxX minY maxY ->
            BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
        )
        (List.minimumWith Quantity.compare xCoordinates)
        (List.maximumWith Quantity.compare xCoordinates)
        (List.minimumWith Quantity.compare yCoordinates)
        (List.maximumWith Quantity.compare yCoordinates)
