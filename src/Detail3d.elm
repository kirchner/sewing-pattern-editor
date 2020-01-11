module Detail3d exposing
    ( Detail3d, NextCurve3d(..), LastCurve3d(..)
    , on
    , rotateAround, translateBy, toScreenSpace
    )

{-|

@docs Detail3d, NextCurve3d, LastCurve3d
@docs on
@docs rotateAround, translateBy, toScreenSpace

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Detail2d exposing (Detail2d, LastCurve2d(..), NextCurve2d(..))
import Maybe.Extra as Maybe
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Detail3d u c =
    { firstPoint : Point3d u c
    , nextCurves : List (NextCurve3d u c)
    , lastCurve : LastCurve3d u c
    }


{-| -}
type NextCurve3d u c
    = NextLineSegment3d
        { endPoint : Point3d u c
        }
    | NextQuadraticSpline3d
        { secondControlPoint : Point3d u c
        , thirdControlPoint : Point3d u c
        }
    | NextCubicSpline3d
        { secondControlPoint : Point3d u c
        , thirdControlPoint : Point3d u c
        , fourthControlPoint : Point3d u c
        }


{-| -}
type LastCurve3d u c
    = LastLineSegment3d
    | LastQuadraticSpline3d
        { secondControlPoint : Point3d u c
        }
    | LastCubicSpline3d
        { secondControlPoint : Point3d u c
        , thirdControlPoint : Point3d u c
        }


on :
    SketchPlane3d units coordinates3d { defines : coordinates2d }
    -> Detail2d units coordinates2d
    -> Detail3d units coordinates3d
on sketchPlane3d detail2d =
    { firstPoint = Point3d.on sketchPlane3d detail2d.firstPoint
    , nextCurves = List.map (nextCurveOn sketchPlane3d) detail2d.nextCurves
    , lastCurve = lastCurveOn sketchPlane3d detail2d.lastCurve
    }


nextCurveOn :
    SketchPlane3d units coordinates3d { defines : coordinates2d }
    -> NextCurve2d units coordinates2d
    -> NextCurve3d units coordinates3d
nextCurveOn sketchPlane3d nextCurve2d =
    case nextCurve2d of
        NextLineSegment2d { endPoint } ->
            NextLineSegment3d
                { endPoint = Point3d.on sketchPlane3d endPoint }

        NextQuadraticSpline2d { secondControlPoint, thirdControlPoint } ->
            NextQuadraticSpline3d
                { secondControlPoint = Point3d.on sketchPlane3d secondControlPoint
                , thirdControlPoint = Point3d.on sketchPlane3d thirdControlPoint
                }

        NextCubicSpline2d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
            NextCubicSpline3d
                { secondControlPoint = Point3d.on sketchPlane3d secondControlPoint
                , thirdControlPoint = Point3d.on sketchPlane3d thirdControlPoint
                , fourthControlPoint = Point3d.on sketchPlane3d fourthControlPoint
                }


lastCurveOn :
    SketchPlane3d units coordinates3d { defines : coordinates2d }
    -> LastCurve2d units coordinates2d
    -> LastCurve3d units coordinates3d
lastCurveOn sketchPlane3d lastCurve2d =
    case lastCurve2d of
        LastLineSegment2d ->
            LastLineSegment3d

        LastQuadraticSpline2d { secondControlPoint } ->
            LastQuadraticSpline3d
                { secondControlPoint = Point3d.on sketchPlane3d secondControlPoint }

        LastCubicSpline2d { secondControlPoint, thirdControlPoint } ->
            LastCubicSpline3d
                { secondControlPoint = Point3d.on sketchPlane3d secondControlPoint
                , thirdControlPoint = Point3d.on sketchPlane3d thirdControlPoint
                }


{-| -}
rotateAround :
    Axis3d units coordinates
    -> Angle
    -> Detail3d units coordinates
    -> Detail3d units coordinates
rotateAround axis2d angle detail3d =
    { firstPoint = Point3d.rotateAround axis2d angle detail3d.firstPoint
    , nextCurves = List.map (nextCurveRotateAround axis2d angle) detail3d.nextCurves
    , lastCurve = lastCurveRotateAround axis2d angle detail3d.lastCurve
    }


nextCurveRotateAround :
    Axis3d units coordinates
    -> Angle
    -> NextCurve3d units coordinates
    -> NextCurve3d units coordinates
nextCurveRotateAround axis2d angle nextCurve3d =
    case nextCurve3d of
        NextLineSegment3d { endPoint } ->
            NextLineSegment3d
                { endPoint = Point3d.rotateAround axis2d angle endPoint }

        NextQuadraticSpline3d { secondControlPoint, thirdControlPoint } ->
            NextQuadraticSpline3d
                { secondControlPoint = Point3d.rotateAround axis2d angle secondControlPoint
                , thirdControlPoint = Point3d.rotateAround axis2d angle thirdControlPoint
                }

        NextCubicSpline3d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
            NextCubicSpline3d
                { secondControlPoint = Point3d.rotateAround axis2d angle secondControlPoint
                , thirdControlPoint = Point3d.rotateAround axis2d angle thirdControlPoint
                , fourthControlPoint = Point3d.rotateAround axis2d angle fourthControlPoint
                }


lastCurveRotateAround :
    Axis3d units coordinates
    -> Angle
    -> LastCurve3d units coordinates
    -> LastCurve3d units coordinates
lastCurveRotateAround axis2d angle lastCurve3d =
    case lastCurve3d of
        LastLineSegment3d ->
            LastLineSegment3d

        LastQuadraticSpline3d { secondControlPoint } ->
            LastQuadraticSpline3d
                { secondControlPoint = Point3d.rotateAround axis2d angle secondControlPoint }

        LastCubicSpline3d { secondControlPoint, thirdControlPoint } ->
            LastCubicSpline3d
                { secondControlPoint = Point3d.rotateAround axis2d angle secondControlPoint
                , thirdControlPoint = Point3d.rotateAround axis2d angle thirdControlPoint
                }


{-| -}
translateBy :
    Vector3d units coordinates
    -> Detail3d units coordinates
    -> Detail3d units coordinates
translateBy vector3d detail3d =
    { firstPoint = Point3d.translateBy vector3d detail3d.firstPoint
    , nextCurves = List.map (nextCurveTranslateBy vector3d) detail3d.nextCurves
    , lastCurve = lastCurveTranslateBy vector3d detail3d.lastCurve
    }


nextCurveTranslateBy :
    Vector3d units coordinates
    -> NextCurve3d units coordinates
    -> NextCurve3d units coordinates
nextCurveTranslateBy vector3d nextCurve3d =
    case nextCurve3d of
        NextLineSegment3d { endPoint } ->
            NextLineSegment3d
                { endPoint = Point3d.translateBy vector3d endPoint }

        NextQuadraticSpline3d { secondControlPoint, thirdControlPoint } ->
            NextQuadraticSpline3d
                { secondControlPoint = Point3d.translateBy vector3d secondControlPoint
                , thirdControlPoint = Point3d.translateBy vector3d thirdControlPoint
                }

        NextCubicSpline3d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
            NextCubicSpline3d
                { secondControlPoint = Point3d.translateBy vector3d secondControlPoint
                , thirdControlPoint = Point3d.translateBy vector3d thirdControlPoint
                , fourthControlPoint = Point3d.translateBy vector3d fourthControlPoint
                }


lastCurveTranslateBy :
    Vector3d units coordinates
    -> LastCurve3d units coordinates
    -> LastCurve3d units coordinates
lastCurveTranslateBy vector3d lastCurve3d =
    case lastCurve3d of
        LastLineSegment3d ->
            LastLineSegment3d

        LastQuadraticSpline3d { secondControlPoint } ->
            LastQuadraticSpline3d
                { secondControlPoint = Point3d.translateBy vector3d secondControlPoint }

        LastCubicSpline3d { secondControlPoint, thirdControlPoint } ->
            LastCubicSpline3d
                { secondControlPoint = Point3d.translateBy vector3d secondControlPoint
                , thirdControlPoint = Point3d.translateBy vector3d thirdControlPoint
                }


{-| -}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Detail3d worldUnits worldCoordinates
    -> Maybe (Detail2d screenUnits screenCoordinates)
toScreenSpace camera window detail2d =
    Maybe.map3 Detail2d
        (Point3d.Projection.toScreenSpace camera window detail2d.firstPoint)
        (Maybe.combine (List.map (nextCurveToScreenSpace camera window) detail2d.nextCurves))
        (lastCurveToScreenSpace camera window detail2d.lastCurve)


nextCurveToScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> NextCurve3d worldUnits worldCoordinates
    -> Maybe (NextCurve2d screenUnits screenCoordinates)
nextCurveToScreenSpace camera window nextCurve2d =
    case nextCurve2d of
        NextLineSegment3d { endPoint } ->
            Maybe.map
                (\projectedEndPoint ->
                    NextLineSegment2d
                        { endPoint = projectedEndPoint }
                )
                (Point3d.Projection.toScreenSpace camera window endPoint)

        NextQuadraticSpline3d { secondControlPoint, thirdControlPoint } ->
            Maybe.map2
                (\projectedSecondControlPoint projectedThirdControlPoint ->
                    NextQuadraticSpline2d
                        { secondControlPoint = projectedSecondControlPoint
                        , thirdControlPoint = projectedThirdControlPoint
                        }
                )
                (Point3d.Projection.toScreenSpace camera window secondControlPoint)
                (Point3d.Projection.toScreenSpace camera window thirdControlPoint)

        NextCubicSpline3d { secondControlPoint, thirdControlPoint, fourthControlPoint } ->
            Maybe.map3
                (\projectedSecondControlPoint projectedThirdControlPoint projectedFourthControlPoint ->
                    NextCubicSpline2d
                        { secondControlPoint = projectedSecondControlPoint
                        , thirdControlPoint = projectedThirdControlPoint
                        , fourthControlPoint = projectedFourthControlPoint
                        }
                )
                (Point3d.Projection.toScreenSpace camera window secondControlPoint)
                (Point3d.Projection.toScreenSpace camera window thirdControlPoint)
                (Point3d.Projection.toScreenSpace camera window fourthControlPoint)


lastCurveToScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> LastCurve3d worldUnits worldCoordinates
    -> Maybe (LastCurve2d screenUnits screenCoordinates)
lastCurveToScreenSpace camera window lastCurve2d =
    case lastCurve2d of
        LastLineSegment3d ->
            Just LastLineSegment2d

        LastQuadraticSpline3d { secondControlPoint } ->
            Maybe.map
                (\projectedSecondControlPoint ->
                    LastQuadraticSpline2d
                        { secondControlPoint = projectedSecondControlPoint }
                )
                (Point3d.Projection.toScreenSpace camera window secondControlPoint)

        LastCubicSpline3d { secondControlPoint, thirdControlPoint } ->
            Maybe.map2
                (\projectedSecondControlPoint projectedThirdControlPoint ->
                    LastCubicSpline2d
                        { secondControlPoint = projectedSecondControlPoint
                        , thirdControlPoint = projectedThirdControlPoint
                        }
                )
                (Point3d.Projection.toScreenSpace camera window secondControlPoint)
                (Point3d.Projection.toScreenSpace camera window thirdControlPoint)
