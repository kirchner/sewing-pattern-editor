module Pattern exposing
    ( Pattern
    , empty
    , Geometry, Segment(..), computeGeometry, geometry, Problems
    , getPointGeometries, getPointGeometry
    , point2d
    , variables, insertVariable, removeVariable
    , Point(..), points, insertPoint, getPoint, updatePoint
    , origin
    , leftOf, rightOf, above, below
    , atAngle
    , betweenRatio, betweenLength
    , firstCircleCircle, secondCircleCircle, lineLine, firstCircleLine, secondCircleLine
    , Circle(..), circles, insertCircle, getCircle
    , centeredAt
    , Line(..), lines, insertLine, getLine
    , throughTwoPoints, throughOnePoint
    , LineSegment(..), lineSegments, insertLineSegment, getLineSegment
    , Detail(..), Connection(..), details, insertDetail, getDetail
    , Transformation(..), insertTransformation
    , decoder, encode
    )

{-|

@docs Pattern

@docs empty


# Geometry

@docs Geometry, Segment, computeGeometry, geometry, Problems

@docs getPointGeometries, getPointGeometry

@docs point2d


# Variables

@docs Variable, variables, insertVariable, removeVariable, renameVariable


# Objects


## Points

@docs Point, points, insertPoint, getPoint, updatePoint, replacePoint, removePoint

@docs origin

@docs leftOf, rightOf, above, below

@docs atAngle

@docs betweenRatio, betweenLength

@docs firstCircleCircle, secondCircleCircle, lineLine, firstCircleLine, secondCircleLine


## Circles

@docs Circle, circles, insertCircle, getCircle, replaceCircle, removeCircle

@docs centeredAt


## Lines

@docs Line, lines, insertLine, getLine

@docs throughTwoPoints, throughOnePoint


## Line Segments

@docs LineSegment, lineSegments, insertLineSegment, getLineSegment


## Details

@docs Detail, Connection, details, insertDetail, getDetail


# Transformations

@docs Transformation, transformations, insertTransformation


# JSON

@docs decoder, encode

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

import Axis2d exposing (Axis2d)
import Axis2d.Extra as Axis2d
import Circle2d exposing (Circle2d)
import Circle2d.Extra as Circle2d exposing (Intersection(..))
import Dict exposing (Dict)
import Direction2d
import Expr exposing (Expr(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Parser
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Set exposing (Set)
import Store exposing (Entry, Store)
import That exposing (That, that)
import Vector2d


type Pattern
    = Pattern PatternData


type alias PatternData =
    { points : Store Point
    , circles : Store Circle
    , lines : Store Line
    , lineSegments : Store LineSegment
    , details : Store Detail
    , transformations : Store Transformation
    , variables : Dict String String

    -- CACHE
    , cache :
        Maybe
            { geometry : Geometry
            , problems : Problems
            }
    }


empty : Pattern
empty =
    Pattern
        { points = Store.empty
        , circles = Store.empty
        , lines = Store.empty
        , lineSegments = Store.empty
        , details = Store.empty
        , variables = Dict.empty
        , transformations = Store.empty
        , cache = Nothing
        }



---- OBJECTS


type Point
    = Origin Float Float
    | LeftOf (That Point) String
    | RightOf (That Point) String
    | Above (That Point) String
    | Below (That Point) String
    | AtAngle (That Point) String String
      -- BETWEEN
    | BetweenRatio (That Point) (That Point) String
    | BetweenLength (That Point) (That Point) String
      -- ON OBJECT
    | OnCurveRatio (That Curve) String
    | OnCurveLength (That Curve) String
    | OnCircleFirstTangent (That Circle) (That Point)
    | OnCircleSecondTangent (That Circle) (That Point)
    | OnCircleFirstChord (That Circle) String
    | OnCircleSecondChord (That Circle) String
      -- BY INTERSECTION
    | FirstCircleCircle (That Circle) (That Circle)
    | SecondCircleCircle (That Circle) (That Circle)
    | LineLine (That Line) (That Line)
    | FirstCircleLine (That Circle) (That Line)
    | SecondCircleLine (That Circle) (That Line)
      -- BY TRANSFORMATION
    | TransformBy (That Transformation) (That Point)


type Line
    = ThroughOnePoint (That Point) String
    | ThroughTwoPoints (That Point) (That Point)


type LineSegment
    = FromTo (That Point) (That Point)


type Circle
    = CenteredAt (That Point) String


type Curve
    = TODOCurve


type Ellipsis
    = TODOEllipsis


type Detail
    = FromPoints
        { firstPoint : That Point
        , segments : List ( That Point, Connection )
        , lastToFirst : Connection
        }


type Connection
    = Straight
    | Cubic (That Point)



---- TRANSFORMATIONS


type Transformation
    = MirrorAt (That Line)
    | RotateAround (That Point) String



---- EXPRESSIONS


reservedWords : List String
reservedWords =
    [ "distance" ]


compute : Pattern -> Expr -> Result DoesNotCompute Float
compute ((Pattern data) as pattern) expr =
    let
        functions name args =
            case name of
                "distance" ->
                    case args of
                        namePointA :: namePointB :: [] ->
                            Maybe.map2 Point2d.distanceFrom
                                (namePointA
                                    |> thatPointByName pattern
                                    |> Maybe.andThen (point2d pattern)
                                )
                                (namePointB
                                    |> thatPointByName pattern
                                    |> Maybe.andThen (point2d pattern)
                                )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        parsedVars =
            data.variables
                |> Dict.toList
                |> List.filterMap
                    (\( name, string ) ->
                        case Expr.parse reservedWords string of
                            Err _ ->
                                Nothing

                            Ok varExpr ->
                                Just ( name, varExpr )
                    )
                |> Dict.fromList
    in
    case Expr.evaluate functions parsedVars expr of
        Nothing ->
            Err TODO

        Just float ->
            Ok float


type DoesNotCompute
    = TODO
    | MissingVariable String
    | RecursiveExpression (List String)



---- GEOMETRY


type alias Geometry =
    { points : List ( That Point, Maybe String, Point2d )
    , circles : List ( That Circle, Maybe String, Circle2d )
    , lines : List ( That Line, Maybe String, Axis2d )
    , lineSegments : List ( That LineSegment, Maybe String, LineSegment2d )
    , details : List ( That Detail, Maybe String, List Segment )
    }


type Segment
    = LineSegment LineSegment2d


type alias Problems =
    { doNotCompute : List ( String, DoesNotCompute )
    , missingPoints : List (That Point)
    , missingCircles : List (That Circle)
    , missingLines : List (That Line)
    }


computeGeometry : Pattern -> ( Pattern, ( Geometry, Problems ) )
computeGeometry ((Pattern data) as pattern) =
    case data.cache of
        Nothing ->
            let
                ( newGeometry, newProblems ) =
                    geometry pattern
            in
            ( Pattern
                { data
                    | cache =
                        Just
                            { geometry = newGeometry
                            , problems = newProblems
                            }
                }
            , ( newGeometry
              , newProblems
              )
            )

        Just cache ->
            ( pattern
            , ( cache.geometry
              , cache.problems
              )
            )


geometry : Pattern -> ( Geometry, Problems )
geometry ((Pattern data) as pattern) =
    case data.cache of
        Nothing ->
            let
                geometryPoint ( thatPoint, { name } ) =
                    point2d pattern thatPoint
                        |> Maybe.map (\p2d -> ( thatPoint, name, p2d ))

                geometryCircle ( thatCircle, { name } ) =
                    circle2d pattern thatCircle
                        |> Maybe.map (\c2d -> ( thatCircle, name, c2d ))

                geometryLine ( thatLine, { name } ) =
                    axis2d pattern thatLine
                        |> Maybe.map (\a2d -> ( thatLine, name, a2d ))

                geometryLineSegment ( thatLineSegment, { name } ) =
                    lineSegment2d pattern thatLineSegment
                        |> Maybe.map (\ls2d -> ( thatLineSegment, name, ls2d ))

                geometryDetail ( thatDetail, { name } ) =
                    polygon2d pattern thatDetail
                        |> Maybe.map (\p2d -> ( thatDetail, name, p2d ))
            in
            ( { points =
                    pattern
                        |> points
                        |> List.filterMap geometryPoint
              , circles =
                    pattern
                        |> circles
                        |> List.filterMap geometryCircle
              , lines =
                    pattern
                        |> lines
                        |> List.filterMap geometryLine
              , lineSegments =
                    pattern
                        |> lineSegments
                        |> List.filterMap geometryLineSegment
              , details =
                    pattern
                        |> details
                        |> List.filterMap geometryDetail
              }
            , { doNotCompute = []
              , missingPoints = []
              , missingCircles = []
              , missingLines = []
              }
            )

        Just cache ->
            ( cache.geometry, cache.problems )


point2d : Pattern -> That Point -> Maybe Point2d
point2d ((Pattern { cache }) as pattern) thatPoint =
    case cache of
        Nothing ->
            computePoint2d pattern thatPoint

        Just actualCache ->
            actualCache.geometry.points
                |> List.filterMap
                    (\( that, _, p2d ) ->
                        if that == thatPoint then
                            Just p2d

                        else
                            Nothing
                    )
                |> List.head


computePoint2d : Pattern -> That Point -> Maybe Point2d
computePoint2d pattern thatPoint =
    let
        simpleDistance anchor rawLength toDirection =
            case Expr.parse reservedWords rawLength of
                Err _ ->
                    Nothing

                Ok exprLength ->
                    case compute pattern exprLength of
                        Err _ ->
                            Nothing

                        Ok distance ->
                            let
                                v =
                                    toDirection distance
                            in
                            anchor
                                |> point2d pattern
                                |> Maybe.map (Point2d.translateBy v)

        applyTransformations point =
            point

        applyTransformation point transformation =
            case transformation of
                MirrorAt thatLine ->
                    case axis2d pattern thatLine of
                        Just axis ->
                            point
                                |> Point2d.mirrorAcross axis

                        Nothing ->
                            point

                _ ->
                    point
    in
    Maybe.map applyTransformations <|
        case Maybe.map .value (getPoint pattern thatPoint) of
            Just (Origin x y) ->
                Just (Point2d.fromCoordinates ( x, y ))

            Just (LeftOf anchor rawLength) ->
                simpleDistance anchor rawLength <|
                    \distance -> Vector2d.fromComponents ( -1 * distance, 0 )

            Just (RightOf anchor rawLength) ->
                simpleDistance anchor rawLength <|
                    \distance -> Vector2d.fromComponents ( distance, 0 )

            Just (Above anchor rawLength) ->
                simpleDistance anchor rawLength <|
                    \distance -> Vector2d.fromComponents ( 0, -1 * distance )

            Just (Below anchor rawLength) ->
                simpleDistance anchor rawLength <|
                    \distance -> Vector2d.fromComponents ( 0, distance )

            Just (AtAngle anchor rawAngle rawDistance) ->
                Maybe.map2 Tuple.pair
                    (Result.toMaybe (Expr.parse reservedWords rawAngle))
                    (Result.toMaybe (Expr.parse reservedWords rawDistance))
                    |> Maybe.andThen
                        (\( exprAngle, exprDistance ) ->
                            Maybe.map2 Tuple.pair
                                (Result.toMaybe (compute pattern exprAngle))
                                (Result.toMaybe (compute pattern exprDistance))
                                |> Maybe.andThen
                                    (\( angle, distance ) ->
                                        anchor
                                            |> point2d pattern
                                            |> Maybe.map
                                                (Point2d.translateBy <|
                                                    Vector2d.fromPolarComponents
                                                        ( distance, degrees angle )
                                                )
                                    )
                        )

            Just (BetweenRatio thatAnchorA thatAnchorB rawRatio) ->
                Maybe.map3
                    (\anchorA anchorB ratio ->
                        anchorA
                            |> Point2d.translateBy
                                (Vector2d.from anchorA anchorB
                                    |> Vector2d.scaleBy ratio
                                )
                    )
                    (point2d pattern thatAnchorA)
                    (point2d pattern thatAnchorB)
                    (rawRatio
                        |> Expr.parse reservedWords
                        |> Result.toMaybe
                        |> Maybe.andThen (compute pattern >> Result.toMaybe)
                    )

            Just (BetweenLength thatAnchorA thatAnchorB rawLength) ->
                case
                    ( point2d pattern thatAnchorA
                    , point2d pattern thatAnchorB
                    )
                of
                    ( Just anchorA, Just anchorB ) ->
                        case Direction2d.from anchorA anchorB of
                            Just direction ->
                                (rawLength
                                    |> Expr.parse reservedWords
                                    |> Result.toMaybe
                                    |> Maybe.andThen (compute pattern >> Result.toMaybe)
                                )
                                    |> Maybe.map
                                        (\length ->
                                            Point2d.along
                                                (Axis2d.through anchorA direction)
                                                length
                                        )

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing

            Just (FirstCircleCircle thatCircleA thatCircleB) ->
                Maybe.map2 Tuple.pair
                    (circle2d pattern thatCircleA)
                    (circle2d pattern thatCircleB)
                    |> Maybe.andThen
                        (\( circleA, circleB ) ->
                            case Circle2d.intersectionCircle circleA circleB of
                                NoIntersection ->
                                    Nothing

                                OnePoint point ->
                                    Just point

                                TwoPoints point _ ->
                                    Just point
                        )

            Just (SecondCircleCircle thatCircleA thatCircleB) ->
                Maybe.map2 Tuple.pair
                    (circle2d pattern thatCircleA)
                    (circle2d pattern thatCircleB)
                    |> Maybe.andThen
                        (\( circleA, circleB ) ->
                            case Circle2d.intersectionCircle circleA circleB of
                                NoIntersection ->
                                    Nothing

                                OnePoint point ->
                                    Just point

                                TwoPoints _ point ->
                                    Just point
                        )

            Just (LineLine thatLineA thatLineB) ->
                Maybe.map2 Tuple.pair
                    (axis2d pattern thatLineA)
                    (axis2d pattern thatLineB)
                    |> Maybe.andThen
                        (\( axisA, axisB ) ->
                            Axis2d.intersectionWithAxis axisA axisB
                        )

            Just (FirstCircleLine thatCircle thatLine) ->
                Maybe.map2 Tuple.pair
                    (circle2d pattern thatCircle)
                    (axis2d pattern thatLine)
                    |> Maybe.andThen
                        (\( circle, axis ) ->
                            case Circle2d.intersectionAxis circle axis of
                                NoIntersection ->
                                    Nothing

                                OnePoint point ->
                                    Just point

                                TwoPoints point _ ->
                                    Just point
                        )

            Just (SecondCircleLine thatCircle thatLine) ->
                Maybe.map2 Tuple.pair
                    (circle2d pattern thatCircle)
                    (axis2d pattern thatLine)
                    |> Maybe.andThen
                        (\( circle, axis ) ->
                            case Circle2d.intersectionAxis circle axis of
                                NoIntersection ->
                                    Nothing

                                OnePoint point ->
                                    Just point

                                TwoPoints _ point ->
                                    Just point
                        )

            _ ->
                Nothing


circle2d : Pattern -> That Circle -> Maybe Circle2d
circle2d pattern thatCircle =
    case Maybe.map .value (getCircle pattern thatCircle) of
        Just (CenteredAt thatCenter rawRadius) ->
            Maybe.map2 Circle2d.withRadius
                (rawRadius
                    |> Expr.parse reservedWords
                    |> Result.toMaybe
                    |> Maybe.andThen (compute pattern >> Result.toMaybe)
                )
                (point2d pattern thatCenter)

        Nothing ->
            Nothing


axis2d : Pattern -> That Line -> Maybe Axis2d
axis2d ((Pattern pattern) as p) thatLine =
    case Maybe.map .value (getLine p thatLine) of
        Just (ThroughTwoPoints thatPointA thatPointB) ->
            case ( point2d p thatPointA, point2d p thatPointB ) of
                ( Just point2dA, Just point2dB ) ->
                    Direction2d.from point2dA point2dB
                        |> Maybe.map (Axis2d.through point2dA)

                _ ->
                    Nothing

        Just (ThroughOnePoint thatPoint rawAngle) ->
            Maybe.map2 Axis2d.through
                (point2d p thatPoint)
                (rawAngle
                    |> Expr.parse reservedWords
                    |> Result.toMaybe
                    |> Maybe.andThen (compute p >> Result.toMaybe)
                    |> Maybe.map (degrees >> Direction2d.fromAngle)
                )

        Nothing ->
            Nothing


lineSegment2d : Pattern -> That LineSegment -> Maybe LineSegment2d
lineSegment2d ((Pattern pattern) as p) thatLineSegment =
    case Maybe.map .value (getLineSegment p thatLineSegment) of
        Just (FromTo thatPointA thatPointB) ->
            case ( point2d p thatPointA, point2d p thatPointB ) of
                ( Just point2dA, Just point2dB ) ->
                    Just (LineSegment2d.from point2dA point2dB)

                _ ->
                    Nothing

        _ ->
            Nothing


polygon2d : Pattern -> That Detail -> Maybe (List Segment)
polygon2d ((Pattern pattern) as p) thatDetail =
    case Maybe.map .value (getDetail p thatDetail) of
        Nothing ->
            Nothing

        Just (FromPoints { firstPoint, segments, lastToFirst }) ->
            let
                computedSegments =
                    List.foldr
                        (\( thatPoint, connection ) result ->
                            case result of
                                Nothing ->
                                    Nothing

                                Just sum ->
                                    case point2d p thatPoint of
                                        Nothing ->
                                            Nothing

                                        Just p2d ->
                                            Just (( p2d, connection ) :: sum)
                        )
                        (Just [])
                        segments
            in
            Maybe.map2
                (\firstPoint2d otherPoints ->
                    let
                        lineSegmentHelp connection p2dA p2dB =
                            case connection of
                                Straight ->
                                    LineSegment (LineSegment2d.from p2dA p2dB)

                                Cubic _ ->
                                    Debug.todo "implement"
                    in
                    (case List.head (List.reverse otherPoints) of
                        Nothing ->
                            Debug.todo "handle error"

                        Just ( lastPoint2d, _ ) ->
                            lineSegmentHelp lastToFirst lastPoint2d firstPoint2d
                    )
                        :: (Tuple.second <|
                                List.foldl
                                    (\( nextPoint2d, connection ) ( previousPoint2d, result ) ->
                                        ( nextPoint2d
                                        , lineSegmentHelp connection previousPoint2d nextPoint2d
                                            :: result
                                        )
                                    )
                                    ( firstPoint2d, [] )
                                    otherPoints
                           )
                )
                (point2d p firstPoint)
                computedSegments



---- VARIABLES


variables : Pattern -> List { name : String, value : String, computed : Float }
variables ((Pattern data) as pattern) =
    data.variables
        |> Dict.toList
        |> List.filterMap
            (\( name, value ) ->
                value
                    |> Expr.parse reservedWords
                    |> Result.toMaybe
                    |> Maybe.andThen (compute pattern >> Result.toMaybe)
                    |> Maybe.map
                        (\computed ->
                            { name = name
                            , value = value
                            , computed = computed
                            }
                        )
            )


insertVariable : String -> String -> Pattern -> Pattern
insertVariable name value ((Pattern data) as pattern) =
    case Expr.parse reservedWords value of
        Err _ ->
            pattern

        Ok _ ->
            Pattern
                { data
                    | variables = Dict.insert name value data.variables
                    , cache = Nothing
                }


removeVariable : String -> Pattern -> Pattern
removeVariable name (Pattern data) =
    Pattern
        { data | variables = Dict.remove name data.variables }



---- POINTS


origin : { x : Float, y : Float } -> Point
origin { x, y } =
    Origin x y


leftOf : Pattern -> That Point -> String -> Maybe Point
leftOf (Pattern pattern) thatPoint rawLength =
    case Expr.parse reservedWords rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (LeftOf thatPoint rawLength)

            else
                Nothing


rightOf : Pattern -> That Point -> String -> Maybe Point
rightOf (Pattern pattern) thatPoint rawLength =
    case Expr.parse reservedWords rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (RightOf thatPoint rawLength)

            else
                Nothing


above : Pattern -> That Point -> String -> Maybe Point
above (Pattern pattern) thatPoint rawLength =
    case Expr.parse reservedWords rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (Above thatPoint rawLength)

            else
                Nothing


below : Pattern -> That Point -> String -> Maybe Point
below (Pattern pattern) thatPoint rawLength =
    case Expr.parse reservedWords rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (Below thatPoint rawLength)

            else
                Nothing


atAngle : Pattern -> That Point -> String -> String -> Maybe Point
atAngle (Pattern pattern) thatPoint rawAngle rawDistance =
    case ( Expr.parse reservedWords rawAngle, Expr.parse reservedWords rawDistance ) of
        ( Ok _, Ok _ ) ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (AtAngle thatPoint rawAngle rawDistance)

            else
                Nothing

        _ ->
            Nothing


betweenRatio : Pattern -> That Point -> That Point -> String -> Maybe Point
betweenRatio (Pattern pattern) thatAnchorA thatAnchorB rawRatio =
    case Expr.parse reservedWords rawRatio of
        Ok _ ->
            if
                Store.member pattern.points (That.objectId thatAnchorA)
                    && Store.member pattern.points (That.objectId thatAnchorB)
            then
                Just (BetweenRatio thatAnchorA thatAnchorB rawRatio)

            else
                Nothing

        Err _ ->
            Nothing


betweenLength : Pattern -> That Point -> That Point -> String -> Maybe Point
betweenLength (Pattern pattern) thatAnchorA thatAnchorB rawLength =
    case Expr.parse reservedWords rawLength of
        Ok _ ->
            if
                Store.member pattern.points (That.objectId thatAnchorA)
                    && Store.member pattern.points (That.objectId thatAnchorB)
            then
                Just (BetweenLength thatAnchorA thatAnchorB rawLength)

            else
                Nothing

        Err _ ->
            Nothing



-- INTERSECTIONS


firstCircleCircle : Pattern -> That Circle -> That Circle -> Maybe Point
firstCircleCircle (Pattern pattern) thatCircleA thatCircleB =
    if
        Store.member pattern.circles (That.objectId thatCircleA)
            && Store.member pattern.circles (That.objectId thatCircleB)
    then
        Just (FirstCircleCircle thatCircleA thatCircleB)

    else
        Nothing


secondCircleCircle : Pattern -> That Circle -> That Circle -> Maybe Point
secondCircleCircle (Pattern pattern) thatCircleA thatCircleB =
    if
        Store.member pattern.circles (That.objectId thatCircleA)
            && Store.member pattern.circles (That.objectId thatCircleB)
    then
        Just (SecondCircleCircle thatCircleA thatCircleB)

    else
        Nothing


lineLine : Pattern -> That Line -> That Line -> Maybe Point
lineLine (Pattern pattern) thatLineA thatLineB =
    if
        Store.member pattern.lines (That.objectId thatLineA)
            && Store.member pattern.lines (That.objectId thatLineB)
    then
        Just (LineLine thatLineA thatLineB)

    else
        Nothing


firstCircleLine : Pattern -> That Circle -> That Line -> Maybe Point
firstCircleLine (Pattern pattern) thatCircle thatLine =
    if
        Store.member pattern.circles (That.objectId thatCircle)
            && Store.member pattern.lines (That.objectId thatLine)
    then
        Just (FirstCircleLine thatCircle thatLine)

    else
        Nothing


secondCircleLine : Pattern -> That Circle -> That Line -> Maybe Point
secondCircleLine (Pattern pattern) thatCircle thatLine =
    if
        Store.member pattern.circles (That.objectId thatCircle)
            && Store.member pattern.lines (That.objectId thatLine)
    then
        Just (SecondCircleLine thatCircle thatLine)

    else
        Nothing



---- CIRCLES


centeredAt : Pattern -> That Point -> String -> Maybe Circle
centeredAt (Pattern pattern) thatCenter rawRadius =
    case Expr.parse reservedWords rawRadius of
        Ok _ ->
            if Store.member pattern.points (That.objectId thatCenter) then
                Just (CenteredAt thatCenter rawRadius)

            else
                Nothing

        Err _ ->
            Nothing


throughTwoPoints : Pattern -> That Point -> That Point -> Maybe Line
throughTwoPoints (Pattern pattern) thatPointA thatPointB =
    if
        Store.member pattern.points (That.objectId thatPointA)
            && Store.member pattern.points (That.objectId thatPointB)
    then
        Just (ThroughTwoPoints thatPointA thatPointB)

    else
        Nothing


throughOnePoint : Pattern -> That Point -> String -> Maybe Line
throughOnePoint (Pattern pattern) thatPoint rawAngle =
    case Expr.parse reservedWords rawAngle of
        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (ThroughOnePoint thatPoint rawAngle)

            else
                Nothing

        Err _ ->
            Nothing



--


points : Pattern -> List ( That Point, Entry Point )
points =
    objects .points


getPoint : Pattern -> That Point -> Maybe (Entry Point)
getPoint (Pattern pattern) =
    Store.get pattern.points << That.objectId


thatPointByName : Pattern -> String -> Maybe (That Point)
thatPointByName (Pattern pattern) name =
    Store.getByName pattern.points name
        |> Maybe.map (Tuple.first >> That.that)


updatePoint : That Point -> Point -> Pattern -> Pattern
updatePoint thatPoint point (Pattern pattern) =
    Pattern
        { pattern
            | points =
                Store.updateValue (That.objectId thatPoint) point pattern.points
            , cache = Nothing
        }


getPointGeometry : Pattern -> That Point -> Maybe Point2d
getPointGeometry pattern thatPoint =
    point2d pattern thatPoint


getPointGeometries : Pattern -> That Point -> List Point2d
getPointGeometries pattern thatPoint =
    getPointGeometriesHelp [] pattern thatPoint


getPointGeometriesHelp : List (Maybe Point2d) -> Pattern -> That Point -> List Point2d
getPointGeometriesHelp geometries pattern thatPoint =
    List.filterMap identity
        (point2d pattern thatPoint :: geometries)


insertPoint : Maybe String -> Point -> Pattern -> ( Pattern, That Point )
insertPoint name point (Pattern pattern) =
    let
        ( newPoints, id ) =
            Store.insert name point pattern.points
    in
    ( Pattern
        { pattern
            | points = newPoints
            , cache = Nothing
        }
    , that id
    )



---- CIRCLES


circles : Pattern -> List ( That Circle, Entry Circle )
circles ((Pattern pattern) as p) =
    pattern.circles
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatCircleFromId p))


thatCircleFromId : Pattern -> Int -> That Circle
thatCircleFromId (Pattern pattern) id =
    that id


getCircle : Pattern -> That Circle -> Maybe (Entry Circle)
getCircle (Pattern pattern) =
    Store.get pattern.circles << That.objectId


insertCircle : Maybe String -> Circle -> Pattern -> ( Pattern, That Circle )
insertCircle name circle (Pattern pattern) =
    let
        ( newCircles, id ) =
            Store.insert name circle pattern.circles
    in
    ( Pattern
        { pattern
            | circles = newCircles
            , cache = Nothing
        }
    , that id
    )



---- LINES


lines : Pattern -> List ( That Line, Entry Line )
lines ((Pattern pattern) as p) =
    pattern.lines
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatLineFromId p))


thatLineFromId : Pattern -> Int -> That Line
thatLineFromId (Pattern pattern) id =
    that id


getLine : Pattern -> That Line -> Maybe (Entry Line)
getLine (Pattern pattern) =
    Store.get pattern.lines << That.objectId


insertLine : Maybe String -> Line -> Pattern -> ( Pattern, That Line )
insertLine maybeName line (Pattern pattern) =
    let
        ( newLines, id ) =
            Store.insert maybeName line pattern.lines
    in
    ( Pattern
        { pattern
            | lines = newLines
            , cache = Nothing
        }
    , that id
    )



---- LINE SEGMENTS


lineSegments : Pattern -> List ( That LineSegment, Entry LineSegment )
lineSegments ((Pattern pattern) as p) =
    pattern.lineSegments
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatLineSegmentFromId p))


thatLineSegmentFromId : Pattern -> Int -> That LineSegment
thatLineSegmentFromId (Pattern pattern) id =
    that id


getLineSegment : Pattern -> That LineSegment -> Maybe (Entry LineSegment)
getLineSegment (Pattern pattern) =
    Store.get pattern.lineSegments << That.objectId


insertLineSegment : Maybe String -> LineSegment -> Pattern -> ( Pattern, That LineSegment )
insertLineSegment maybeName lineSegment (Pattern pattern) =
    let
        ( newLineSegments, id ) =
            Store.insert maybeName lineSegment pattern.lineSegments
    in
    ( Pattern
        { pattern
            | lineSegments = newLineSegments
            , cache = Nothing
        }
    , that id
    )



---- TRANSFORMATIONS


insertTransformation : Transformation -> Pattern -> ( Pattern, That Transformation )
insertTransformation transformation (Pattern pattern) =
    let
        ( newTransformations, id ) =
            Store.insert Nothing transformation pattern.transformations
    in
    ( Pattern
        { pattern
            | transformations = newTransformations
            , cache = Nothing
        }
    , that id
    )



---- DETAILS


details : Pattern -> List ( That Detail, Entry Detail )
details pattern =
    objects .details pattern


insertDetail : Maybe String -> Detail -> Pattern -> ( Pattern, That Detail )
insertDetail maybeName detail (Pattern pattern) =
    let
        ( newDetails, id ) =
            Store.insert maybeName detail pattern.details
    in
    ( Pattern
        { pattern
            | details = newDetails
            , cache = Nothing
        }
    , that id
    )


getDetail : Pattern -> That Detail -> Maybe (Entry Detail)
getDetail (Pattern pattern) =
    Store.get pattern.details << That.objectId



---- OBJECTS


objects : (PatternData -> Store a) -> Pattern -> List ( That a, Entry a )
objects objectsAccessor ((Pattern pattern) as p) =
    pattern
        |> objectsAccessor
        |> Store.toList
        |> List.map (Tuple.mapFirst that)



---- ENCODER


encode : Pattern -> Value
encode (Pattern pattern) =
    Encode.object
        [ ( "points", Store.encode encodePoint pattern.points )
        , ( "circles", Store.encode encodeCircle pattern.circles )
        , ( "lines", Store.encode encodeLine pattern.lines )
        , ( "lineSegments", Store.encode encodeLineSegment pattern.lineSegments )
        , ( "details", Store.encode encodeDetail pattern.details )
        , ( "variables", encodeVariables pattern.variables )
        , ( "transformations", Store.encode encodeTransformation pattern.transformations )
        ]


encodeTransformation : Transformation -> Value
encodeTransformation transformation =
    case transformation of
        MirrorAt line ->
            withType "mirrorAt"
                [ ( "line", That.encode line ) ]

        RotateAround point angle ->
            withType "rotateAround"
                [ ( "point", That.encode point )
                , ( "angle", Encode.string angle )
                ]


encodeVariables : Dict String String -> Value
encodeVariables =
    let
        encodePair ( name, value ) =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "value", Encode.string value )
                ]
    in
    Dict.toList
        >> Encode.list encodePair


encodePoint : Point -> Value
encodePoint point =
    case point of
        Origin x y ->
            withType "origin"
                [ ( "x", Encode.float x )
                , ( "y", Encode.float y )
                ]

        LeftOf anchor length ->
            withType "leftOf"
                [ ( "anchor", That.encode anchor )
                , ( "distance", Encode.string length )
                ]

        RightOf anchor length ->
            withType "rightOf"
                [ ( "anchor", That.encode anchor )
                , ( "distance", Encode.string length )
                ]

        Above anchor length ->
            withType "above"
                [ ( "anchor", That.encode anchor )
                , ( "distance", Encode.string length )
                ]

        Below anchor length ->
            withType "below"
                [ ( "anchor", That.encode anchor )
                , ( "distance", Encode.string length )
                ]

        AtAngle anchor angle distance ->
            withType "atAngle"
                [ ( "anchor", That.encode anchor )
                , ( "angle", Encode.string angle )
                , ( "distance", Encode.string distance )
                ]

        BetweenRatio anchorA anchorB ratio ->
            withType "betweenRatio"
                [ ( "anchorA", That.encode anchorA )
                , ( "anchorB", That.encode anchorB )
                , ( "ratio", Encode.string ratio )
                ]

        BetweenLength anchorA anchorB length ->
            withType "betweenLength"
                [ ( "anchorA", That.encode anchorA )
                , ( "anchorB", That.encode anchorB )
                , ( "length", Encode.string length )
                ]

        FirstCircleCircle circleA circleB ->
            withType "firstCircleCircle"
                [ ( "circleA", That.encode circleA )
                , ( "circleB", That.encode circleB )
                ]

        SecondCircleCircle circleA circleB ->
            withType "secondCircleCircle"
                [ ( "circleA", That.encode circleA )
                , ( "circleB", That.encode circleB )
                ]

        LineLine lineA lineB ->
            withType "lineLine"
                [ ( "lineA", That.encode lineA )
                , ( "lineB", That.encode lineB )
                ]

        FirstCircleLine circle line ->
            withType "firstCircleLine"
                [ ( "circle", That.encode circle )
                , ( "line", That.encode line )
                ]

        SecondCircleLine circle line ->
            withType "secondCircleLine"
                [ ( "circle", That.encode circle )
                , ( "line", That.encode line )
                ]

        _ ->
            Encode.null


encodeCircle : Circle -> Value
encodeCircle circle =
    case circle of
        CenteredAt center radius ->
            withType "centeredAt"
                [ ( "center", That.encode center )
                , ( "radius", Encode.string radius )
                ]


encodeLine : Line -> Value
encodeLine line =
    case line of
        ThroughTwoPoints anchorA anchorB ->
            withType "throughTwoPoints"
                [ ( "anchorA", That.encode anchorA )
                , ( "anchorB", That.encode anchorB )
                ]

        ThroughOnePoint anchor angle ->
            withType "throughOnePoint"
                [ ( "anchor", That.encode anchor )
                , ( "angle", Encode.string angle )
                ]


encodeLineSegment : LineSegment -> Value
encodeLineSegment lineSegment =
    case lineSegment of
        FromTo anchorA anchorB ->
            withType "fromTo"
                [ ( "anchorA", That.encode anchorA )
                , ( "anchorB", That.encode anchorB )
                ]


encodeDetail : Detail -> Value
encodeDetail detail =
    let
        encodeSegment ( thatPoint, connection ) =
            Encode.list identity
                [ That.encode thatPoint
                , encodeConnection connection
                ]
    in
    case detail of
        FromPoints { firstPoint, segments, lastToFirst } ->
            withType "fromPoints"
                [ ( "firstPoint", That.encode firstPoint )
                , ( "segments", Encode.list encodeSegment segments )
                , ( "lastToFirst", encodeConnection lastToFirst )
                ]


encodeConnection : Connection -> Value
encodeConnection connection =
    case connection of
        Straight ->
            withType "straight" []

        Cubic thatPoint ->
            withType "cubic"
                [ ( "knot", That.encode thatPoint ) ]


encodeExpr : Expr -> Value
encodeExpr expr =
    case expr of
        Number float ->
            withType "number"
                [ ( "value", Encode.float float ) ]

        _ ->
            Encode.null


withType : String -> List ( String, Value ) -> Value
withType type_ fields =
    Encode.object (( "type", Encode.string type_ ) :: fields)



---- DECODER


decoder : Decoder Pattern
decoder =
    Decode.succeed PatternData
        |> Decode.required "points" (Store.decoder pointDecoder)
        |> Decode.required "circles" (Store.decoder circleDecoder)
        |> Decode.required "lines" (Store.decoder lineDecoder)
        |> Decode.required "lineSegments" (Store.decoder lineSegmentDecoder)
        |> Decode.required "details" (Store.decoder detailDecoder)
        |> Decode.required "transformations" (Store.decoder transformationDecoder)
        |> Decode.required "variables" variablesDecoder
        |> Decode.hardcoded Nothing
        |> Decode.map Pattern


variablesDecoder : Decoder (Dict String String)
variablesDecoder =
    let
        pairDecoder =
            Decode.succeed Tuple.pair
                |> Decode.required "name" Decode.string
                |> Decode.required "value" Decode.string
    in
    Decode.list pairDecoder
        |> Decode.map Dict.fromList


pointDecoder : Decoder Point
pointDecoder =
    Decode.oneOf
        [ typeDecoder "origin" <|
            Decode.map2 Origin
                (Decode.field "x" Decode.float)
                (Decode.field "y" Decode.float)
        , typeDecoder "leftOf" <|
            Decode.map2 LeftOf
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" Decode.string)
        , typeDecoder "rightOf" <|
            Decode.map2 RightOf
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" Decode.string)
        , typeDecoder "above" <|
            Decode.map2 Above
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" Decode.string)
        , typeDecoder "below" <|
            Decode.map2 Below
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" Decode.string)
        , typeDecoder "atAngle" <|
            Decode.map3 AtAngle
                (Decode.field "anchor" That.decoder)
                (Decode.field "angle" Decode.string)
                (Decode.field "distance" Decode.string)
        , typeDecoder "betweenRatio" <|
            Decode.map3 BetweenRatio
                (Decode.field "anchorA" That.decoder)
                (Decode.field "anchorB" That.decoder)
                (Decode.field "ratio" Decode.string)
        , typeDecoder "betweenLength" <|
            Decode.map3 BetweenLength
                (Decode.field "anchorA" That.decoder)
                (Decode.field "anchorB" That.decoder)
                (Decode.field "length" Decode.string)
        , typeDecoder "firstCircleCircle" <|
            Decode.map2 FirstCircleCircle
                (Decode.field "circleA" That.decoder)
                (Decode.field "circleB" That.decoder)
        , typeDecoder "secondCircleCircle" <|
            Decode.map2 SecondCircleCircle
                (Decode.field "circleA" That.decoder)
                (Decode.field "circleB" That.decoder)
        , typeDecoder "lineLine" <|
            Decode.map2 LineLine
                (Decode.field "lineA" That.decoder)
                (Decode.field "lineB" That.decoder)
        , typeDecoder "firstCircleLine" <|
            Decode.map2 FirstCircleLine
                (Decode.field "circle" That.decoder)
                (Decode.field "line" That.decoder)
        , typeDecoder "secondCircleLine" <|
            Decode.map2 SecondCircleLine
                (Decode.field "circle" That.decoder)
                (Decode.field "line" That.decoder)
        ]


circleDecoder : Decoder Circle
circleDecoder =
    Decode.oneOf
        [ typeDecoder "centeredAt" <|
            Decode.map2 CenteredAt
                (Decode.field "center" That.decoder)
                (Decode.field "radius" Decode.string)
        ]


lineDecoder : Decoder Line
lineDecoder =
    Decode.oneOf
        [ typeDecoder "throughTwoPoints" <|
            Decode.map2 ThroughTwoPoints
                (Decode.field "anchorA" That.decoder)
                (Decode.field "anchorB" That.decoder)
        , typeDecoder "throughOnePoint" <|
            Decode.map2 ThroughOnePoint
                (Decode.field "anchor" That.decoder)
                (Decode.field "angle" Decode.string)
        ]


lineSegmentDecoder : Decoder LineSegment
lineSegmentDecoder =
    Decode.oneOf
        [ typeDecoder "fromTo" <|
            Decode.map2 FromTo
                (Decode.field "anchorA" That.decoder)
                (Decode.field "anchorB" That.decoder)
        ]


detailDecoder : Decoder Detail
detailDecoder =
    let
        segmentDecoder =
            Decode.map2 Tuple.pair
                (Decode.index 0 That.decoder)
                (Decode.index 1 connectionDecoder)

        connectionDecoder =
            Decode.oneOf
                [ typeDecoder "straight" (Decode.succeed Straight)
                , typeDecoder "cubic" <|
                    Decode.map Cubic
                        (Decode.field "knot" That.decoder)
                ]
    in
    Decode.oneOf
        [ typeDecoder "fromPoints" <|
            Decode.map FromPoints <|
                Decode.map3
                    (\firstPoint segments lastToFirst ->
                        { firstPoint = firstPoint
                        , segments = segments
                        , lastToFirst = lastToFirst
                        }
                    )
                    (Decode.field "firstPoint" That.decoder)
                    (Decode.field "segments" (Decode.list segmentDecoder))
                    (Decode.field "lastToFirst" connectionDecoder)
        ]


transformationDecoder : Decoder Transformation
transformationDecoder =
    Decode.oneOf
        [ typeDecoder "mirrorAt" <|
            Decode.map MirrorAt
                (Decode.field "line" That.decoder)
        , typeDecoder "rotateAround" <|
            Decode.map2 RotateAround
                (Decode.field "point" That.decoder)
                (Decode.field "angle" Decode.string)
        ]


typeDecoder : String -> Decoder a -> Decoder a
typeDecoder type_ dataDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\rawType ->
                if rawType == type_ then
                    dataDecoder

                else
                    Decode.fail "not a valid type"
            )
