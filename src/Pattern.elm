module Pattern exposing
    ( Pattern
    , empty
    , Geometry, Segment(..), geometry, Problems
    , getPointGeometries, getPointGeometry
    , getCircleGeometry
    , getLineSegmentGeometry
    , point2d, circle2d, axis2d
    , computeCache
    , variables, insertVariable, removeVariable
    , Point(..), points, insertPoint, getPoint, updatePoint
    , origin
    , leftOf, rightOf, above, below
    , atAngle
    , betweenRatio, betweenLength
    , firstCircleCircle, secondCircleCircle, lineLine, firstCircleLine, secondCircleLine
    , Circle(..), circles, insertCircle, getCircle, updateCircle
    , centeredAt
    , Line(..), lines, insertLine, getLine, updateLine
    , throughTwoPoints, throughOnePoint
    , LineSegment(..), lineSegments, insertLineSegment, getLineSegment
    , Detail(..), Connection(..), details, insertDetail, updateDetail, deleteDetail, getDetail
    , Transformation(..), insertTransformation
    , decoder, encode
    )

{-|

@docs Pattern

@docs empty


# Geometry

@docs Geometry, Segment, geometry, Problems

@docs getPointGeometries, getPointGeometry

@docs getCircleGeometry

@docs getLineSegmentGeometry

@docs point2d, circle2d, axis2d

@docs computeCache


# Variables

@docs Variable, variables, insertVariable, removeVariable, renameVariable


# Objects


## Points

@docs Point, points, insertPoint, getPoint, updatePoint

@docs origin

@docs leftOf, rightOf, above, below

@docs atAngle

@docs betweenRatio, betweenLength

@docs firstCircleCircle, secondCircleCircle, lineLine, firstCircleLine, secondCircleLine


## Circles

@docs Circle, circles, insertCircle, getCircle, updateCircle

@docs centeredAt


## Lines

@docs Line, lines, insertLine, getLine, updateLine

@docs throughTwoPoints, throughOnePoint


## Line Segments

@docs LineSegment, lineSegments, insertLineSegment, getLineSegment


## Details

@docs Detail, Connection, details, insertDetail, updateDetail, deleteDetail, getDetail


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
import Direction2d exposing (Direction2d)
import Expr exposing (BoolExpr(..), Expr(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser
import Point2d exposing (Point2d)
import Point2d.Extra as Point2d
import Polygon2d exposing (Polygon2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Set exposing (Set)
import State exposing (State)
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
    , cache : Cache
    }


type alias Cache =
    { variables : Dict String Float
    , point2ds : Dict Int Point2d
    , circle2ds : Dict Int Circle2d
    , axis2ds : Dict Int Axis2d
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
        , cache = emptyCache
        }


emptyCache : Cache
emptyCache =
    { variables = Dict.empty
    , point2ds = Dict.empty
    , circle2ds = Dict.empty
    , axis2ds = Dict.empty
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
      --| OnCurveRatio (That Curve) String
      --| OnCurveLength (That Curve) String
      --| OnCircleFirstTangent (That Circle) (That Point)
      --| OnCircleSecondTangent (That Circle) (That Point)
      --| OnCircleFirstChord (That Circle) String
      --| OnCircleSecondChord (That Circle) String
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
    | Quadratic (That Point)



---- TRANSFORMATIONS


type Transformation
    = MirrorAt (That Line)
    | RotateAround (That Point) String



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
    | QuadraticSpline QuadraticSpline2d


type alias Problems =
    { doNotCompute : List ( String, DoesNotCompute )
    , missingPoints : List (That Point)
    , missingCircles : List (That Circle)
    , missingLines : List (That Line)
    }


geometry : Pattern -> ( Geometry, Problems )
geometry ((Pattern data) as pattern) =
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


computeCache : Pattern -> Pattern
computeCache =
    computeVariablesCache
        >> computePoint2dCache
        >> computeCircle2dCache
        >> computeAxis2dCache


computePoint2dCache : Pattern -> Pattern
computePoint2dCache ((Pattern data) as pattern) =
    Pattern
        { data
            | cache =
                pattern
                    |> points
                    |> List.map Tuple.first
                    |> State.traverse (point2dHelper pattern)
                    |> State.finalState data.cache
        }


point2d : Pattern -> That Point -> Maybe Point2d
point2d ((Pattern { cache }) as pattern) thatPoint =
    State.finalValue cache (point2dHelper pattern thatPoint)


point2dHelper : Pattern -> That Point -> State Cache (Maybe Point2d)
point2dHelper pattern thatPoint =
    let
        modifyWhenNeeded cache =
            case Dict.get (That.objectId thatPoint) cache.point2ds of
                Nothing ->
                    thatPoint
                        |> getPoint pattern
                        |> Maybe.map (.value >> calculatePoint2d pattern)
                        |> Maybe.withDefault (State.state Nothing)
                        |> State.andThen addPoint2d

                Just value ->
                    State.state (Just value)

        addPoint2d maybeValue =
            State.modify
                (\cache ->
                    case maybeValue of
                        Nothing ->
                            cache

                        Just value ->
                            { cache
                                | point2ds =
                                    Dict.insert (That.objectId thatPoint)
                                        value
                                        cache.point2ds
                            }
                )
                |> State.map (\_ -> maybeValue)
    in
    State.get
        |> State.andThen modifyWhenNeeded


calculatePoint2d : Pattern -> Point -> State Cache (Maybe Point2d)
calculatePoint2d pattern point =
    let
        simpleDistance thatAnchor rawLength toDirection =
            State.map2
                (Maybe.map2
                    (\anchor distance ->
                        Point2d.translateBy (toDirection distance) anchor
                    )
                )
                (point2dHelper pattern thatAnchor)
                (evaluateRawHelper pattern rawLength)
    in
    case point of
        Origin x y ->
            State.state <|
                Just (Point2d.fromCoordinates ( x, y ))

        LeftOf anchor rawLength ->
            simpleDistance anchor rawLength <|
                \distance -> Vector2d.fromComponents ( -1 * distance, 0 )

        RightOf anchor rawLength ->
            simpleDistance anchor rawLength <|
                \distance -> Vector2d.fromComponents ( distance, 0 )

        Above anchor rawLength ->
            simpleDistance anchor rawLength <|
                \distance -> Vector2d.fromComponents ( 0, -1 * distance )

        Below anchor rawLength ->
            simpleDistance anchor rawLength <|
                \distance -> Vector2d.fromComponents ( 0, distance )

        AtAngle thatAnchor rawAngle rawDistance ->
            State.map3 (Maybe.map3 Point2d.atAngle)
                (point2dHelper pattern thatAnchor)
                (evaluateRawHelper pattern rawAngle)
                (evaluateRawHelper pattern rawDistance)

        BetweenRatio thatAnchorA thatAnchorB rawRatio ->
            State.map3 (Maybe.map3 Point2d.betweenRatio)
                (point2dHelper pattern thatAnchorA)
                (point2dHelper pattern thatAnchorB)
                (evaluateRawHelper pattern rawRatio)

        BetweenLength thatAnchorA thatAnchorB rawLength ->
            State.map3 (Maybe.andThen3 Point2d.betweenLength)
                (point2dHelper pattern thatAnchorA)
                (point2dHelper pattern thatAnchorB)
                (evaluateRawHelper pattern rawLength)

        FirstCircleCircle thatCircleA thatCircleB ->
            State.map2 (Maybe.andThen2 Point2d.firstCircleCircle)
                (circle2dHelper pattern thatCircleA)
                (circle2dHelper pattern thatCircleB)

        SecondCircleCircle thatCircleA thatCircleB ->
            State.map2 (Maybe.andThen2 Point2d.secondCircleCircle)
                (circle2dHelper pattern thatCircleA)
                (circle2dHelper pattern thatCircleB)

        LineLine thatLineA thatLineB ->
            State.map2 (Maybe.andThen2 Axis2d.intersectionWithAxis)
                (axis2dHelper pattern thatLineA)
                (axis2dHelper pattern thatLineB)

        FirstCircleLine thatCircle thatLine ->
            State.map2 (Maybe.andThen2 Point2d.firstCircleAxis)
                (circle2dHelper pattern thatCircle)
                (axis2dHelper pattern thatLine)

        SecondCircleLine thatCircle thatLine ->
            State.map2 (Maybe.andThen2 Point2d.secondCircleAxis)
                (circle2dHelper pattern thatCircle)
                (axis2dHelper pattern thatLine)

        TransformBy _ _ ->
            State.state Nothing


computeCircle2dCache : Pattern -> Pattern
computeCircle2dCache ((Pattern data) as pattern) =
    Pattern
        { data
            | cache =
                pattern
                    |> circles
                    |> List.map Tuple.first
                    |> State.traverse (circle2dHelper pattern)
                    |> State.finalState data.cache
        }


circle2d : Pattern -> That Circle -> Maybe Circle2d
circle2d ((Pattern { cache }) as pattern) thatCircle =
    State.finalValue cache (circle2dHelper pattern thatCircle)


circle2dHelper : Pattern -> That Circle -> State Cache (Maybe Circle2d)
circle2dHelper pattern thatCircle =
    let
        modifyWhenNeeded cache =
            case Dict.get (That.objectId thatCircle) cache.circle2ds of
                Nothing ->
                    thatCircle
                        |> getCircle pattern
                        |> Maybe.map (.value >> calculateCircle2d pattern)
                        |> Maybe.withDefault (State.state Nothing)
                        |> State.andThen addCircle2d

                Just value ->
                    State.state (Just value)

        addCircle2d maybeValue =
            State.modify
                (\cache ->
                    case maybeValue of
                        Nothing ->
                            cache

                        Just value ->
                            { cache
                                | circle2ds =
                                    Dict.insert (That.objectId thatCircle)
                                        value
                                        cache.circle2ds
                            }
                )
                |> State.map (\_ -> maybeValue)
    in
    State.get
        |> State.andThen modifyWhenNeeded


calculateCircle2d : Pattern -> Circle -> State Cache (Maybe Circle2d)
calculateCircle2d pattern circle =
    case circle of
        CenteredAt thatCenter rawRadius ->
            State.map2 (Maybe.map2 Circle2d.withRadius)
                (evaluateRawHelper pattern rawRadius)
                (point2dHelper pattern thatCenter)


computeAxis2dCache : Pattern -> Pattern
computeAxis2dCache ((Pattern data) as pattern) =
    Pattern
        { data
            | cache =
                pattern
                    |> lines
                    |> List.map Tuple.first
                    |> State.traverse (axis2dHelper pattern)
                    |> State.finalState data.cache
        }


axis2d : Pattern -> That Line -> Maybe Axis2d
axis2d ((Pattern { cache }) as pattern) thatLine =
    State.finalValue cache (axis2dHelper pattern thatLine)


axis2dHelper : Pattern -> That Line -> State Cache (Maybe Axis2d)
axis2dHelper pattern thatLine =
    let
        modifyWhenNeeded cache =
            case Dict.get (That.objectId thatLine) cache.axis2ds of
                Nothing ->
                    thatLine
                        |> getLine pattern
                        |> Maybe.map (.value >> calculateAxis2d pattern)
                        |> Maybe.withDefault (State.state Nothing)
                        |> State.andThen addLine2d

                Just value ->
                    State.state (Just value)

        addLine2d maybeValue =
            State.modify
                (\cache ->
                    case maybeValue of
                        Nothing ->
                            cache

                        Just value ->
                            { cache
                                | axis2ds =
                                    Dict.insert (That.objectId thatLine)
                                        value
                                        cache.axis2ds
                            }
                )
                |> State.map (\_ -> maybeValue)
    in
    State.get
        |> State.andThen modifyWhenNeeded


calculateAxis2d : Pattern -> Line -> State Cache (Maybe Axis2d)
calculateAxis2d pattern line =
    case line of
        ThroughTwoPoints thatPointA thatPointB ->
            State.map2 (Maybe.andThen2 Axis2d.throughTwoPoints)
                (point2dHelper pattern thatPointA)
                (point2dHelper pattern thatPointB)

        ThroughOnePoint thatPoint rawAngle ->
            State.map2 (Maybe.map2 Axis2d.throughOnePoint)
                (point2dHelper pattern thatPoint)
                (evaluateRawHelper pattern rawAngle)


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
            Maybe.map2 Tuple.pair
                (point2d p firstPoint)
                computedSegments
                |> Maybe.andThen
                    (\( firstPoint2d, otherPoints ) ->
                        let
                            lineSegmentHelp connection p2dA p2dB =
                                case connection of
                                    Straight ->
                                        Just (LineSegment (LineSegment2d.from p2dA p2dB))

                                    Quadratic thatKnot ->
                                        point2d p thatKnot
                                            |> Maybe.map
                                                (\knot2d ->
                                                    QuadraticSpline <|
                                                        QuadraticSpline2d.with
                                                            { startPoint = p2dA
                                                            , controlPoint = knot2d
                                                            , endPoint = p2dB
                                                            }
                                                )

                            maybeLastSegment =
                                otherPoints
                                    |> List.reverse
                                    |> List.head
                                    |> Maybe.andThen
                                        (\( lastPoint2d, _ ) ->
                                            lineSegmentHelp
                                                lastToFirst
                                                lastPoint2d
                                                firstPoint2d
                                        )

                            maybeOtherSegments =
                                otherPoints
                                    |> List.foldl
                                        (\( nextPoint2d, connection ) result ->
                                            case result of
                                                Nothing ->
                                                    Nothing

                                                Just ( previousPoint2d, list ) ->
                                                    case
                                                        lineSegmentHelp connection
                                                            previousPoint2d
                                                            nextPoint2d
                                                    of
                                                        Nothing ->
                                                            Nothing

                                                        Just newSegment ->
                                                            Just
                                                                ( nextPoint2d
                                                                , newSegment :: list
                                                                )
                                        )
                                        (Just ( firstPoint2d, [] ))
                                    |> Maybe.map (Tuple.second >> List.reverse)
                        in
                        Maybe.map2
                            (\otherSegments lastSegment ->
                                otherSegments ++ [ lastSegment ]
                            )
                            maybeOtherSegments
                            maybeLastSegment
                    )



---- VARIABLES


insertVariable : String -> String -> Pattern -> Pattern
insertVariable name value ((Pattern data) as pattern) =
    case Expr.parse reservedWords value of
        Err _ ->
            pattern

        Ok _ ->
            computeVariablesCache <|
                Pattern { data | variables = Dict.insert name value data.variables }


removeVariable : String -> Pattern -> Pattern
removeVariable name (Pattern data) =
    computeVariablesCache <|
        Pattern
            { data
                | variables = Dict.remove name data.variables
                , cache = emptyCache
            }


computeVariablesCache : Pattern -> Pattern
computeVariablesCache ((Pattern data) as pattern) =
    Pattern
        { data
            | cache =
                Dict.keys data.variables
                    |> State.traverse (variableNamedHelper pattern)
                    |> State.finalState data.cache
        }


variables : Pattern -> List { name : String, value : String, computed : Float }
variables ((Pattern data) as pattern) =
    Dict.keys data.variables
        |> State.traverse (variableNamedHelper pattern)
        |> State.finalValue data.cache
        |> List.filterMap identity


variable : Pattern -> String -> Maybe Float
variable ((Pattern { cache }) as pattern) name =
    State.finalValue cache (variableHelper pattern name)


variableHelper : Pattern -> String -> State Cache (Maybe Float)
variableHelper ((Pattern data) as pattern) name =
    let
        modifyWhenNeeded cache =
            case Dict.get name cache.variables of
                Nothing ->
                    calculateVariable name
                        |> State.andThen addVariable

                Just value ->
                    State.state (Just value)

        calculateVariable n =
            data.variables
                |> Dict.get n
                |> Maybe.andThen (Expr.parse reservedWords >> Result.toMaybe)
                |> Maybe.map (evaluateHelper pattern >> State.map Result.toMaybe)
                |> Maybe.withDefault (State.state Nothing)

        addVariable maybeValue =
            State.modify
                (\cache ->
                    case maybeValue of
                        Nothing ->
                            cache

                        Just value ->
                            { cache | variables = Dict.insert name value cache.variables }
                )
                |> State.map (\_ -> maybeValue)
    in
    State.get
        |> State.andThen modifyWhenNeeded


variableNamedHelper :
    Pattern
    -> String
    ->
        State Cache
            (Maybe
                { name : String
                , value : String
                , computed : Float
                }
            )
variableNamedHelper ((Pattern data) as pattern) name =
    State.map
        (Maybe.map2
            (\value computed ->
                { name = name
                , value = value
                , computed = computed
                }
            )
            (Dict.get name data.variables)
        )
        (variableHelper pattern name)


evaluate : Pattern -> Expr -> Result DoesNotCompute Float
evaluate ((Pattern data) as pattern) expr =
    State.finalValue data.cache (evaluateHelper pattern expr)


evaluateRawHelper : Pattern -> String -> State Cache (Maybe Float)
evaluateRawHelper pattern rawExpr =
    Expr.parse reservedWords rawExpr
        |> Result.toMaybe
        |> Maybe.map
            (evaluateHelper pattern
                >> State.map Result.toMaybe
            )
        |> Maybe.withDefault (State.state Nothing)


evaluateHelper : Pattern -> Expr -> State Cache (Result DoesNotCompute Float)
evaluateHelper pattern expr =
    case expr of
        Number float ->
            State.state (Ok float)

        Variable variableName ->
            State.map (Result.fromMaybe TODO)
                (variableHelper pattern variableName)

        Function functionName args ->
            case functionName of
                "distance" ->
                    case args of
                        namePointA :: namePointB :: [] ->
                            Maybe.map2
                                (\thatPointA thatPointB ->
                                    State.map2 (Maybe.map2 Point2d.distanceFrom)
                                        (point2dHelper pattern thatPointA)
                                        (point2dHelper pattern thatPointB)
                                        |> State.map (Result.fromMaybe TODO)
                                )
                                (thatPointByName pattern namePointA)
                                (thatPointByName pattern namePointB)
                                |> Maybe.withDefault (State.state (Err TODO))

                        _ ->
                            State.state <|
                                Err <|
                                    BadArguments
                                        { functionName = functionName
                                        , arguments = args
                                        }

                "angleOfLine" ->
                    case args of
                        namePointA :: namePointB :: [] ->
                            Maybe.map2
                                (\thatPointA thatPointB ->
                                    State.map2 (Maybe.andThen2 Direction2d.from)
                                        (point2dHelper pattern thatPointA)
                                        (point2dHelper pattern thatPointB)
                                        |> State.map (Maybe.map Direction2d.toAngle)
                                        |> State.map (Maybe.map (\radian -> 180 * radian / pi))
                                        |> State.map (Result.fromMaybe TODO)
                                )
                                (thatPointByName pattern namePointA)
                                (thatPointByName pattern namePointB)
                                |> Maybe.withDefault (State.state (Err TODO))

                        _ ->
                            State.state <|
                                Err <|
                                    BadArguments
                                        { functionName = functionName
                                        , arguments = args
                                        }

                _ ->
                    State.state (Err (UnknownFunction functionName))

        Sum exprA exprB ->
            State.map2
                (Result.map2 (\a b -> a + b))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        Difference exprA exprB ->
            State.map2
                (Result.map2 (\a b -> a - b))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        Product exprA exprB ->
            State.map2
                (Result.map2 (\a b -> a * b))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        Quotient exprA exprB ->
            State.map2 (Result.map2 (\a b -> a / b))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        Max exprA exprB ->
            State.map2
                (Result.map2 Basics.max)
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        IfThenElse boolExpr exprA exprB ->
            State.map3
                (Result.map3
                    (\bool a b ->
                        if bool then
                            a

                        else
                            b
                    )
                )
                (evaluateBoolHelper pattern boolExpr)
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)


evaluateBoolHelper : Pattern -> BoolExpr -> State Cache (Result DoesNotCompute Bool)
evaluateBoolHelper pattern boolExpr =
    case boolExpr of
        ExprTrue ->
            State.state (Ok True)

        ExprFalse ->
            State.state (Ok False)

        Not nestedBoolExpr ->
            State.map (Result.map not)
                (evaluateBoolHelper pattern nestedBoolExpr)

        And boolExprA boolExprB ->
            State.map2 (Result.map2 (&&))
                (evaluateBoolHelper pattern boolExprA)
                (evaluateBoolHelper pattern boolExprB)

        Or boolExprA boolExprB ->
            State.map2 (Result.map2 (||))
                (evaluateBoolHelper pattern boolExprA)
                (evaluateBoolHelper pattern boolExprB)

        Equal exprA exprB ->
            State.map2 (Result.map2 (==))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        GreaterThan exprA exprB ->
            State.map2 (Result.map2 (>))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)

        StrictlyGreaterThan exprA exprB ->
            State.map2 (Result.map2 (>=))
                (evaluateHelper pattern exprA)
                (evaluateHelper pattern exprB)


reservedWords : List String
reservedWords =
    [ "distance"
    , "angleOfLine"
    ]


type DoesNotCompute
    = TODO
    | UnknownFunction String
    | BadArguments
        { functionName : String
        , arguments : List String
        }
    | MissingVariable String
    | RecursiveExpression (List String)



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
            | points = Store.updateValue (That.objectId thatPoint) point pattern.points
            , cache = emptyCache
        }
        |> computeCache


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
    ( computePoint2dCache <|
        Pattern { pattern | points = newPoints }
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


getCircleGeometry : Pattern -> That Circle -> Maybe Circle2d
getCircleGeometry pattern thatCircle =
    circle2d pattern thatCircle


updateCircle : That Circle -> Circle -> Pattern -> Pattern
updateCircle thatcircle circle (Pattern pattern) =
    Pattern
        { pattern
            | circles = Store.updateValue (That.objectId thatcircle) circle pattern.circles
            , cache = emptyCache
        }
        |> computeCache


insertCircle : Maybe String -> Circle -> Pattern -> ( Pattern, That Circle )
insertCircle name circle (Pattern pattern) =
    let
        ( newCircles, id ) =
            Store.insert name circle pattern.circles
    in
    ( computeCircle2dCache <|
        Pattern { pattern | circles = newCircles }
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
    ( computeAxis2dCache <|
        Pattern { pattern | lines = newLines }
    , that id
    )


updateLine : That Line -> Line -> Pattern -> Pattern
updateLine thatLine line (Pattern pattern) =
    Pattern
        { pattern
            | lines = Store.updateValue (That.objectId thatLine) line pattern.lines
            , cache = emptyCache
        }
        |> computeCache



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


getLineSegmentGeometry : Pattern -> That LineSegment -> Maybe LineSegment2d
getLineSegmentGeometry pattern thatLineSegment =
    lineSegment2d pattern thatLineSegment


insertLineSegment : Maybe String -> LineSegment -> Pattern -> ( Pattern, That LineSegment )
insertLineSegment maybeName lineSegment (Pattern pattern) =
    let
        ( newLineSegments, id ) =
            Store.insert maybeName lineSegment pattern.lineSegments
    in
    ( computeAxis2dCache <|
        Pattern { pattern | lineSegments = newLineSegments }
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
            , cache = emptyCache
        }
        |> computeCache
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
    ( Pattern { pattern | details = newDetails }
    , that id
    )


updateDetail : That Detail -> Detail -> Pattern -> Pattern
updateDetail thatDetail detail (Pattern pattern) =
    Pattern
        { pattern
            | details = Store.updateValue (That.objectId thatDetail) detail pattern.details
        }


deleteDetail : That Detail -> Pattern -> Pattern
deleteDetail thatDetail (Pattern pattern) =
    Pattern
        { pattern
            | details = Store.remove (That.objectId thatDetail) pattern.details
        }


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
        , ( "cache", encodeCache pattern.cache )
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

        TransformBy transformation target ->
            withType "transformBy"
                [ ( "transformation", That.encode transformation )
                , ( "target", That.encode target )
                ]


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

        Quadratic thatPoint ->
            withType "quadratic"
                [ ( "controlPoint", That.encode thatPoint ) ]


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


encodeCache : Cache -> Value
encodeCache cache =
    Encode.object
        [ ( "variables"
          , Encode.dict identity Encode.float cache.variables
          )
        , ( "point2ds"
          , Encode.dict String.fromInt encodePoint2d cache.point2ds
          )
        , ( "circle2ds"
          , Encode.dict String.fromInt encodeCircle2d cache.circle2ds
          )
        , ( "axis2ds"
          , Encode.dict String.fromInt encodeAxis2d cache.axis2ds
          )
        ]


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encodeA maybeA =
    case maybeA of
        Nothing ->
            Encode.null

        Just a ->
            encodeA a


encodePoint2d : Point2d -> Value
encodePoint2d point =
    Encode.object
        [ ( "x", Encode.float (Point2d.xCoordinate point) )
        , ( "y", Encode.float (Point2d.yCoordinate point) )
        ]


encodeCircle2d : Circle2d -> Value
encodeCircle2d circle =
    Encode.object
        [ ( "radius", Encode.float (Circle2d.radius circle) )
        , ( "center", encodePoint2d (Circle2d.centerPoint circle) )
        ]


encodeAxis2d : Axis2d -> Value
encodeAxis2d axis =
    Encode.object
        [ ( "origin", encodePoint2d (Axis2d.originPoint axis) )
        , ( "direction", encodeDirection2d (Axis2d.direction axis) )
        ]


encodeDirection2d : Direction2d -> Value
encodeDirection2d direction =
    Encode.object
        [ ( "x", Encode.float (Direction2d.xComponent direction) )
        , ( "y", Encode.float (Direction2d.yComponent direction) )
        ]



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
        |> Decode.optional "cache" cacheDecoder emptyCache
        |> Decode.map
            (Pattern >> computeCache)


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
        , typeDecoder "transformBy" <|
            Decode.map2 TransformBy
                (Decode.field "transformation" That.decoder)
                (Decode.field "target" That.decoder)
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
                , typeDecoder "quadratic" <|
                    Decode.map Quadratic
                        (Decode.field "controlPoint" That.decoder)
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


cacheDecoder : Decoder Cache
cacheDecoder =
    Decode.succeed Cache
        |> Decode.required "variables" (Decode.dict Decode.float)
        |> Decode.required "point2ds" (dictIntDecoder point2dDecoder)
        |> Decode.required "circle2ds" (dictIntDecoder circle2dDecoder)
        |> Decode.required "axis2ds" (dictIntDecoder axis2dDecoder)


dictIntDecoder : Decoder a -> Decoder (Dict Int a)
dictIntDecoder aDecoder =
    Decode.keyValuePairs aDecoder
        |> Decode.map
            (List.filterMap
                (\( key, value ) ->
                    case String.toInt key of
                        Nothing ->
                            Nothing

                        Just int ->
                            Just ( int, value )
                )
                >> Dict.fromList
            )


point2dDecoder : Decoder Point2d
point2dDecoder =
    Decode.succeed
        (\x y ->
            Point2d.fromCoordinates ( x, y )
        )
        |> Decode.required "x" Decode.float
        |> Decode.required "y" Decode.float


circle2dDecoder : Decoder Circle2d
circle2dDecoder =
    Decode.succeed Circle2d.withRadius
        |> Decode.required "radius" Decode.float
        |> Decode.required "center" point2dDecoder


axis2dDecoder : Decoder Axis2d
axis2dDecoder =
    Decode.succeed Axis2d.through
        |> Decode.required "origin" point2dDecoder
        |> Decode.required "direction" direction2dDecoder


direction2dDecoder : Decoder Direction2d
direction2dDecoder =
    Decode.succeed
        (\x y -> Direction2d.unsafe ( x, y ))
        |> Decode.required "x" Decode.float
        |> Decode.required "y" Decode.float
