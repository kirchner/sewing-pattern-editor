module Pattern exposing
    ( Pattern, empty
    , Point, Axis, Circle, Curve, Detail
    , Intersectable
    , A, this, name, inlined, hash
    , intersectableAxis, intersectableCircle, intersectableCurve
    , whichSize, IntersectableTag(..), tagFromIntersectable
    , insertPoint, insertAxis, insertCircle, insertCurve, insertDetail
    , insertTransformation
    , insertVariable
    , InsertHelp(..)
    , points, axes, circles, curves, details, transformations, variables
    , pointInfo, PointInfo(..)
    , axisInfo, AxisInfo(..)
    , circleInfo, CircleInfo(..)
    , curveInfo, CurveInfo(..)
    , detailInfo, DetailInfo, FirstCurve(..), NextCurve(..), LastCurve(..)
    , transformationInfo, TransformationInfo(..)
    , variableInfo
    , Direction(..), Orientation(..), OneInTwo(..)
    , point2d, axis2d, circle2d, curve2d, detail2d, intersectable2d
    , float
    , Curve2d(..)
    , Detail2d, NextCurve2d(..), LastCurve2d(..)
    , Intersectable2d(..)
    , ComputeHelp(..)
    , origin
    , fromOnePoint, FromOnePointHelp
    , betweenRatio, BetweenRatioHelp
    , betweenLength, BetweenLengthHelp
    , intersection, IntersectionHelp
    , transformedPoint
    , throughOnePoint, ThroughOnePointHelp
    , throughTwoPoints, ThroughTwoPointsHelp
    , transformedAxis
    , withRadius, WithRadiusHelp
    , throughThreePoints, ThroughThreePointsHelp
    , transformedCircle
    , straight, StraightHelp
    , quadratic, QuadraticHelp
    , cubic, CubicHelp
    , transformedCurve
    , detail, DetailHelp
    , checkExpr, ExprHelp(..)
    , encode, decoder
    )

{-|

@docs Pattern, empty

@docs Point, Axis, Circle, Curve, Detail
@docs Intersectable


# Nest and reference

@docs A, this, name, inlined, hash
@docs intersectableAxis, intersectableCircle, intersectableCurve
@docs whichSize, IntersectableTag, tagFromIntersectable


# Insert

@docs insertPoint, insertAxis, insertCircle, insertCurve, insertDetail
@docs insertTransformation
@docs insertVariable
@docs InsertHelp


# Query

@docs points, axes, circles, curves, details, transformations, variables


## Info

@docs pointInfo, PointInfo
@docs axisInfo, AxisInfo
@docs circleInfo, CircleInfo
@docs curveInfo, CurveInfo
@docs detailInfo, DetailInfo, FirstCurve, NextCurve, LastCurve
@docs transformationInfo, TransformationInfo
@docs variableInfo


## Shared types

@docs Direction, Orientation, OneInTwo


# Compute

@docs point2d, axis2d, circle2d, curve2d, detail2d, intersectable2d
@docs float

@docs Curve2d

@docs Detail2d, NextCurve2d, LastCurve2d

@docs Intersectable2d

@docs ComputeHelp


# Construct


## Point

@docs origin
@docs fromOnePoint, FromOnePointHelp
@docs betweenRatio, BetweenRatioHelp
@docs betweenLength, BetweenLengthHelp
@docs intersection, IntersectionHelp
@docs transformedPoint


## Axis

@docs throughOnePoint, ThroughOnePointHelp
@docs throughTwoPoints, ThroughTwoPointsHelp
@docs transformedAxis


## Circle

@docs withRadius, WithRadiusHelp
@docs throughThreePoints, ThroughThreePointsHelp
@docs transformedCircle


## Curve

@docs straight, StraightHelp
@docs quadratic, QuadraticHelp
@docs cubic, CubicHelp
@docs transformedCurve


## Detail

@docs detail, DetailHelp


## Expressions

@docs checkExpr, ExprHelp


# JSON

@docs encode, decoder

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
import CubicSpline2d exposing (CubicSpline2d)
import Dict exposing (Dict)
import Direction2d
import Expr exposing (BoolExpr(..), Expr(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import LineSegment2d exposing (LineSegment2d)
import Parser exposing (DeadEnd)
import Point2d exposing (Point2d)
import Point2d.Extra as Point2d
import QuadraticSpline2d exposing (QuadraticSpline2d)
import State exposing (State)
import StateResult
import Vector2d


type Pattern
    = Pattern PatternData


type alias PatternData =
    { points : Dict String Point
    , axes : Dict String Axis
    , circles : Dict String Circle
    , curves : Dict String Curve
    , details : Dict String Detail
    , transformations : Dict String Transformation
    , variables : Dict String String

    -- CACHE
    , point2ds : Dict String (Result ComputeHelp Point2d)
    , axis2ds : Dict String (Result ComputeHelp Axis2d)
    , circle2ds : Dict String (Result ComputeHelp Circle2d)
    , curve2ds : Dict String (Result ComputeHelp Curve2d)
    , detail2ds : Dict String (Result ComputeHelp Detail2d)
    , variablesCache : Dict String (Result ComputeHelp Float)
    }


empty : Pattern
empty =
    Pattern
        { points = Dict.empty
        , axes = Dict.empty
        , circles = Dict.empty
        , curves = Dict.empty
        , details = Dict.empty
        , transformations = Dict.empty
        , variables = Dict.empty

        -- CACHE
        , point2ds = Dict.empty
        , axis2ds = Dict.empty
        , circle2ds = Dict.empty
        , curve2ds = Dict.empty
        , detail2ds = Dict.empty
        , variablesCache = Dict.empty
        }



---- OBJECTS


type Point
    = Point PointInfo


type Axis
    = Axis AxisInfo


type Circle
    = Circle CircleInfo


type Curve
    = Curve CurveInfo


type Detail
    = Detail DetailInfo


type Transformation
    = Transformation TransformationInfo


type Intersectable
    = IntersectableAxis Axis
    | IntersectableCircle Circle
    | IntersectableCurve Curve


type IntersectableTag
    = IntersectableAxisTag
    | IntersectableCircleTag
    | IntersectableCurveTag



---- NEST AND REFERENCE


type A object
    = That String
    | This object


this : object -> A object
this object =
    This object


name : A object -> Maybe String
name aObject =
    case aObject of
        That n ->
            Just n

        This _ ->
            Nothing


inlined : A object -> Bool
inlined aObject =
    case aObject of
        That _ ->
            False

        This _ ->
            True


hash : A object -> String
hash aObject =
    case aObject of
        That n ->
            n

        This _ ->
            Debug.todo "implement"


intersectableAxis : A Axis -> A Intersectable
intersectableAxis aAxis =
    case aAxis of
        That name_ ->
            That name_

        This axis ->
            This (IntersectableAxis axis)


intersectableCircle : A Circle -> A Intersectable
intersectableCircle aCircle =
    case aCircle of
        That name_ ->
            That name_

        This circle ->
            This (IntersectableCircle circle)


intersectableCurve : A Curve -> A Intersectable
intersectableCurve aCurve =
    case aCurve of
        That name_ ->
            That name_

        This curve ->
            This (IntersectableCurve curve)



---- INSERT


insertPoint : String -> Point -> Pattern -> Result InsertHelp Pattern
insertPoint name_ point ((Pattern data) as pattern) =
    if nameTaken name_ data then
        Err NameTaken

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern { data | points = Dict.insert name_ point data.points })
                    (point2d (That name_))
        in
        case result of
            Err computeHelp ->
                Err (BadObject computeHelp)

            Ok _ ->
                Ok newPattern


insertAxis : String -> Axis -> Pattern -> Result InsertHelp Pattern
insertAxis name_ axis ((Pattern data) as pattern) =
    if nameTaken name_ data then
        Err NameTaken

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern { data | axes = Dict.insert name_ axis data.axes })
                    (computeAxis2d axis)
        in
        case result of
            Err computeHelp ->
                Err (BadObject computeHelp)

            Ok _ ->
                Ok newPattern


insertCircle : String -> Circle -> Pattern -> Result InsertHelp Pattern
insertCircle name_ circle ((Pattern data) as pattern) =
    if nameTaken name_ data then
        Err NameTaken

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern { data | circles = Dict.insert name_ circle data.circles })
                    (computeCircle2d circle)
        in
        case result of
            Err computeHelp ->
                Err (BadObject computeHelp)

            Ok _ ->
                Ok newPattern


insertCurve : String -> Curve -> Pattern -> Result InsertHelp Pattern
insertCurve name_ curve ((Pattern data) as pattern) =
    if nameTaken name_ data then
        Err NameTaken

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern { data | curves = Dict.insert name_ curve data.curves })
                    (computeCurve2d curve)
        in
        case result of
            Err computeHelp ->
                Err (BadObject computeHelp)

            Ok _ ->
                Ok newPattern


insertDetail : String -> Detail -> Pattern -> Result InsertHelp Pattern
insertDetail name_ detail_ ((Pattern data) as pattern) =
    if nameTaken name_ data then
        Err NameTaken

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern { data | details = Dict.insert name_ detail_ data.details })
                    (computeDetail2d detail_)
        in
        case result of
            Err computeHelp ->
                Err (BadObject computeHelp)

            Ok _ ->
                Ok newPattern


insertTransformation : String -> Transformation -> Pattern -> Result InsertHelp Pattern
insertTransformation n transformation pattern =
    Debug.todo "implement"


insertVariable : String -> String -> Pattern -> Result InsertHelp Pattern
insertVariable name_ expr ((Pattern data) as pattern) =
    if nameTaken name_ data then
        Err NameTaken

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern { data | variables = Dict.insert name_ expr data.variables })
                    (float name_)
        in
        case result of
            Err computeHelp ->
                Err (BadObject computeHelp)

            Ok _ ->
                Ok newPattern


type InsertHelp
    = NameTaken
    | BadObject ComputeHelp


nameTaken : String -> PatternData -> Bool
nameTaken name_ data =
    Dict.member name_ data.points
        || Dict.member name_ data.axes
        || Dict.member name_ data.circles
        || Dict.member name_ data.curves
        || Dict.member name_ data.details
        || Dict.member name_ data.transformations
        || Dict.member name_ data.variables



---- QUERY


points : Pattern -> List (A Point)
points (Pattern data) =
    Dict.keys data.points
        |> List.map That


axes : Pattern -> List (A Axis)
axes (Pattern data) =
    Dict.keys data.axes
        |> List.map That


circles : Pattern -> List (A Circle)
circles (Pattern data) =
    Dict.keys data.circles
        |> List.map That


curves : Pattern -> List (A Curve)
curves (Pattern data) =
    Dict.keys data.curves
        |> List.map That


details : Pattern -> List (A Detail)
details (Pattern data) =
    Dict.keys data.details
        |> List.map That


transformations : Pattern -> List (A Transformation)
transformations (Pattern data) =
    Dict.keys data.transformations
        |> List.map That


variables : Pattern -> List String
variables (Pattern data) =
    Dict.keys data.variables



-- INFO


pointInfo : A Point -> Pattern -> Maybe PointInfo
pointInfo aPoint (Pattern data) =
    case aPoint of
        That name_ ->
            Dict.get name_ data.points
                |> Maybe.map (\(Point info) -> info)

        This (Point info) ->
            Just info


type PointInfo
    = Origin OriginStuff
    | FromOnePoint FromOnePointStuff
    | BetweenRatio BetweenRatioStuff
    | BetweenLength BetweenLengthStuff
    | Intersection IntersectionStuff
    | TransformedPoint TransformedPointStuff


type alias OriginStuff =
    { x : Float
    , y : Float
    }


type alias FromOnePointStuff =
    { basePoint : A Point
    , direction : Direction
    , distance : String
    }


type alias BetweenRatioStuff =
    { basePointA : A Point
    , basePointB : A Point
    , ratio : String
    }


type alias BetweenLengthStuff =
    { basePointA : A Point
    , basePointB : A Point
    , distance : String
    , from : OneInTwo
    }


type alias IntersectionStuff =
    { objectA : A Intersectable
    , objectB : A Intersectable
    , which : Int
    }


type alias TransformedPointStuff =
    { point : A Point
    , transformation : A Transformation
    }


axisInfo : A Axis -> Pattern -> Maybe AxisInfo
axisInfo aAxis (Pattern data) =
    case aAxis of
        That name_ ->
            Dict.get name_ data.axes
                |> Maybe.map (\(Axis info) -> info)

        This (Axis info) ->
            Just info


type AxisInfo
    = ThroughOnePoint ThroughOnePointStuff
    | ThroughTwoPoints ThroughTwoPointsStuff
    | TransformedAxis TransformedAxisStuff


type alias ThroughOnePointStuff =
    { point : A Point
    , orientation : Orientation
    }


type alias ThroughTwoPointsStuff =
    { pointA : A Point
    , pointB : A Point
    }


type alias TransformedAxisStuff =
    { axis : A Axis
    , transformation : A Transformation
    }


circleInfo : A Circle -> Pattern -> Maybe CircleInfo
circleInfo aCircle (Pattern data) =
    case aCircle of
        That name_ ->
            Dict.get name_ data.circles
                |> Maybe.map (\(Circle info) -> info)

        This (Circle info) ->
            Just info


type CircleInfo
    = WithRadius WithRadiusStuff
    | ThroughThreePoints ThroughThreePointsStuff
    | TransformedCircle TransformedCircleStuff


type alias WithRadiusStuff =
    { centerPoint : A Point
    , radius : String
    }


type alias ThroughThreePointsStuff =
    { pointA : A Point
    , pointB : A Point
    , pointC : A Point
    }


type alias TransformedCircleStuff =
    { circle : A Circle
    , transformation : A Transformation
    }


curveInfo : A Curve -> Pattern -> Maybe CurveInfo
curveInfo aCurve (Pattern data) =
    case aCurve of
        That name_ ->
            Dict.get name_ data.curves
                |> Maybe.map (\(Curve info) -> info)

        This (Curve info) ->
            Just info


type CurveInfo
    = Straight StraightStuff
    | Quadratic QuadraticStuff
    | Cubic CubicStuff
    | TransformedCurve TransformedCurveStuff


type alias StraightStuff =
    { startPoint : A Point
    , endPoint : A Point
    }


type alias QuadraticStuff =
    { startPoint : A Point
    , controlPoint : A Point
    , endPoint : A Point
    }


type alias CubicStuff =
    { startPoint : A Point
    , startControlPoint : A Point
    , endControlPoint : A Point
    , endPoint : A Point
    }


type alias TransformedCurveStuff =
    { curve : A Curve
    , transformation : A Transformation
    }


detailInfo : A Detail -> Pattern -> Maybe DetailInfo
detailInfo aDetail (Pattern data) =
    case aDetail of
        That name_ ->
            Dict.get name_ data.details
                |> Maybe.map (\(Detail info) -> info)

        This (Detail info) ->
            Just info


type alias DetailInfo =
    { firstCurve : FirstCurve
    , nextCurves : List NextCurve
    , lastCurve : LastCurve
    }


type FirstCurve
    = FirstStraight FirstStraightStuff
    | FirstQuadratic FirstQuadraticStuff
    | FirstCubic FirstCubicStuff
    | FirstReferencedCurve FirstReferencedCurveStuff


type alias FirstStraightStuff =
    { startPoint : A Point
    , endPoint : A Point
    }


type alias FirstQuadraticStuff =
    { startPoint : A Point
    , controlPoint : A Point
    , endPoint : A Point
    }


type alias FirstCubicStuff =
    { startPoint : A Point
    , startControlPoint : A Point
    , endControlPoint : A Point
    , endPoint : A Point
    }


type alias FirstReferencedCurveStuff =
    { curve : A Curve
    }


type NextCurve
    = NextStraight NextStraightStuff
    | NextQuadratic NextQuadraticStuff
    | NextCubic NextCubicStuff
    | NextReferencedCurve NextReferencedCurveStuff


type alias NextStraightStuff =
    { endPoint : A Point
    }


type alias NextQuadraticStuff =
    { controlPoint : A Point
    , endPoint : A Point
    }


type alias NextCubicStuff =
    { startControlPoint : A Point
    , endControlPoint : A Point
    , endPoint : A Point
    }


type alias NextReferencedCurveStuff =
    { curve : A Curve
    }


type LastCurve
    = LastStraight
    | LastQuadratic LastQuadraticStuff
    | LastCubic LastCubicStuff
    | LastReferencedCurve LastReferencedCurveStuff


type alias LastQuadraticStuff =
    { controlPoint : A Point
    }


type alias LastCubicStuff =
    { startControlPoint : A Point
    , endControlPoint : A Point
    }


type alias LastReferencedCurveStuff =
    { curve : A Curve
    }


transformationInfo : A Transformation -> Pattern -> Maybe TransformationInfo
transformationInfo aTransformation (Pattern data) =
    case aTransformation of
        That name_ ->
            Dict.get name_ data.transformations
                |> Maybe.map (\(Transformation info) -> info)

        This (Transformation info) ->
            Just info


type TransformationInfo
    = TranslateBy TranslateByStuff
    | TranslateFromTo TranslateFromToStuff
    | RotateAround RotateAroundStuff


type alias TranslateByStuff =
    { direction : Direction
    , distance : String
    }


type alias TranslateFromToStuff =
    { startPoint : A Point
    , endPoint : A Point
    }


type alias RotateAroundStuff =
    { centerPoint : A Point
    , angle : String
    }


transformedObjects : A Transformation -> Pattern -> Maybe TransformedObjects
transformedObjects aTransformation pattern =
    Debug.todo "implement"


type alias TransformedObjects =
    { points : List (A Point)
    , axes : List (A Axis)
    , circles : List (A Circle)
    , curves : List (A Curve)
    }


variableInfo : String -> Pattern -> Maybe String
variableInfo variable (Pattern data) =
    Dict.get variable data.variables



-- SHARED TYPES


type Direction
    = Leftward
    | Rightward
    | Up
    | Down
    | DirectionAngle String


type OneInTwo
    = FirstInTwo
    | SecondInTwo


type Orientation
    = Horizontal
    | Vertical
    | OrientationAngle String



---- COMPUTE


point2d : A Point -> State Pattern (Result ComputeHelp Point2d)
point2d aPoint =
    case aPoint of
        That name_ ->
            let
                modifyWhenNeeded (Pattern data) =
                    case Dict.get name_ data.points of
                        Nothing ->
                            State.state (Err (MissingObject name_))

                        Just point ->
                            case Dict.get name_ data.point2ds of
                                Nothing ->
                                    computePoint2d point
                                        |> State.andThen addToCache

                                Just result ->
                                    State.state result

                addToCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | point2ds = Dict.insert name_ result data.point2ds
                                }
                        )
                        |> State.map (\_ -> result)
            in
            State.get
                |> State.andThen modifyWhenNeeded

        This point ->
            computePoint2d point


computePoint2d : Point -> State Pattern (Result ComputeHelp Point2d)
computePoint2d (Point info) =
    case info of
        Origin stuff ->
            StateResult.ok <|
                Point2d.fromCoordinates ( stuff.x, stuff.y )

        FromOnePoint stuff ->
            let
                toPoint2d basePoint direction distance =
                    Point2d.translateBy
                        (Vector2d.withLength distance
                            (Direction2d.fromAngle (pi * direction / 180))
                        )
                        basePoint
            in
            StateResult.ok toPoint2d
                |> StateResult.with (point2d stuff.basePoint)
                |> StateResult.with (computeDirection stuff.direction)
                |> StateResult.with (computeExpr stuff.distance)

        BetweenRatio stuff ->
            StateResult.ok Point2d.betweenRatio
                |> StateResult.with (point2d stuff.basePointA)
                |> StateResult.with (point2d stuff.basePointB)
                |> StateResult.with (computeExpr stuff.ratio)

        BetweenLength stuff ->
            let
                toPoint2d basePointA basePointB distance =
                    Point2d.betweenLength basePointA basePointB distance
                        |> Result.fromMaybe PointsCoincide
            in
            StateResult.ok toPoint2d
                |> StateResult.with (point2d stuff.basePointA)
                |> StateResult.with (point2d stuff.basePointB)
                |> StateResult.with (computeExpr stuff.distance)
                |> StateResult.join

        Intersection stuff ->
            let
                toPoint2d intersectableA intersectableB =
                    case ( intersectableA, intersectableB ) of
                        ( Axis2d axisA, Axis2d axisB ) ->
                            Axis2d.intersectionWithAxis axisA axisB
                                |> Result.fromMaybe AxesAreParallel

                        ( Axis2d axis, Circle2d circle ) ->
                            Point2d.firstCircleAxis circle axis
                                |> Result.fromMaybe AxisAndCircleDoNotIntersect

                        ( Axis2d axis, Curve2d curve ) ->
                            Debug.todo "implement"

                        ( Circle2d circle, Axis2d axis ) ->
                            Point2d.firstCircleAxis circle axis
                                |> Result.fromMaybe AxisAndCircleDoNotIntersect

                        ( Circle2d circleA, Circle2d circleB ) ->
                            case stuff.which of
                                0 ->
                                    Point2d.firstCircleCircle circleA circleB
                                        |> Result.fromMaybe CirclesDoNotIntersect

                                1 ->
                                    Point2d.secondCircleCircle circleA circleB
                                        |> Result.fromMaybe CirclesDoNotIntersect

                                _ ->
                                    Err (WhichMustBeBetween 0 1)

                        ( Circle2d circle, Curve2d curve ) ->
                            Debug.todo "implement"

                        ( Curve2d curve, Axis2d axis ) ->
                            Debug.todo "implement"

                        ( Curve2d curve, Circle2d circle ) ->
                            Debug.todo "implement"

                        ( Curve2d curveA, Curve2d curveB ) ->
                            Debug.todo "implement"
            in
            StateResult.ok toPoint2d
                |> StateResult.with (intersectable2d stuff.objectA)
                |> StateResult.with (intersectable2d stuff.objectB)
                |> StateResult.join

        TransformedPoint stuff ->
            Debug.todo "implement"


type Intersectable2d
    = Axis2d Axis2d
    | Circle2d Circle2d
    | Curve2d Curve2d


intersectable2d : A Intersectable -> State Pattern (Result ComputeHelp Intersectable2d)
intersectable2d aIntersectable =
    case aIntersectable of
        That name_ ->
            let
                modifyWhenNeeded (Pattern data) =
                    case
                        ( Dict.get name_ data.axes
                        , Dict.get name_ data.circles
                        , Dict.get name_ data.curves
                        )
                    of
                        ( Just axis, Nothing, Nothing ) ->
                            StateResult.map Axis2d <|
                                case Dict.get name_ data.axis2ds of
                                    Nothing ->
                                        computeAxis2d axis
                                            |> State.andThen addToAxisCache

                                    Just result ->
                                        State.state result

                        ( Nothing, Just circle, Nothing ) ->
                            StateResult.map Circle2d <|
                                case Dict.get name_ data.circle2ds of
                                    Nothing ->
                                        computeCircle2d circle
                                            |> State.andThen addToCircleCache

                                    Just result ->
                                        State.state result

                        ( Nothing, Nothing, Just curve ) ->
                            StateResult.map Curve2d <|
                                case Dict.get name_ data.curve2ds of
                                    Nothing ->
                                        computeCurve2d curve
                                            |> State.andThen addToCurveCache

                                    Just result ->
                                        State.state result

                        _ ->
                            State.state (Err (MissingObject name_))

                addToAxisCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | axis2ds = Dict.insert name_ result data.axis2ds
                                }
                        )
                        |> State.map (\_ -> result)

                addToCircleCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | circle2ds = Dict.insert name_ result data.circle2ds
                                }
                        )
                        |> State.map (\_ -> result)

                addToCurveCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | curve2ds = Dict.insert name_ result data.curve2ds
                                }
                        )
                        |> State.map (\_ -> result)
            in
            State.get
                |> State.andThen modifyWhenNeeded

        This (IntersectableAxis axis) ->
            computeAxis2d axis
                |> StateResult.map Axis2d

        This (IntersectableCircle circle) ->
            computeCircle2d circle
                |> StateResult.map Circle2d

        This (IntersectableCurve curve) ->
            computeCurve2d curve
                |> StateResult.map Curve2d


axis2d : A Axis -> State Pattern (Result ComputeHelp Axis2d)
axis2d aAxis =
    case aAxis of
        That name_ ->
            let
                modifyWhenNeeded (Pattern data) =
                    case Dict.get name_ data.axes of
                        Nothing ->
                            State.state (Err (MissingObject name_))

                        Just axis ->
                            case Dict.get name_ data.axis2ds of
                                Nothing ->
                                    computeAxis2d axis
                                        |> State.andThen addToCache

                                Just result ->
                                    State.state result

                addToCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | axis2ds = Dict.insert name_ result data.axis2ds
                                }
                        )
                        |> State.map (\_ -> result)
            in
            State.get
                |> State.andThen modifyWhenNeeded

        This axis ->
            computeAxis2d axis


computeAxis2d : Axis -> State Pattern (Result ComputeHelp Axis2d)
computeAxis2d (Axis info) =
    case info of
        ThroughOnePoint stuff ->
            StateResult.ok Axis2d.throughOnePoint
                |> StateResult.with (point2d stuff.point)
                |> StateResult.with (computeOrientation stuff.orientation)

        ThroughTwoPoints stuff ->
            let
                toAxis pointA pointB =
                    Axis2d.throughTwoPoints pointA pointB
                        |> Result.fromMaybe PointsCoincide
            in
            StateResult.ok toAxis
                |> StateResult.with (point2d stuff.pointA)
                |> StateResult.with (point2d stuff.pointB)
                |> StateResult.join

        TransformedAxis stuff ->
            Debug.todo "implement"


circle2d : A Circle -> State Pattern (Result ComputeHelp Circle2d)
circle2d aCircle =
    case aCircle of
        That name_ ->
            let
                modifyWhenNeeded (Pattern data) =
                    case Dict.get name_ data.circles of
                        Nothing ->
                            State.state (Err (MissingObject name_))

                        Just circle ->
                            case Dict.get name_ data.circle2ds of
                                Nothing ->
                                    computeCircle2d circle
                                        |> State.andThen addToCache

                                Just result ->
                                    State.state result

                addToCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | circle2ds = Dict.insert name_ result data.circle2ds
                                }
                        )
                        |> State.map (\_ -> result)
            in
            State.get
                |> State.andThen modifyWhenNeeded

        This circle ->
            computeCircle2d circle


computeCircle2d : Circle -> State Pattern (Result ComputeHelp Circle2d)
computeCircle2d (Circle info) =
    case info of
        WithRadius stuff ->
            StateResult.ok Circle2d.withRadius
                |> StateResult.with (computeExpr stuff.radius)
                |> StateResult.with (point2d stuff.centerPoint)

        ThroughThreePoints stuff ->
            let
                toCircle2d pointA pointB pointC =
                    Circle2d.throughPoints pointA pointB pointC
                        |> Result.fromMaybe PointsAreColinear
            in
            StateResult.ok toCircle2d
                |> StateResult.with (point2d stuff.pointA)
                |> StateResult.with (point2d stuff.pointB)
                |> StateResult.with (point2d stuff.pointC)
                |> StateResult.join

        TransformedCircle stuff ->
            Debug.todo "implement"


type Curve2d
    = LineSegment2d LineSegment2d
    | QuadraticSpline2d QuadraticSpline2d
    | CubicSpline2d CubicSpline2d


curve2d : A Curve -> State Pattern (Result ComputeHelp Curve2d)
curve2d aCurve =
    case aCurve of
        That name_ ->
            let
                modifyWhenNeeded (Pattern data) =
                    case Dict.get name_ data.curves of
                        Nothing ->
                            State.state (Err (MissingObject name_))

                        Just curve ->
                            case Dict.get name_ data.curve2ds of
                                Nothing ->
                                    computeCurve2d curve
                                        |> State.andThen addToCache

                                Just result ->
                                    State.state result

                addToCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | curve2ds = Dict.insert name_ result data.curve2ds
                                }
                        )
                        |> State.map (\_ -> result)
            in
            State.get
                |> State.andThen modifyWhenNeeded

        This curve ->
            computeCurve2d curve


computeCurve2d : Curve -> State Pattern (Result ComputeHelp Curve2d)
computeCurve2d (Curve info) =
    case info of
        Straight stuff ->
            let
                toCurve2d startPoint endPoint =
                    LineSegment2d (LineSegment2d.from startPoint endPoint)
            in
            StateResult.ok toCurve2d
                |> StateResult.with (point2d stuff.startPoint)
                |> StateResult.with (point2d stuff.endPoint)

        Quadratic stuff ->
            let
                toCurve2d startPoint controlPoint endPoint =
                    QuadraticSpline2d <|
                        QuadraticSpline2d.with
                            { startPoint = startPoint
                            , controlPoint = controlPoint
                            , endPoint = endPoint
                            }
            in
            StateResult.ok toCurve2d
                |> StateResult.with (point2d stuff.startPoint)
                |> StateResult.with (point2d stuff.controlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        Cubic stuff ->
            let
                toCurve2d startPoint startControlPoint endControlPoint endPoint =
                    CubicSpline2d <|
                        CubicSpline2d.with
                            { startPoint = startPoint
                            , startControlPoint = startControlPoint
                            , endControlPoint = endControlPoint
                            , endPoint = endPoint
                            }
            in
            StateResult.ok toCurve2d
                |> StateResult.with (point2d stuff.startPoint)
                |> StateResult.with (point2d stuff.startControlPoint)
                |> StateResult.with (point2d stuff.endControlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        TransformedCurve stuff ->
            Debug.todo "implement"


type alias Detail2d =
    { firstPoint : Point2d
    , nextCurves : List NextCurve2d
    , lastCurve : LastCurve2d
    }


type NextCurve2d
    = NextLineSegment2d
        { endPoint : Point2d
        }
    | NextQuadraticSpline2d
        { controlPoint : Point2d
        , endPoint : Point2d
        }
    | NextCubicSpline2d
        { startControlPoint : Point2d
        , endControlPoint : Point2d
        , endPoint : Point2d
        }


type LastCurve2d
    = LastLineSegment2d
    | LastQuadraticSpline2d
        { controlPoint : Point2d
        }
    | LastCubicSpline2d
        { startControlPoint : Point2d
        , endControlPoint : Point2d
        }


detail2d : A Detail -> State Pattern (Result ComputeHelp Detail2d)
detail2d aDetail =
    case aDetail of
        That name_ ->
            let
                modifyWhenNeeded (Pattern data) =
                    case Dict.get name_ data.details of
                        Nothing ->
                            State.state (Err (MissingObject name_))

                        Just detail_ ->
                            case Dict.get name_ data.detail2ds of
                                Nothing ->
                                    computeDetail2d detail_
                                        |> State.andThen addToCache

                                Just result ->
                                    State.state result

                addToCache result =
                    State.modify
                        (\(Pattern data) ->
                            Pattern
                                { data
                                    | detail2ds = Dict.insert name_ result data.detail2ds
                                }
                        )
                        |> State.map (\_ -> result)
            in
            State.get
                |> State.andThen modifyWhenNeeded

        This curve ->
            computeDetail2d curve


computeDetail2d : Detail -> State Pattern (Result ComputeHelp Detail2d)
computeDetail2d (Detail info) =
    StateResult.ok Detail2d
        |> StateResult.with
            (case info.firstCurve of
                FirstStraight stuff ->
                    point2d stuff.startPoint

                FirstQuadratic stuff ->
                    point2d stuff.startPoint

                FirstCubic stuff ->
                    point2d stuff.startPoint

                FirstReferencedCurve stuff ->
                    Debug.todo "implement"
            )
        |> StateResult.with
            (StateResult.ok (::)
                |> StateResult.with (secondCurve2d info.firstCurve)
                |> StateResult.with (StateResult.traverse nextCurve2d info.nextCurves)
            )
        |> StateResult.with
            (case info.lastCurve of
                LastStraight ->
                    StateResult.ok LastLineSegment2d

                LastQuadratic stuff ->
                    let
                        toLastCurve controlPoint =
                            LastQuadraticSpline2d
                                { controlPoint = controlPoint }
                    in
                    StateResult.ok toLastCurve
                        |> StateResult.with (point2d stuff.controlPoint)

                LastCubic stuff ->
                    let
                        toLastCurve startControlPoint endControlPoint =
                            LastCubicSpline2d
                                { startControlPoint = startControlPoint
                                , endControlPoint = endControlPoint
                                }
                    in
                    StateResult.ok toLastCurve
                        |> StateResult.with (point2d stuff.startControlPoint)
                        |> StateResult.with (point2d stuff.endControlPoint)

                LastReferencedCurve stuff ->
                    Debug.todo "implement"
            )


secondCurve2d : FirstCurve -> State Pattern (Result ComputeHelp NextCurve2d)
secondCurve2d firstCurve =
    case firstCurve of
        FirstStraight stuff ->
            let
                toCurve endPoint =
                    NextLineSegment2d
                        { endPoint = endPoint }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.endPoint)

        FirstQuadratic stuff ->
            let
                toCurve controlPoint endPoint =
                    NextQuadraticSpline2d
                        { controlPoint = controlPoint
                        , endPoint = endPoint
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.controlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        FirstCubic stuff ->
            let
                toCurve startControlPoint endControlPoint endPoint =
                    NextCubicSpline2d
                        { startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        , endPoint = endPoint
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.startControlPoint)
                |> StateResult.with (point2d stuff.endControlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        FirstReferencedCurve stuff ->
            Debug.todo "implement"


nextCurve2d : NextCurve -> State Pattern (Result ComputeHelp NextCurve2d)
nextCurve2d nextCurve =
    case nextCurve of
        NextStraight stuff ->
            let
                toCurve endPoint =
                    NextLineSegment2d
                        { endPoint = endPoint }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.endPoint)

        NextQuadratic stuff ->
            let
                toCurve controlPoint endPoint =
                    NextQuadraticSpline2d
                        { controlPoint = controlPoint
                        , endPoint = endPoint
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.controlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        NextCubic stuff ->
            let
                toCurve startControlPoint endControlPoint endPoint =
                    NextCubicSpline2d
                        { startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        , endPoint = endPoint
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.startControlPoint)
                |> StateResult.with (point2d stuff.endControlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        NextReferencedCurve stuff ->
            Debug.todo "implement"


float : String -> State Pattern (Result ComputeHelp Float)
float variable =
    let
        modifyWhenNeeded (Pattern data) =
            case Dict.get variable data.variables of
                Nothing ->
                    State.state (Err (MissingObject variable))

                Just rawExpr ->
                    case Dict.get variable data.variablesCache of
                        Nothing ->
                            computeExpr rawExpr
                                |> State.andThen addToCache

                        Just result ->
                            State.state result

        addToCache result =
            State.modify
                (\(Pattern data) ->
                    Pattern
                        { data
                            | variablesCache =
                                Dict.insert variable result data.variablesCache
                        }
                )
                |> State.map (\_ -> result)
    in
    State.get
        |> State.andThen modifyWhenNeeded


computeDirection : Direction -> State Pattern (Result ComputeHelp Float)
computeDirection direction =
    case direction of
        Leftward ->
            StateResult.ok 180

        Rightward ->
            StateResult.ok 0

        Up ->
            StateResult.ok 270

        Down ->
            StateResult.ok 90

        DirectionAngle angle ->
            computeExpr angle


computeOrientation : Orientation -> State Pattern (Result ComputeHelp Float)
computeOrientation orientation =
    case orientation of
        Horizontal ->
            StateResult.ok 0

        Vertical ->
            StateResult.ok 90

        OrientationAngle angle ->
            computeExpr angle


computeExpr : String -> State Pattern (Result ComputeHelp Float)
computeExpr rawExpr =
    case Expr.parse reservedWords rawExpr of
        Err syntaxHelp ->
            StateResult.err (ExprHelp (SyntaxHelp syntaxHelp))

        Ok expr ->
            evaluateExpr expr


reservedWords : List String
reservedWords =
    [ "distance"
    , "angleOfLine"
    ]


evaluateExpr : Expr -> State Pattern (Result ComputeHelp Float)
evaluateExpr expr =
    case expr of
        Number num ->
            StateResult.ok num

        Variable variable ->
            evaluateVariable variable

        Function function args ->
            case function of
                "distance" ->
                    case args of
                        nameA :: nameB :: [] ->
                            StateResult.ok Point2d.distanceFrom
                                |> StateResult.with (point2d (That nameA))
                                |> StateResult.with (point2d (That nameB))

                        _ ->
                            StateResult.err <|
                                ExprHelp <|
                                    WrongArguments
                                        { function = function
                                        , args = args
                                        }

                "angleOfLine" ->
                    case args of
                        nameA :: nameB :: [] ->
                            let
                                toAngle point2dA point2dB =
                                    Direction2d.from point2dA point2dB
                                        |> Maybe.map (Direction2d.toAngle >> toDegree)

                                toDegree radian =
                                    180 * radian / pi
                            in
                            StateResult.ok toAngle
                                |> StateResult.with (point2d (That nameA))
                                |> StateResult.with (point2d (That nameB))
                                |> StateResult.andThen
                                    (\maybeAngle ->
                                        case maybeAngle of
                                            Nothing ->
                                                StateResult.err <|
                                                    ExprHelp <|
                                                        CannotComputeFunction function

                                            Just angle ->
                                                StateResult.ok angle
                                    )

                        _ ->
                            StateResult.err <|
                                ExprHelp <|
                                    WrongArguments
                                        { function = function
                                        , args = args
                                        }

                _ ->
                    StateResult.err (ExprHelp (UnknownFunction function))

        Sum exprA exprB ->
            StateResult.ok (+)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        Difference exprA exprB ->
            StateResult.ok (-)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        Product exprA exprB ->
            StateResult.ok (*)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        Quotient exprA exprB ->
            StateResult.ok (/)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        Max exprA exprB ->
            StateResult.ok max
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        IfThenElse boolExpr exprA exprB ->
            StateResult.ok
                (\bool a b ->
                    if bool then
                        a

                    else
                        b
                )
                |> StateResult.with (evaluateBoolExpr boolExpr)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)


evaluateVariable : String -> State Pattern (Result ComputeHelp Float)
evaluateVariable variable =
    let
        modifyWhenNeeded (Pattern data) =
            case Dict.get variable data.variablesCache of
                Nothing ->
                    calculateVariable data
                        |> State.andThen addVariable

                Just result ->
                    State.state result

        calculateVariable data =
            case Dict.get variable data.variables of
                Nothing ->
                    StateResult.err (MissingVariable variable)

                Just rawExpr ->
                    case Expr.parse reservedWords rawExpr of
                        Err syntaxHelp ->
                            StateResult.err (ExprHelp (SyntaxHelp syntaxHelp))

                        Ok expr ->
                            evaluateExpr expr

        addVariable result =
            State.modify
                (\(Pattern data) ->
                    Pattern
                        { data
                            | variablesCache =
                                Dict.insert variable result data.variablesCache
                        }
                )
                |> State.map (\_ -> result)
    in
    State.get
        |> State.andThen modifyWhenNeeded


evaluateBoolExpr : BoolExpr -> State Pattern (Result ComputeHelp Bool)
evaluateBoolExpr boolExpr =
    case boolExpr of
        ExprTrue ->
            StateResult.ok True

        ExprFalse ->
            StateResult.ok False

        Not nestedBoolExpr ->
            StateResult.ok not
                |> StateResult.with (evaluateBoolExpr nestedBoolExpr)

        And boolExprA boolExprB ->
            StateResult.ok (&&)
                |> StateResult.with (evaluateBoolExpr boolExprA)
                |> StateResult.with (evaluateBoolExpr boolExprB)

        Or boolExprA boolExprB ->
            StateResult.ok (||)
                |> StateResult.with (evaluateBoolExpr boolExprA)
                |> StateResult.with (evaluateBoolExpr boolExprB)

        Equal exprA exprB ->
            StateResult.ok (==)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        GreaterThan exprA exprB ->
            StateResult.ok (>)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)

        StrictlyGreaterThan exprA exprB ->
            StateResult.ok (>=)
                |> StateResult.with (evaluateExpr exprA)
                |> StateResult.with (evaluateExpr exprB)


type ComputeHelp
    = MissingObject String
    | MissingVariable String
    | PointsCoincide
    | PointsAreColinear
    | AxesAreParallel
    | CirclesDoNotIntersect
    | AxisAndCircleDoNotIntersect
    | WhichMustBeBetween Int Int
    | ExprHelp ExprHelp



---- CONSTRUCT


origin : Float -> Float -> Point
origin x y =
    Point <|
        Origin
            { x = x
            , y = y
            }


fromOnePoint :
    A Point
    -> Direction
    -> String
    -> Pattern
    -> Result FromOnePointHelp Point
fromOnePoint aBasePoint direction distance pattern =
    Ok FromOnePointHelp
        |> collectMaybe (checkDirection direction)
        |> collectMaybe (checkExpr distance)
        |> Result.map
            (always <|
                Point <|
                    FromOnePoint
                        { basePoint = aBasePoint
                        , direction = direction
                        , distance = distance
                        }
            )


type alias FromOnePointHelp =
    { parseDirection : Maybe ExprHelp
    , parseDistance : Maybe ExprHelp
    }


betweenRatio : A Point -> A Point -> String -> Pattern -> Result BetweenRatioHelp Point
betweenRatio aBasePointA aBasePointB ratio pattern =
    Ok BetweenRatioHelp
        |> collectMaybe (checkExpr ratio)
        |> collectBool (checkObjectsCoincidence aBasePointA aBasePointB)
        |> Result.map
            (always <|
                Point <|
                    BetweenRatio
                        { basePointA = aBasePointA
                        , basePointB = aBasePointB
                        , ratio = ratio
                        }
            )


type alias BetweenRatioHelp =
    { parseRatio : Maybe ExprHelp
    , basePointsCoincide : Bool
    }


betweenLength :
    A Point
    -> A Point
    -> String
    -> OneInTwo
    -> Pattern
    -> Result BetweenLengthHelp Point
betweenLength aBasePointA aBasePointB distance from pattern =
    Ok BetweenLengthHelp
        |> collectMaybe (checkExpr distance)
        |> collectBool (checkObjectsCoincidence aBasePointA aBasePointB)
        |> Result.map
            (always <|
                Point <|
                    BetweenLength
                        { basePointA = aBasePointA
                        , basePointB = aBasePointB
                        , distance = distance
                        , from = from
                        }
            )


type alias BetweenLengthHelp =
    { parseDistance : Maybe ExprHelp
    , basePointsCoincide : Bool
    }


intersection :
    A Intersectable
    -> A Intersectable
    -> Int
    -> Pattern
    -> Result IntersectionHelp Point
intersection aObjectA aObjectB which pattern =
    Ok IntersectionHelp
        |> collectBool (checkObjectsCoincidence aObjectA aObjectB)
        |> collectBool (checkWhichInBound aObjectA aObjectB pattern which)
        |> Result.map
            (always <|
                Point <|
                    Intersection
                        { objectA = aObjectA
                        , objectB = aObjectB
                        , which = which
                        }
            )


checkWhichInBound : A Intersectable -> A Intersectable -> Pattern -> Int -> Bool
checkWhichInBound aIntersectableA aIntersectableB pattern which =
    let
        maybeSize =
            Maybe.map2 whichSize
                (tagFromIntersectable pattern aIntersectableA)
                (tagFromIntersectable pattern aIntersectableB)
    in
    case maybeSize of
        Nothing ->
            False

        Just size ->
            0 >= which && which < size


tagFromIntersectable : Pattern -> A Intersectable -> Maybe IntersectableTag
tagFromIntersectable (Pattern data) aIntersectable =
    case aIntersectable of
        That name_ ->
            if Dict.member name_ data.axes then
                Just IntersectableAxisTag

            else if Dict.member name_ data.circles then
                Just IntersectableCircleTag

            else if Dict.member name_ data.curves then
                Just IntersectableCurveTag

            else
                Nothing

        This (IntersectableAxis _) ->
            Just IntersectableAxisTag

        This (IntersectableCircle _) ->
            Just IntersectableCircleTag

        This (IntersectableCurve _) ->
            Just IntersectableCurveTag


whichSize : IntersectableTag -> IntersectableTag -> Int
whichSize intersectableTagA intersectableTagB =
    case ( intersectableTagA, intersectableTagB ) of
        ( IntersectableAxisTag, IntersectableAxisTag ) ->
            1

        ( IntersectableCircleTag, IntersectableCurveTag ) ->
            2

        ( IntersectableAxisTag, IntersectableCircleTag ) ->
            2

        ( IntersectableCircleTag, IntersectableAxisTag ) ->
            2

        _ ->
            Debug.todo "implement"


type alias IntersectionHelp =
    { objectsCoincide : Bool
    , whichOutOfBound : Bool
    }


transformedPoint : A Point -> A Transformation -> Pattern -> Point
transformedPoint aPoint aTransformation pattern =
    Debug.todo "implement"


throughOnePoint : A Point -> Orientation -> Pattern -> Result ThroughOnePointHelp Axis
throughOnePoint aPoint orientation pattern =
    Ok ThroughOnePointHelp
        |> collectMaybe (checkOrientation orientation)
        |> Result.map
            (always <|
                Axis <|
                    ThroughOnePoint
                        { point = aPoint
                        , orientation = orientation
                        }
            )


type alias ThroughOnePointHelp =
    { parseAngle : Maybe ExprHelp
    }


throughTwoPoints : A Point -> A Point -> Pattern -> Result ThroughTwoPointsHelp Axis
throughTwoPoints aPointA aPointB pattern =
    Ok ThroughTwoPointsHelp
        |> collectBool (checkObjectsCoincidence aPointA aPointB)
        |> Result.map
            (always <|
                Axis <|
                    ThroughTwoPoints
                        { pointA = aPointA
                        , pointB = aPointB
                        }
            )


type alias ThroughTwoPointsHelp =
    { basePointsCoincide : Bool
    }


transformedAxis : A Axis -> A Transformation -> Pattern -> Axis
transformedAxis aAxis aTransformation pattern =
    Debug.todo "implement"


withRadius : String -> A Point -> Pattern -> Result WithRadiusHelp Circle
withRadius radius aCenterPoint pattern =
    Ok WithRadiusHelp
        |> collectMaybe (checkExpr radius)
        |> Result.map
            (always <|
                Circle <|
                    WithRadius
                        { centerPoint = aCenterPoint
                        , radius = radius
                        }
            )


type alias WithRadiusHelp =
    { parseRadius : Maybe ExprHelp
    }


throughThreePoints :
    A Point
    -> A Point
    -> A Point
    -> Pattern
    -> Result ThroughThreePointsHelp Circle
throughThreePoints aPointA aPointB aPointC pattern =
    Ok ThroughThreePointsHelp
        |> collectBool
            (checkObjectsCoincidence aPointA aPointB
                || checkObjectsCoincidence aPointB aPointC
                || checkObjectsCoincidence aPointC aPointA
            )
        |> Result.map
            (always <|
                Circle <|
                    ThroughThreePoints
                        { pointA = aPointA
                        , pointB = aPointB
                        , pointC = aPointC
                        }
            )


type alias ThroughThreePointsHelp =
    { pointsCoincide : Bool
    }


transformedCircle : A Circle -> A Transformation -> Pattern -> Circle
transformedCircle aCircle aTransformation pattern =
    Debug.todo "implement"


straight : A Point -> A Point -> Pattern -> Result StraightHelp Curve
straight aStartPoint aEndPoint pattern =
    Ok StraightHelp
        |> collectBool (checkObjectsCoincidence aStartPoint aEndPoint)
        |> Result.map
            (always <|
                Curve <|
                    Straight
                        { startPoint = aStartPoint
                        , endPoint = aEndPoint
                        }
            )


type alias StraightHelp =
    { pointsCoincide : Bool
    }


quadratic : A Point -> A Point -> A Point -> Pattern -> Result QuadraticHelp Curve
quadratic aStartPoint aControlPoint aEndPoint pattern =
    Ok QuadraticHelp
        |> collectBool (checkObjectsCoincidence aStartPoint aEndPoint)
        |> Result.map
            (always <|
                Curve <|
                    Quadratic
                        { startPoint = aStartPoint
                        , controlPoint = aControlPoint
                        , endPoint = aEndPoint
                        }
            )


type alias QuadraticHelp =
    { pointsCoincide : Bool
    }


cubic : A Point -> A Point -> A Point -> A Point -> Pattern -> Result CubicHelp Curve
cubic aStartPoint aStartControlPoint aEndControlPoint aEndPoint pattern =
    Ok CubicHelp
        |> collectBool (checkObjectsCoincidence aStartPoint aEndPoint)
        |> Result.map
            (always <|
                Curve <|
                    Cubic
                        { startPoint = aStartPoint
                        , startControlPoint = aStartControlPoint
                        , endControlPoint = aEndControlPoint
                        , endPoint = aEndPoint
                        }
            )


type alias CubicHelp =
    { pointsCoincide : Bool
    }


transformedCurve : A Curve -> A Transformation -> Pattern -> Curve
transformedCurve aCurve aTransformation pattern =
    Debug.todo "implement"


detail : FirstCurve -> List NextCurve -> LastCurve -> Pattern -> Result DetailHelp Detail
detail firstCurve nextCurves lastCurve pattern =
    Ok <|
        Detail
            { firstCurve = firstCurve
            , nextCurves = nextCurves
            , lastCurve = lastCurve
            }


type alias DetailHelp =
    {}



-- HELP HELPER


collectMaybe :
    Maybe a
    -> Result (Maybe a -> rest) (Maybe a -> rest)
    -> Result rest rest
collectMaybe maybeA resultFunc =
    case resultFunc of
        Err toRest ->
            Err (toRest maybeA)

        Ok toRest ->
            case maybeA of
                Nothing ->
                    Ok (toRest Nothing)

                Just _ ->
                    Err (toRest maybeA)


collectBool :
    Bool
    -> Result (Bool -> rest) (Bool -> rest)
    -> Result rest rest
collectBool bool resultFunc =
    case resultFunc of
        Err toRest ->
            Err (toRest bool)

        Ok toRest ->
            case bool of
                False ->
                    Ok (toRest False)

                True ->
                    Err (toRest True)



---- CHECKS


type ExprHelp
    = SyntaxHelp (List DeadEnd)
    | UnknownFunction String
    | WrongArguments
        { function : String
        , args : List String
        }
    | CannotComputeFunction String


checkExpr : String -> Maybe ExprHelp
checkExpr rawExpr =
    case Expr.parse reservedWords rawExpr of
        Ok _ ->
            Nothing

        Err syntaxHelp ->
            Just (SyntaxHelp syntaxHelp)


checkDirection : Direction -> Maybe ExprHelp
checkDirection direction =
    case direction of
        Leftward ->
            Nothing

        Rightward ->
            Nothing

        Up ->
            Nothing

        Down ->
            Nothing

        DirectionAngle rawExpr ->
            checkExpr rawExpr


checkOrientation : Orientation -> Maybe ExprHelp
checkOrientation orientation =
    case orientation of
        Horizontal ->
            Nothing

        Vertical ->
            Nothing

        OrientationAngle rawAngle ->
            checkExpr rawAngle


checkObjectsCoincidence : A object -> A object -> Bool
checkObjectsCoincidence aObjectA aObjectB =
    case ( aObjectA, aObjectB ) of
        ( That nameA, That nameB ) ->
            nameA == nameB

        _ ->
            False



---- ENCODE


encode : Pattern -> Value
encode (Pattern data) =
    Encode.object
        [ ( "points", Encode.dict identity encodePoint data.points )
        , ( "axes", Encode.dict identity encodeAxis data.axes )
        , ( "circles", Encode.dict identity encodeCircle data.circles )
        , ( "curves", Encode.dict identity encodeCurve data.curves )
        , ( "details", Encode.dict identity encodeDetail data.details )
        , ( "transformations"
          , Encode.dict identity encodeTransformation data.transformations
          )
        , ( "variables", Encode.dict identity Encode.string data.variables )
        ]


encodePoint : Point -> Value
encodePoint (Point info) =
    case info of
        Origin stuff ->
            withType "origin"
                [ ( "x", Encode.float stuff.x )
                , ( "y", Encode.float stuff.y )
                ]

        FromOnePoint stuff ->
            withType "fromOnePoint"
                [ ( "basePoint", encodeAPoint stuff.basePoint )
                , ( "direction", encodeDirection stuff.direction )
                , ( "distance", Encode.string stuff.distance )
                ]

        BetweenRatio stuff ->
            withType "betweenRatio"
                [ ( "basePointA", encodeAPoint stuff.basePointA )
                , ( "basePointB", encodeAPoint stuff.basePointB )
                , ( "ratio", Encode.string stuff.ratio )
                ]

        BetweenLength stuff ->
            withType "betweenLength"
                [ ( "basePointA", encodeAPoint stuff.basePointA )
                , ( "basePointB", encodeAPoint stuff.basePointB )
                , ( "distance", Encode.string stuff.distance )
                , ( "from", encodeOneInTwo stuff.from )
                ]

        Intersection stuff ->
            withType "intersection"
                [ ( "objectA", encodeAIntersectable stuff.objectA )
                , ( "objectB", encodeAIntersectable stuff.objectB )
                , ( "which", Encode.int stuff.which )
                ]

        TransformedPoint stuff ->
            withType "transformedPoint"
                [ ( "point", encodeAPoint stuff.point )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeAxis : Axis -> Value
encodeAxis (Axis info) =
    case info of
        ThroughOnePoint stuff ->
            withType "throughOnePoint"
                [ ( "point", encodeAPoint stuff.point )
                , ( "orientation", encodeOrientation stuff.orientation )
                ]

        ThroughTwoPoints stuff ->
            withType "throughTwoPoints"
                [ ( "pointA", encodeAPoint stuff.pointA )
                , ( "pointB", encodeAPoint stuff.pointB )
                ]

        TransformedAxis stuff ->
            withType "transformedAxis"
                [ ( "axis", encodeAAxis stuff.axis )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeCircle : Circle -> Value
encodeCircle (Circle info) =
    case info of
        WithRadius stuff ->
            withType "withRadius"
                [ ( "centerPoint", encodeAPoint stuff.centerPoint )
                , ( "radius", Encode.string stuff.radius )
                ]

        ThroughThreePoints stuff ->
            withType "throughThreePoints"
                [ ( "pointA", encodeAPoint stuff.pointA )
                , ( "pointB", encodeAPoint stuff.pointB )
                , ( "pointC", encodeAPoint stuff.pointC )
                ]

        TransformedCircle stuff ->
            withType "transformedCircle"
                [ ( "circle", encodeACircle stuff.circle )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeCurve : Curve -> Value
encodeCurve (Curve info) =
    case info of
        Straight stuff ->
            withType "straight"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        Quadratic stuff ->
            withType "quadratic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "controlPoint", encodeAPoint stuff.controlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        Cubic stuff ->
            withType "cubic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        TransformedCurve stuff ->
            withType "transformedCurve"
                [ ( "curve", encodeACurve stuff.curve )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeDetail : Detail -> Value
encodeDetail (Detail stuff) =
    Encode.object
        [ ( "firstCurve", encodeFirstCurve stuff.firstCurve )
        , ( "nextCurves", Encode.list encodeNextCurve stuff.nextCurves )
        , ( "lastCurve", encodeLastCurve stuff.lastCurve )
        ]


encodeFirstCurve : FirstCurve -> Value
encodeFirstCurve firstCurve =
    case firstCurve of
        FirstStraight stuff ->
            withType "firstStraight"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        FirstQuadratic stuff ->
            withType "firstQuadratic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "controlPoint", encodeAPoint stuff.controlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        FirstCubic stuff ->
            withType "firstCubic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        FirstReferencedCurve stuff ->
            withType "firstReferencedCurve"
                [ ( "curve", encodeACurve stuff.curve ) ]


encodeNextCurve : NextCurve -> Value
encodeNextCurve nextCurve =
    case nextCurve of
        NextStraight stuff ->
            withType "nextStraight"
                [ ( "endPoint", encodeAPoint stuff.endPoint ) ]

        NextQuadratic stuff ->
            withType "nextQuadratic"
                [ ( "controlPoint", encodeAPoint stuff.controlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        NextCubic stuff ->
            withType "nextCubic"
                [ ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        NextReferencedCurve stuff ->
            withType "nextReferencedCurve"
                [ ( "curve", encodeACurve stuff.curve ) ]


encodeLastCurve : LastCurve -> Value
encodeLastCurve lastCurve =
    case lastCurve of
        LastStraight ->
            withType "lastStraight" []

        LastQuadratic stuff ->
            withType "lastQuadratic"
                [ ( "controlPoint", encodeAPoint stuff.controlPoint ) ]

        LastCubic stuff ->
            withType "lastCubic"
                [ ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                ]

        LastReferencedCurve stuff ->
            withType "firstReferencedCurve"
                [ ( "curve", encodeACurve stuff.curve ) ]


encodeTransformation : Transformation -> Value
encodeTransformation (Transformation info) =
    case info of
        TranslateBy stuff ->
            withType "translateBy"
                [ ( "direction", encodeDirection stuff.direction )
                , ( "distance", Encode.string stuff.distance )
                ]

        TranslateFromTo stuff ->
            withType "translateFromTo"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        RotateAround stuff ->
            withType "rotateAround"
                [ ( "centerPoint", encodeAPoint stuff.centerPoint )
                , ( "angle", Encode.string stuff.angle )
                ]


encodeIntersectable : Intersectable -> Value
encodeIntersectable intersectable =
    case intersectable of
        IntersectableAxis axis ->
            withType "intersectableAxis"
                [ ( "axis", encodeAxis axis ) ]

        IntersectableCircle circle ->
            withType "intersectableCircle"
                [ ( "circle", encodeCircle circle ) ]

        IntersectableCurve curve ->
            withType "intersectableCurve"
                [ ( "curve", encodeCurve curve ) ]



-- A


encodeAPoint : A Point -> Value
encodeAPoint =
    encodeAObject "point" encodePoint


encodeAAxis : A Axis -> Value
encodeAAxis =
    encodeAObject "axis" encodeAxis


encodeACircle : A Circle -> Value
encodeACircle =
    encodeAObject "circle" encodeCircle


encodeACurve : A Curve -> Value
encodeACurve =
    encodeAObject "curve" encodeCurve


encodeADetail : A Detail -> Value
encodeADetail =
    encodeAObject "detail" encodeDetail


encodeATransformation : A Transformation -> Value
encodeATransformation =
    encodeAObject "transformation" encodeTransformation


encodeAIntersectable : A Intersectable -> Value
encodeAIntersectable =
    encodeAObject "intersectable" encodeIntersectable


encodeAObject : String -> (object -> Value) -> A object -> Value
encodeAObject objectType encodeObject aObject =
    case aObject of
        That name_ ->
            withType "that"
                [ ( "name", Encode.string name_ ) ]

        This object ->
            withType "this"
                [ ( objectType, encodeObject object ) ]



-- SHARED


encodeDirection : Direction -> Value
encodeDirection direction =
    case direction of
        Leftward ->
            withType "leftward" []

        Rightward ->
            withType "rightward" []

        Up ->
            withType "up" []

        Down ->
            withType "down" []

        DirectionAngle angle ->
            withType "directionAngle"
                [ ( "angle", Encode.string angle ) ]


encodeOrientation : Orientation -> Value
encodeOrientation orientation =
    case orientation of
        Horizontal ->
            withType "horizontal" []

        Vertical ->
            withType "vertical" []

        OrientationAngle angle ->
            withType "orientationAngle"
                [ ( "angle", Encode.string angle ) ]


encodeOneInTwo : OneInTwo -> Value
encodeOneInTwo oneInTwo =
    case oneInTwo of
        FirstInTwo ->
            withType "firstInTwo" []

        SecondInTwo ->
            withType "secondInTwo" []



-- HELPER


withType : String -> List ( String, Value ) -> Value
withType type_ fields =
    Encode.object (( "type", Encode.string type_ ) :: fields)



---- DECODER


decoder : Decoder Pattern
decoder =
    Decode.succeed PatternData
        |> Decode.required "points" (Decode.dict pointDecoder)
        |> Decode.required "axes" (Decode.dict axisDecoder)
        |> Decode.required "circles" (Decode.dict circleDecoder)
        |> Decode.required "curves" (Decode.dict curveDecoder)
        |> Decode.required "details" (Decode.dict detailDecoder)
        |> Decode.required "transformations" (Decode.dict transformationDecoder)
        |> Decode.required "variables" (Decode.dict Decode.string)
        |> Decode.hardcoded Dict.empty
        |> Decode.hardcoded Dict.empty
        |> Decode.hardcoded Dict.empty
        |> Decode.hardcoded Dict.empty
        |> Decode.hardcoded Dict.empty
        |> Decode.hardcoded Dict.empty
        |> Decode.map Pattern


pointDecoder : Decoder Point
pointDecoder =
    Decode.oneOf
        [ Decode.succeed OriginStuff
            |> Decode.required "x" Decode.float
            |> Decode.required "y" Decode.float
            |> Decode.map Origin
            |> ensureType "origin"
        , Decode.succeed FromOnePointStuff
            |> Decode.required "basePoint" aPointDecoder
            |> Decode.required "direction" directionDecoder
            |> Decode.required "distance" Decode.string
            |> Decode.map FromOnePoint
            |> ensureType "fromOnePoint"
        , Decode.succeed BetweenRatioStuff
            |> Decode.required "basePointA" aPointDecoder
            |> Decode.required "basePointB" aPointDecoder
            |> Decode.required "ratio" Decode.string
            |> Decode.map BetweenRatio
            |> ensureType "betweenRatio"
        , Decode.succeed BetweenLengthStuff
            |> Decode.required "basePointA" aPointDecoder
            |> Decode.required "basePointB" aPointDecoder
            |> Decode.required "distance" Decode.string
            |> Decode.required "from" oneInTwoDecoder
            |> Decode.map BetweenLength
            |> ensureType "betweenLength"
        , Decode.succeed IntersectionStuff
            |> Decode.required "objectA" aIntersectableDecoder
            |> Decode.required "objectB" aIntersectableDecoder
            |> Decode.required "which" Decode.int
            |> Decode.map Intersection
            |> ensureType "intersection"
        ]
        |> Decode.map Point


axisDecoder : Decoder Axis
axisDecoder =
    Decode.oneOf
        [ Decode.succeed ThroughOnePointStuff
            |> Decode.required "point" aPointDecoder
            |> Decode.required "orientation" orientationDecoder
            |> Decode.map ThroughOnePoint
            |> ensureType "throughOnePoint"
        , Decode.succeed ThroughTwoPointsStuff
            |> Decode.required "pointA" aPointDecoder
            |> Decode.required "pointB" aPointDecoder
            |> Decode.map ThroughTwoPoints
            |> ensureType "throughTwoPoints"
        , Decode.succeed TransformedAxisStuff
            |> Decode.required "axis" aAxisDecoder
            |> Decode.required "aTransformation" aTransformationDecoder
            |> Decode.map TransformedAxis
            |> ensureType "transformedAxis"
        ]
        |> Decode.map Axis


circleDecoder : Decoder Circle
circleDecoder =
    Decode.oneOf
        [ Decode.succeed WithRadiusStuff
            |> Decode.required "centerPoint" aPointDecoder
            |> Decode.required "radius" Decode.string
            |> Decode.map WithRadius
            |> ensureType "withRadius"
        , Decode.succeed ThroughThreePointsStuff
            |> Decode.required "pointA" aPointDecoder
            |> Decode.required "pointB" aPointDecoder
            |> Decode.required "pointC" aPointDecoder
            |> Decode.map ThroughThreePoints
            |> ensureType "throughThreePoints"
        , Decode.succeed TransformedCircleStuff
            |> Decode.required "circle" aCircleDecoder
            |> Decode.required "transformation" aTransformationDecoder
            |> Decode.map TransformedCircle
            |> ensureType "transformedCircle"
        ]
        |> Decode.map Circle


curveDecoder : Decoder Curve
curveDecoder =
    Decode.oneOf
        [ Decode.succeed StraightStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map Straight
            |> ensureType "straight"
        , Decode.succeed QuadraticStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map Quadratic
            |> ensureType "quadratic"
        , Decode.succeed CubicStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map Cubic
            |> ensureType "cubic"
        , Decode.succeed TransformedCurveStuff
            |> Decode.required "curve" aCurveDecoder
            |> Decode.required "transformation" aTransformationDecoder
            |> Decode.map TransformedCurve
            |> ensureType "transformedCurve"
        ]
        |> Decode.map Curve


detailDecoder : Decoder Detail
detailDecoder =
    Decode.succeed DetailInfo
        |> Decode.required "firstCurve" firstCurveDecoder
        |> Decode.required "nextCurves" (Decode.list nextCurveDecoder)
        |> Decode.required "lastCurve" lastCurveDecoder
        |> Decode.map Detail


firstCurveDecoder : Decoder FirstCurve
firstCurveDecoder =
    Decode.oneOf
        [ Decode.succeed FirstStraightStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map FirstStraight
            |> ensureType "firstStraight"
        , Decode.succeed FirstQuadraticStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map FirstQuadratic
            |> ensureType "firstQuadratic"
        , Decode.succeed FirstCubicStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map FirstCubic
            |> ensureType "firstCubic"
        ]


nextCurveDecoder : Decoder NextCurve
nextCurveDecoder =
    Decode.oneOf
        [ Decode.succeed NextStraightStuff
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map NextStraight
            |> ensureType "nextStraight"
        , Decode.succeed NextQuadraticStuff
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map NextQuadratic
            |> ensureType "nextQuadratic"
        , Decode.succeed NextCubicStuff
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map NextCubic
            |> ensureType "nextCubic"
        ]


lastCurveDecoder : Decoder LastCurve
lastCurveDecoder =
    Decode.oneOf
        [ Decode.succeed LastStraight
            |> ensureType "lastStraight"
        , Decode.succeed LastQuadraticStuff
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.map LastQuadratic
            |> ensureType "lastQuadratic"
        , Decode.succeed LastCubicStuff
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.map LastCubic
            |> ensureType "lastCubic"
        ]


transformationDecoder : Decoder Transformation
transformationDecoder =
    Decode.oneOf
        [ Decode.succeed TranslateByStuff
            |> Decode.required "direction" directionDecoder
            |> Decode.required "distance" Decode.string
            |> Decode.map TranslateBy
        , Decode.succeed TranslateFromToStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map TranslateFromTo
        , Decode.succeed RotateAroundStuff
            |> Decode.required "centerPoint" aPointDecoder
            |> Decode.required "angle" Decode.string
            |> Decode.map RotateAround
        ]
        |> Decode.map Transformation


intersectableDecoder : Decoder Intersectable
intersectableDecoder =
    Decode.oneOf
        [ Decode.succeed IntersectableAxis
            |> Decode.required "axis" axisDecoder
            |> ensureType "intersectableAxis"
        , Decode.succeed IntersectableCircle
            |> Decode.required "circle" circleDecoder
            |> ensureType "intersectableCircle"
        , Decode.succeed IntersectableCurve
            |> Decode.required "curve" curveDecoder
            |> ensureType "intersectableCurve"
        ]



-- SHARED


directionDecoder : Decoder Direction
directionDecoder =
    Decode.oneOf
        [ Decode.succeed Leftward
            |> ensureType "leftward"
        , Decode.succeed Rightward
            |> ensureType "rightward"
        , Decode.succeed Up
            |> ensureType "up"
        , Decode.succeed Down
            |> ensureType "down"
        , Decode.succeed DirectionAngle
            |> Decode.required "angle" Decode.string
            |> ensureType "directionAngle"
        ]


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.oneOf
        [ Decode.succeed Horizontal
            |> ensureType "horizontal"
        , Decode.succeed Vertical
            |> ensureType "vertical"
        , Decode.succeed OrientationAngle
            |> Decode.required "angle" Decode.string
            |> ensureType "orientationAngle"
        ]


oneInTwoDecoder : Decoder OneInTwo
oneInTwoDecoder =
    Decode.oneOf
        [ Decode.succeed FirstInTwo
            |> ensureType "firstInTwo"
        , Decode.succeed SecondInTwo
            |> ensureType "secondInTwo"
        ]


aPointDecoder : Decoder (A Point)
aPointDecoder =
    aObjectDecoder "point" (Decode.lazy (\_ -> pointDecoder))


aAxisDecoder : Decoder (A Axis)
aAxisDecoder =
    aObjectDecoder "axis" (Decode.lazy (\_ -> axisDecoder))


aCircleDecoder : Decoder (A Circle)
aCircleDecoder =
    aObjectDecoder "circle" (Decode.lazy (\_ -> circleDecoder))


aCurveDecoder : Decoder (A Curve)
aCurveDecoder =
    aObjectDecoder "curve" (Decode.lazy (\_ -> curveDecoder))


aDetailDecoder : Decoder (A Detail)
aDetailDecoder =
    aObjectDecoder "detail" (Decode.lazy (\_ -> detailDecoder))


aTransformationDecoder : Decoder (A Transformation)
aTransformationDecoder =
    aObjectDecoder "transformation" (Decode.lazy (\_ -> transformationDecoder))


aIntersectableDecoder : Decoder (A Intersectable)
aIntersectableDecoder =
    aObjectDecoder "intersectable" (Decode.lazy (\_ -> intersectableDecoder))


aObjectDecoder : String -> Decoder object -> Decoder (A object)
aObjectDecoder objectType objectDecoder =
    Decode.oneOf
        [ Decode.succeed That
            |> Decode.required "name" Decode.string
            |> ensureType "that"
        , Decode.succeed This
            |> Decode.required objectType objectDecoder
            |> ensureType "this"
        ]



-- HELPER


ensureType : String -> Decoder a -> Decoder a
ensureType type_ dataDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\rawType ->
                if rawType == type_ then
                    dataDecoder

                else
                    Decode.fail "not a valid type"
            )
