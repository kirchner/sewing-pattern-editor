module Pattern exposing
    ( Pattern, empty
    , Point, Axis, Circle, Curve, Detail
    , Object(..)
    , Intersectable(..), IntersectableTag(..), intersectableTag, hashIntersectable, inlinedIntersectable
    , A, this, name, inlined, hash
    , whichSize
    , insertPoint, insertAxis, insertCircle, insertCurve, insertDetail
    , insertTransformation
    , insertVariable
    , InsertHelp(..)
    , replacePoint, replaceAxis, replaceCircle, replaceCurve, replaceDetail
    , replaceVariable
    , ReplaceHelp(..)
    , removePoint, removeAxis, removeCircle, removeCurve, removeDetail
    , removeVariable
    , points, axes, circles, curves, curvesWith, details, transformations, variables
    , objects
    , pointInfo, PointInfo(..)
    , OriginStuff, FromOnePointStuff, BetweenRatioStuff, BetweenLengthStuff, IntersectionStuff, TransformedPointStuff
    , axisInfo, AxisInfo(..)
    , circleInfo, CircleInfo(..)
    , curveInfo, CurveInfo(..), StraightStuff, QuadraticStuff, CubicStuff
    , detailInfo, DetailInfo, FirstCurve(..), NextCurve(..), LastCurve(..)
    , transformationInfo, TransformationInfo(..)
    , variableInfo
    , Direction(..), Orientation(..), OneInTwo(..)
    , point2d, axis2d, circle2d, curve2d, detail2d, intersectable2d
    , float
    , ComputeHelp(..)
    , Objects
    , objectsDependingOnPoint, objectsNotDependingOnPoint
    , objectsDependingOnAxis, objectsNotDependingOnAxis
    , objectsDependingOnCircle, objectsNotDependingOnCircle
    , objectsDependingOnCurve, objectsNotDependingOnCurve
    , objectsDependingOnDetail, objectsNotDependingOnDetail
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

@docs Object
@docs Intersectable, IntersectableTag, intersectableTag, hashIntersectable, inlinedIntersectable


# Nest and reference

@docs A, this, name, inlined, hash
@docs whichSize


# Insert

@docs insertPoint, insertAxis, insertCircle, insertCurve, insertDetail
@docs insertTransformation
@docs insertVariable
@docs InsertHelp


# Replace

@docs replacePoint, replaceAxis, replaceCircle, replaceCurve, replaceDetail
@docs replaceVariable
@docs ReplaceHelp


# Remove

@docs removePoint, removeAxis, removeCircle, removeCurve, removeDetail
@docs removeVariable


# Query

@docs points, axes, circles, curves, curvesWith, details, transformations, variables

@docs objects


## Info

@docs pointInfo, PointInfo
@docs OriginStuff, FromOnePointStuff, BetweenRatioStuff, BetweenLengthStuff, IntersectionStuff, TransformedPointStuff
@docs axisInfo, AxisInfo
@docs circleInfo, CircleInfo
@docs curveInfo, CurveInfo, StraightStuff, QuadraticStuff, CubicStuff
@docs detailInfo, DetailInfo, FirstCurve, NextCurve, LastCurve
@docs transformationInfo, TransformationInfo
@docs variableInfo


## Shared types

@docs Direction, Orientation, OneInTwo


# Compute

@docs point2d, axis2d, circle2d, curve2d, detail2d, intersectable2d
@docs float

@docs ComputeHelp


# Dependencies

@docs Objects

@docs objectsDependingOnPoint, objectsNotDependingOnPoint
@docs objectsDependingOnAxis, objectsNotDependingOnAxis
@docs objectsDependingOnCircle, objectsNotDependingOnCircle
@docs objectsDependingOnCurve, objectsNotDependingOnCurve
@docs objectsDependingOnDetail, objectsNotDependingOnDetail


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

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Axis2d.Extra as Axis2d
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Curve2d exposing (Curve2d(..))
import Detail2d exposing (Detail2d, LastCurve2d(..), NextCurve2d(..))
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Expr exposing (BoolExpr(..), Expr(..))
import Intersectable2d exposing (Intersectable2d(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Parser exposing (DeadEnd)
import Point2d exposing (Point2d)
import Point2d.Extra as Point2d
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Set exposing (Set)
import State exposing (State)
import StateResult
import Vector2d


{-| -}
type Pattern coordinates
    = Pattern (PatternData coordinates)


type alias PatternData coordinates =
    { points : Dict String Point
    , axes : Dict String Axis
    , circles : Dict String Circle
    , curves : Dict String Curve
    , details : Dict String Detail
    , transformations : Dict String Transformation
    , variables : Dict String String

    -- CACHE
    , point2ds : Dict String (Result ComputeHelp (Point2d Meters coordinates))
    , axis2ds : Dict String (Result ComputeHelp (Axis2d Meters coordinates))
    , circle2ds : Dict String (Result ComputeHelp (Circle2d Meters coordinates))
    , curve2ds : Dict String (Result ComputeHelp (Curve2d Meters coordinates))
    , detail2ds : Dict String (Result ComputeHelp (Detail2d Meters coordinates))
    , variablesCache : Dict String (Result ComputeHelp Float)
    }


{-| -}
empty : Pattern coordinates
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


{-| -}
type Point
    = Point_ PointInfo


{-| -}
type Axis
    = Axis_ AxisInfo


{-| -}
type Circle
    = Circle_ CircleInfo


{-| -}
type Curve
    = Curve_ CurveInfo


{-| -}
type Detail
    = Detail_ DetailInfo


{-| -}
type Transformation
    = Transformation TransformationInfo



---- NEST AND REFERENCE


{-| -}
type A object
    = That String
    | This object


{-| -}
this : object -> A object
this object =
    This object


{-| -}
name : A object -> Maybe String
name aObject =
    case aObject of
        That n ->
            Just n

        This _ ->
            Nothing


isThatWith : String -> A object -> Bool
isThatWith name_ aObject =
    case aObject of
        That n ->
            n == name_

        This _ ->
            False


{-| -}
inlined : A object -> Bool
inlined aObject =
    case aObject of
        That _ ->
            False

        This _ ->
            True


{-| -}
inlinedIntersectable : Intersectable -> Bool
inlinedIntersectable intersectable =
    case intersectable of
        IntersectableAxis aAxis ->
            inlined aAxis

        IntersectableCircle aCircle ->
            inlined aCircle

        IntersectableCurve aCurve ->
            inlined aCurve


{-| -}
hash : A object -> String
hash aObject =
    case aObject of
        That n ->
            n

        This _ ->
            "do-we-even-need-this"


{-| -}
hashIntersectable : Intersectable -> String
hashIntersectable intersectable =
    case intersectable of
        IntersectableAxis aAxis ->
            hash aAxis

        IntersectableCircle aCircle ->
            hash aCircle

        IntersectableCurve aCurve ->
            hash aCurve


{-| -}
type Object
    = Point (A Point)
    | Axis (A Axis)
    | Circle (A Circle)
    | Curve (A Curve)
    | Detail (A Detail)


{-| -}
type Intersectable
    = IntersectableAxis (A Axis)
    | IntersectableCircle (A Circle)
    | IntersectableCurve (A Curve)


{-| -}
type IntersectableTag
    = IntersectableAxisTag
    | IntersectableCircleTag
    | IntersectableCurveTag


{-| -}
intersectableTag : Intersectable -> IntersectableTag
intersectableTag intersectable =
    case intersectable of
        IntersectableAxis _ ->
            IntersectableAxisTag

        IntersectableCircle _ ->
            IntersectableCircleTag

        IntersectableCurve _ ->
            IntersectableCurveTag



---- INSERT


{-| -}
insertPoint : String -> Point -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
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


{-| -}
insertAxis : String -> Axis -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
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


{-| -}
insertCircle : String -> Circle -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
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


{-| -}
insertCurve : String -> Curve -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
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


{-| -}
insertDetail : String -> Detail -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
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


{-| -}
insertTransformation : String -> Transformation -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
insertTransformation n transformation pattern =
    Err NotImplementedYet


{-| -}
insertVariable : String -> String -> Pattern coordinates -> Result InsertHelp (Pattern coordinates)
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


{-| -}
type InsertHelp
    = NameTaken
    | BadObject ComputeHelp
    | NotImplementedYet


nameTaken : String -> PatternData coordinates -> Bool
nameTaken name_ data =
    Dict.member name_ data.points
        || Dict.member name_ data.axes
        || Dict.member name_ data.circles
        || Dict.member name_ data.curves
        || Dict.member name_ data.details
        || Dict.member name_ data.transformations
        || Dict.member name_ data.variables



---- REPLACE


{-| -}
replacePoint : A Point -> Point -> Pattern coordinates -> Result ReplaceHelp (Pattern coordinates)
replacePoint aPoint newPoint ((Pattern data) as pattern) =
    case aPoint of
        That name_ ->
            let
                circularDependency =
                    objectsDependingOnPoint pattern (This newPoint)
                        |> .points
                        |> List.any (isThatWith name_)
            in
            if circularDependency then
                Err CircularDependency

            else
                let
                    ( result, newPattern ) =
                        State.run
                            (Pattern
                                { data
                                    | points = Dict.insert name_ newPoint data.points
                                    , point2ds = Dict.remove name_ data.point2ds
                                }
                            )
                            (point2d (That name_))
                in
                case result of
                    Err computeHelp ->
                        Err (BadNewObject computeHelp)

                    Ok _ ->
                        Ok (regenerateCaches newPattern)

        This _ ->
            Err ObjectDoesNotExist


{-| -}
replaceAxis : A Axis -> Axis -> Pattern coordinates -> Result ReplaceHelp (Pattern coordinates)
replaceAxis aAxis newAxis ((Pattern data) as pattern) =
    case aAxis of
        That name_ ->
            let
                circularDependency =
                    objectsDependingOnAxis pattern (This newAxis)
                        |> .points
                        |> List.any (isThatWith name_)
            in
            if circularDependency then
                Err CircularDependency

            else
                let
                    ( result, newPattern ) =
                        State.run
                            (Pattern
                                { data
                                    | axes = Dict.insert name_ newAxis data.axes
                                    , axis2ds = Dict.remove name_ data.axis2ds
                                }
                            )
                            (point2d (That name_))
                in
                case result of
                    Err computeHelp ->
                        Err (BadNewObject computeHelp)

                    Ok _ ->
                        Ok (regenerateCaches newPattern)

        This _ ->
            Err ObjectDoesNotExist


{-| -}
replaceCircle : A Circle -> Circle -> Pattern coordinates -> Result ReplaceHelp (Pattern coordinates)
replaceCircle aCircle newCircle ((Pattern data) as pattern) =
    case aCircle of
        That name_ ->
            let
                circularDependency =
                    objectsDependingOnCircle pattern (This newCircle)
                        |> .circles
                        |> List.any (isThatWith name_)
            in
            if circularDependency then
                Err CircularDependency

            else
                let
                    ( result, newPattern ) =
                        State.run
                            (Pattern
                                { data
                                    | circles = Dict.insert name_ newCircle data.circles
                                    , circle2ds = Dict.remove name_ data.circle2ds
                                }
                            )
                            (circle2d (That name_))
                in
                case result of
                    Err computeHelp ->
                        Err (BadNewObject computeHelp)

                    Ok _ ->
                        Ok (regenerateCaches newPattern)

        This _ ->
            Err ObjectDoesNotExist


{-| -}
replaceCurve : A Curve -> Curve -> Pattern coordinates -> Result ReplaceHelp (Pattern coordinates)
replaceCurve aCurve newCurve ((Pattern data) as pattern) =
    case aCurve of
        That name_ ->
            let
                circularDependency =
                    objectsDependingOnCurve pattern (This newCurve)
                        |> .curves
                        |> List.any (isThatWith name_)
            in
            if circularDependency then
                Err CircularDependency

            else
                let
                    ( result, newPattern ) =
                        State.run
                            (Pattern
                                { data
                                    | curves = Dict.insert name_ newCurve data.curves
                                    , curve2ds = Dict.remove name_ data.curve2ds
                                }
                            )
                            (curve2d (That name_))
                in
                case result of
                    Err computeHelp ->
                        Err (BadNewObject computeHelp)

                    Ok _ ->
                        Ok (regenerateCaches newPattern)

        This _ ->
            Err ObjectDoesNotExist


{-| -}
replaceDetail : A Detail -> Detail -> Pattern coordinates -> Result ReplaceHelp (Pattern coordinates)
replaceDetail aDetail newDetail ((Pattern data) as pattern) =
    case aDetail of
        That name_ ->
            let
                circularDependency =
                    objectsDependingOnDetail pattern (This newDetail)
                        |> .details
                        |> List.any (isThatWith name_)
            in
            if circularDependency then
                Err CircularDependency

            else
                let
                    ( result, newPattern ) =
                        State.run
                            (Pattern
                                { data
                                    | details = Dict.insert name_ newDetail data.details
                                    , detail2ds = Dict.remove name_ data.detail2ds
                                }
                            )
                            (detail2d (That name_))
                in
                case result of
                    Err computeHelp ->
                        Err (BadNewObject computeHelp)

                    Ok _ ->
                        Ok (regenerateCaches newPattern)

        This _ ->
            Err ObjectDoesNotExist


{-| -}
replaceVariable : String -> String -> Pattern coordinates -> Result ReplaceHelp (Pattern coordinates)
replaceVariable variable newValue ((Pattern data) as pattern) =
    let
        circularDependency =
            objectsDependingOnVariable pattern variable
                |> .variables
                |> List.member variable
    in
    if circularDependency then
        Err CircularDependency

    else
        let
            ( result, newPattern ) =
                State.run
                    (Pattern
                        { data
                            | variables = Dict.insert variable newValue data.variables
                            , variablesCache = Dict.remove variable data.variablesCache
                        }
                    )
                    (float variable)
        in
        case result of
            Err computeHelp ->
                Err (BadNewObject computeHelp)

            Ok _ ->
                Ok (regenerateCaches newPattern)


{-| -}
type ReplaceHelp
    = BadNewObject ComputeHelp
    | CircularDependency
    | ObjectDoesNotExist



---- REMOVE


{-| -}
removePoint : A Point -> Pattern coordinates -> Pattern coordinates
removePoint aPoint pattern =
    case aPoint of
        That name_ ->
            let
                (Pattern data) =
                    removeObjects (objectsDependingOnPoint pattern aPoint) pattern
            in
            regenerateCaches <|
                Pattern
                    { data
                        | points = Dict.remove name_ data.points
                        , point2ds = Dict.empty
                    }

        This _ ->
            pattern


{-| -}
removeAxis : A Axis -> Pattern coordinates -> Pattern coordinates
removeAxis aAxis pattern =
    case aAxis of
        That name_ ->
            let
                (Pattern data) =
                    removeObjects (objectsDependingOnAxis pattern aAxis) pattern
            in
            regenerateCaches <|
                Pattern
                    { data
                        | axes = Dict.remove name_ data.axes
                        , axis2ds = Dict.empty
                    }

        This _ ->
            pattern


{-| -}
removeCircle : A Circle -> Pattern coordinates -> Pattern coordinates
removeCircle aCircle pattern =
    case aCircle of
        That name_ ->
            let
                (Pattern data) =
                    removeObjects (objectsDependingOnCircle pattern aCircle) pattern
            in
            regenerateCaches <|
                Pattern
                    { data
                        | circles = Dict.remove name_ data.circles
                        , circle2ds = Dict.empty
                    }

        This _ ->
            pattern


{-| -}
removeCurve : A Curve -> Pattern coordinates -> Pattern coordinates
removeCurve aCurve pattern =
    case aCurve of
        That name_ ->
            let
                (Pattern data) =
                    removeObjects (objectsDependingOnCurve pattern aCurve) pattern
            in
            regenerateCaches <|
                Pattern
                    { data
                        | curves = Dict.remove name_ data.curves
                        , curve2ds = Dict.empty
                    }

        This _ ->
            pattern


{-| -}
removeDetail : A Detail -> Pattern coordinates -> Pattern coordinates
removeDetail aDetail pattern =
    case aDetail of
        That name_ ->
            let
                (Pattern data) =
                    removeObjects (objectsDependingOnDetail pattern aDetail) pattern
            in
            regenerateCaches <|
                Pattern
                    { data
                        | details = Dict.remove name_ data.details
                        , detail2ds = Dict.empty
                    }

        This _ ->
            pattern


{-| -}
removeVariable : String -> Pattern coordinates -> Pattern coordinates
removeVariable variable pattern =
    let
        (Pattern data) =
            removeObjects (objectsDependingOnVariable pattern variable) pattern
    in
    regenerateCaches <|
        Pattern
            { data
                | variables = Dict.remove variable data.variables
                , variablesCache = Dict.empty
            }


{-| -}
removeObjects : Objects -> Pattern coordinates -> Pattern coordinates
removeObjects objects_ (Pattern data) =
    Pattern
        { data
            | points =
                Dict.filter
                    (\thisName _ ->
                        objects_.points
                            |> List.filterMap name
                            |> List.member thisName
                            |> not
                    )
                    data.points
            , point2ds = Dict.empty
        }


regenerateCaches : Pattern coordinates -> Pattern coordinates
regenerateCaches ((Pattern data) as pattern) =
    State.finalState
        (Pattern
            { data
                | point2ds = Dict.empty
                , axis2ds = Dict.empty
                , circle2ds = Dict.empty
                , curve2ds = Dict.empty
                , detail2ds = Dict.empty
                , variablesCache = Dict.empty
            }
        )
        (State.combine <|
            [ State.traverse point2d (points pattern)
                |> State.map (always ())
            , State.traverse axis2d (axes pattern)
                |> State.map (always ())
            , State.traverse circle2d (circles pattern)
                |> State.map (always ())
            , State.traverse curve2d (curves pattern)
                |> State.map (always ())
            , State.traverse detail2d (details pattern)
                |> State.map (always ())
            , State.traverse float (variables pattern)
                |> State.map (always ())
            ]
        )



---- QUERY


{-| -}
points : Pattern coordinates -> List (A Point)
points (Pattern data) =
    Dict.keys data.points
        |> List.map That


{-| -}
axes : Pattern coordinates -> List (A Axis)
axes (Pattern data) =
    Dict.keys data.axes
        |> List.map That


{-| -}
circles : Pattern coordinates -> List (A Circle)
circles (Pattern data) =
    Dict.keys data.circles
        |> List.map That


{-| -}
curves : Pattern coordinates -> List (A Curve)
curves (Pattern data) =
    Dict.keys data.curves
        |> List.map That


{-| -}
curvesWith :
    Pattern coordinates
    ->
        { startPoint : Maybe (A Point)
        , endPoint : Maybe (A Point)
        }
    -> List (A Curve)
curvesWith (Pattern data) constraints =
    []


{-| -}
details : Pattern coordinates -> List (A Detail)
details (Pattern data) =
    Dict.keys data.details
        |> List.map That


{-| -}
transformations : Pattern coordinates -> List (A Transformation)
transformations (Pattern data) =
    Dict.keys data.transformations
        |> List.map That


{-| -}
variables : Pattern coordinates -> List String
variables (Pattern data) =
    Dict.keys data.variables


{-| -}
objects : Pattern coordinates -> Objects
objects pattern =
    { points = points pattern
    , axes = axes pattern
    , circles = circles pattern
    , curves = curves pattern
    , details = details pattern
    , variables = variables pattern
    }



-- INFO


{-| -}
pointInfo : A Point -> Pattern coordinates -> Maybe PointInfo
pointInfo aPoint (Pattern data) =
    case aPoint of
        That name_ ->
            Dict.get name_ data.points
                |> Maybe.map (\(Point_ info) -> info)

        This (Point_ info) ->
            Just info


{-| -}
type PointInfo
    = Origin OriginStuff
    | FromOnePoint FromOnePointStuff
    | BetweenRatio BetweenRatioStuff
    | BetweenLength BetweenLengthStuff
    | Intersection IntersectionStuff
    | TransformedPoint TransformedPointStuff


{-| -}
type alias OriginStuff =
    { x : Float
    , y : Float
    }


{-| -}
type alias FromOnePointStuff =
    { basePoint : A Point
    , direction : Direction
    , distance : String
    }


{-| -}
type alias BetweenRatioStuff =
    { basePointA : A Point
    , basePointB : A Point
    , ratio : String
    }


{-| -}
type alias BetweenLengthStuff =
    { basePointA : A Point
    , basePointB : A Point
    , distance : String
    , from : OneInTwo
    }


{-| -}
type alias IntersectionStuff =
    { intersectableA : Intersectable
    , intersectableB : Intersectable
    , which : Int
    }


{-| -}
type alias TransformedPointStuff =
    { point : A Point
    , transformation : A Transformation
    }


{-| -}
axisInfo : A Axis -> Pattern coordinates -> Maybe AxisInfo
axisInfo aAxis (Pattern data) =
    case aAxis of
        That name_ ->
            Dict.get name_ data.axes
                |> Maybe.map (\(Axis_ info) -> info)

        This (Axis_ info) ->
            Just info


{-| -}
type AxisInfo
    = ThroughOnePoint ThroughOnePointStuff
    | ThroughTwoPoints ThroughTwoPointsStuff
    | TransformedAxis TransformedAxisStuff


{-| -}
type alias ThroughOnePointStuff =
    { point : A Point
    , orientation : Orientation
    }


{-| -}
type alias ThroughTwoPointsStuff =
    { pointA : A Point
    , pointB : A Point
    }


{-| -}
type alias TransformedAxisStuff =
    { axis : A Axis
    , transformation : A Transformation
    }


{-| -}
circleInfo : A Circle -> Pattern coordinates -> Maybe CircleInfo
circleInfo aCircle (Pattern data) =
    case aCircle of
        That name_ ->
            Dict.get name_ data.circles
                |> Maybe.map (\(Circle_ info) -> info)

        This (Circle_ info) ->
            Just info


{-| -}
type CircleInfo
    = WithRadius WithRadiusStuff
    | ThroughThreePoints ThroughThreePointsStuff
    | TransformedCircle TransformedCircleStuff


{-| -}
type alias WithRadiusStuff =
    { centerPoint : A Point
    , radius : String
    }


{-| -}
type alias ThroughThreePointsStuff =
    { pointA : A Point
    , pointB : A Point
    , pointC : A Point
    }


{-| -}
type alias TransformedCircleStuff =
    { circle : A Circle
    , transformation : A Transformation
    }


{-| -}
curveInfo : A Curve -> Pattern coordinates -> Maybe CurveInfo
curveInfo aCurve (Pattern data) =
    case aCurve of
        That name_ ->
            Dict.get name_ data.curves
                |> Maybe.map (\(Curve_ info) -> info)

        This (Curve_ info) ->
            Just info


{-| -}
type CurveInfo
    = Straight StraightStuff
    | Quadratic QuadraticStuff
    | Cubic CubicStuff
    | TransformedCurve TransformedCurveStuff


{-| -}
type alias StraightStuff =
    { startPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias QuadraticStuff =
    { startPoint : A Point
    , controlPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias CubicStuff =
    { startPoint : A Point
    , startControlPoint : A Point
    , endControlPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias TransformedCurveStuff =
    { curve : A Curve
    , transformation : A Transformation
    }


{-| -}
detailInfo : A Detail -> Pattern coordinates -> Maybe DetailInfo
detailInfo aDetail (Pattern data) =
    case aDetail of
        That name_ ->
            Dict.get name_ data.details
                |> Maybe.map (\(Detail_ info) -> info)

        This (Detail_ info) ->
            Just info


{-| -}
type alias DetailInfo =
    { firstCurve : FirstCurve
    , nextCurves : List NextCurve
    , lastCurve : LastCurve
    }


{-| -}
type FirstCurve
    = FirstStraight FirstStraightStuff
    | FirstQuadratic FirstQuadraticStuff
    | FirstCubic FirstCubicStuff
    | FirstReferencedCurve FirstReferencedCurveStuff


{-| -}
type alias FirstStraightStuff =
    { startPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias FirstQuadraticStuff =
    { startPoint : A Point
    , controlPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias FirstCubicStuff =
    { startPoint : A Point
    , startControlPoint : A Point
    , endControlPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias FirstReferencedCurveStuff =
    { curve : A Curve
    , reversed : Bool
    }


{-| -}
type NextCurve
    = NextStraight NextStraightStuff
    | NextQuadratic NextQuadraticStuff
    | NextCubic NextCubicStuff
    | NextReferencedCurve NextReferencedCurveStuff


{-| -}
type alias NextStraightStuff =
    { endPoint : A Point
    }


{-| -}
type alias NextQuadraticStuff =
    { controlPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias NextCubicStuff =
    { startControlPoint : A Point
    , endControlPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias NextReferencedCurveStuff =
    { curve : A Curve
    , reversed : Bool
    }


{-| -}
type LastCurve
    = LastStraight
    | LastQuadratic LastQuadraticStuff
    | LastCubic LastCubicStuff
    | LastReferencedCurve LastReferencedCurveStuff


{-| -}
type alias LastQuadraticStuff =
    { controlPoint : A Point
    }


{-| -}
type alias LastCubicStuff =
    { startControlPoint : A Point
    , endControlPoint : A Point
    }


{-| -}
type alias LastReferencedCurveStuff =
    { curve : A Curve
    , reversed : Bool
    }


{-| -}
transformationInfo : A Transformation -> Pattern coordinates -> Maybe TransformationInfo
transformationInfo aTransformation (Pattern data) =
    case aTransformation of
        That name_ ->
            Dict.get name_ data.transformations
                |> Maybe.map (\(Transformation info) -> info)

        This (Transformation info) ->
            Just info


{-| -}
type TransformationInfo
    = TranslateBy TranslateByStuff
    | TranslateFromTo TranslateFromToStuff
    | RotateAround RotateAroundStuff


{-| -}
type alias TranslateByStuff =
    { direction : Direction
    , distance : String
    }


{-| -}
type alias TranslateFromToStuff =
    { startPoint : A Point
    , endPoint : A Point
    }


{-| -}
type alias RotateAroundStuff =
    { centerPoint : A Point
    , angle : String
    }


{-| -}
transformedObjects : A Transformation -> Pattern coordinates -> Maybe TransformedObjects
transformedObjects aTransformation pattern =
    Nothing


{-| -}
type alias TransformedObjects =
    { points : List (A Point)
    , axes : List (A Axis)
    , circles : List (A Circle)
    , curves : List (A Curve)
    }


{-| -}
variableInfo : String -> Pattern coordinates -> Maybe String
variableInfo variable (Pattern data) =
    Dict.get variable data.variables



-- SHARED TYPES


{-| -}
type Direction
    = Leftward
    | Rightward
    | Up
    | Down
    | DirectionAngle String


{-| -}
type OneInTwo
    = FirstInTwo
    | SecondInTwo


{-| -}
type Orientation
    = Horizontal
    | Vertical
    | OrientationAngle String



---- COMPUTE


{-| -}
point2d : A Point -> State (Pattern coordinates) (Result ComputeHelp (Point2d Meters coordinates))
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


computePoint2d : Point -> State (Pattern coordinates) (Result ComputeHelp (Point2d Meters coordinates))
computePoint2d (Point_ info) =
    case info of
        Origin stuff ->
            StateResult.ok <|
                Point2d.meters stuff.x stuff.y

        FromOnePoint stuff ->
            let
                toPoint2d basePoint direction distance =
                    Point2d.translateBy
                        (Vector2d.rTheta (Length.meters distance) direction)
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
                    Point2d.betweenLength basePointA basePointB (Length.meters distance)
                        |> Result.fromMaybe PointsCoincide
            in
            StateResult.ok toPoint2d
                |> StateResult.with (point2d stuff.basePointA)
                |> StateResult.with (point2d stuff.basePointB)
                |> StateResult.with (computeExpr stuff.distance)
                |> StateResult.join

        Intersection stuff ->
            let
                toPoint2d intersectable2dA intersectable2dB =
                    case ( intersectable2dA, intersectable2dB ) of
                        ( Axis2d axisA, Axis2d axisB ) ->
                            Axis2d.intersectionWithAxis axisA axisB
                                |> Result.fromMaybe AxesAreParallel

                        ( Axis2d axis, Circle2d circle ) ->
                            case stuff.which of
                                1 ->
                                    Point2d.secondCircleAxis circle axis
                                        |> Result.fromMaybe AxisAndCircleDoNotIntersect

                                2 ->
                                    Point2d.firstCircleAxis circle axis
                                        |> Result.fromMaybe AxisAndCircleDoNotIntersect

                                _ ->
                                    Err (WhichMustBeBetween 1 2)

                        ( Axis2d axis, Curve2d curve ) ->
                            Err NotComputableYet

                        ( Circle2d circle, Axis2d axis ) ->
                            case stuff.which of
                                1 ->
                                    Point2d.firstCircleAxis circle axis
                                        |> Result.fromMaybe AxisAndCircleDoNotIntersect

                                2 ->
                                    Point2d.secondCircleAxis circle axis
                                        |> Result.fromMaybe AxisAndCircleDoNotIntersect

                                _ ->
                                    Err (WhichMustBeBetween 1 2)

                        ( Circle2d circleA, Circle2d circleB ) ->
                            case stuff.which of
                                1 ->
                                    Point2d.firstCircleCircle circleA circleB
                                        |> Result.fromMaybe CirclesDoNotIntersect

                                2 ->
                                    Point2d.secondCircleCircle circleA circleB
                                        |> Result.fromMaybe CirclesDoNotIntersect

                                _ ->
                                    Err (WhichMustBeBetween 1 2)

                        ( Circle2d circle, Curve2d curve ) ->
                            Err NotComputableYet

                        ( Curve2d curve, Axis2d axis ) ->
                            Err NotComputableYet

                        ( Curve2d curve, Circle2d circle ) ->
                            Err NotComputableYet

                        ( Curve2d curveA, Curve2d curveB ) ->
                            Err NotComputableYet
            in
            StateResult.ok toPoint2d
                |> StateResult.with (intersectable2d stuff.intersectableA)
                |> StateResult.with (intersectable2d stuff.intersectableB)
                |> StateResult.join

        TransformedPoint stuff ->
            StateResult.err NotComputableYet


{-| -}
intersectable2d : Intersectable -> State (Pattern coordinates) (Result ComputeHelp (Intersectable2d Meters coordinates))
intersectable2d intersectable =
    case intersectable of
        IntersectableAxis aAxis ->
            StateResult.map Axis2d (axis2d aAxis)

        IntersectableCircle aCircle ->
            StateResult.map Circle2d (circle2d aCircle)

        IntersectableCurve aCurve ->
            StateResult.map Curve2d (curve2d aCurve)


{-| -}
axis2d : A Axis -> State (Pattern coordinates) (Result ComputeHelp (Axis2d Meters coordinates))
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


computeAxis2d : Axis -> State (Pattern coordinates) (Result ComputeHelp (Axis2d Meters coordinates))
computeAxis2d (Axis_ info) =
    case info of
        ThroughOnePoint stuff ->
            StateResult.ok Axis2d.through
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
            StateResult.err NotComputableYet


{-| -}
circle2d : A Circle -> State (Pattern coordinates) (Result ComputeHelp (Circle2d Meters coordinates))
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


computeCircle2d : Circle -> State (Pattern coordinates) (Result ComputeHelp (Circle2d Meters coordinates))
computeCircle2d (Circle_ info) =
    case info of
        WithRadius stuff ->
            let
                toCircle2d radius center =
                    Circle2d.withRadius (Length.meters radius) center
            in
            StateResult.ok toCircle2d
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
            StateResult.err NotComputableYet


{-| -}
curve2d : A Curve -> State (Pattern coordinates) (Result ComputeHelp (Curve2d Meters coordinates))
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


computeCurve2d : Curve -> State (Pattern coordinates) (Result ComputeHelp (Curve2d Meters coordinates))
computeCurve2d (Curve_ info) =
    case info of
        Straight stuff ->
            let
                toCurve2d startPoint_ endPoint_ =
                    LineSegment2d (LineSegment2d.from startPoint_ endPoint_)
            in
            StateResult.ok toCurve2d
                |> StateResult.with (point2d stuff.startPoint)
                |> StateResult.with (point2d stuff.endPoint)

        Quadratic stuff ->
            let
                toCurve2d startPoint_ controlPoint endPoint_ =
                    QuadraticSpline2d <|
                        QuadraticSpline2d.fromControlPoints
                            startPoint_
                            controlPoint
                            endPoint_
            in
            StateResult.ok toCurve2d
                |> StateResult.with (point2d stuff.startPoint)
                |> StateResult.with (point2d stuff.controlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        Cubic stuff ->
            let
                toCurve2d startPoint_ startControlPoint endControlPoint endPoint_ =
                    CubicSpline2d <|
                        CubicSpline2d.fromControlPoints
                            startPoint_
                            startControlPoint
                            endControlPoint
                            endPoint_
            in
            StateResult.ok toCurve2d
                |> StateResult.with (point2d stuff.startPoint)
                |> StateResult.with (point2d stuff.startControlPoint)
                |> StateResult.with (point2d stuff.endControlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        TransformedCurve stuff ->
            StateResult.err NotComputableYet


{-| -}
detail2d : A Detail -> State (Pattern coordinates) (Result ComputeHelp (Detail2d Meters coordinates))
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


computeDetail2d : Detail -> State (Pattern coordinates) (Result ComputeHelp (Detail2d Meters coordinates))
computeDetail2d (Detail_ info) =
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
                    let
                        toStartPoint curve maybeNextCurve maybeLastCurve =
                            case
                                startPointWith
                                    { nextCurve = maybeNextCurve
                                    , previousCurve = maybeLastCurve
                                    }
                                    (curve |> Curve2d.reverseIf stuff.reversed)
                            of
                                Nothing ->
                                    Err DisconnectedCurves

                                Just startPoint_ ->
                                    Ok startPoint_
                    in
                    StateResult.ok toStartPoint
                        |> StateResult.with (curve2d stuff.curve)
                        |> StateResult.with
                            (case info.nextCurves of
                                (NextReferencedCurve nextStuff) :: _ ->
                                    StateResult.map Just
                                        (curve2d nextStuff.curve
                                            |> StateResult.map
                                                (Curve2d.reverseIf nextStuff.reversed)
                                        )

                                _ ->
                                    StateResult.ok Nothing
                            )
                        |> StateResult.with
                            (case info.lastCurve of
                                LastReferencedCurve lastStuff ->
                                    StateResult.map Just
                                        (curve2d lastStuff.curve
                                            |> StateResult.map
                                                (Curve2d.reverseIf lastStuff.reversed)
                                        )

                                _ ->
                                    StateResult.ok Nothing
                            )
                        |> StateResult.join
            )
        |> StateResult.with
            (StateResult.ok (::)
                |> StateResult.with
                    (secondCurve2d info.firstCurve info.nextCurves info.lastCurve)
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
                                { secondControlPoint = controlPoint }
                    in
                    StateResult.ok toLastCurve
                        |> StateResult.with (point2d stuff.controlPoint)

                LastCubic stuff ->
                    let
                        toLastCurve secondControlPoint thirdControlPoint =
                            LastCubicSpline2d
                                { secondControlPoint = secondControlPoint
                                , thirdControlPoint = thirdControlPoint
                                }
                    in
                    StateResult.ok toLastCurve
                        |> StateResult.with (point2d stuff.startControlPoint)
                        |> StateResult.with (point2d stuff.endControlPoint)

                LastReferencedCurve stuff ->
                    let
                        toLastCurve maybePreviousCurve curve maybeFirstCurve =
                            Maybe.map2
                                (\actualStartPoint actualEndPoint ->
                                    if
                                        (actualStartPoint == Curve2d.startPoint curve)
                                            && (actualEndPoint == Curve2d.endPoint curve)
                                    then
                                        Ok (toLastCurve2d curve)

                                    else if
                                        (actualStartPoint == Curve2d.endPoint curve)
                                            && (actualEndPoint == Curve2d.startPoint curve)
                                    then
                                        Ok (toLastCurve2d (Curve2d.reverse curve))

                                    else
                                        Err DisconnectedCurves
                                )
                                (startPointWith
                                    { nextCurve = maybeFirstCurve
                                    , previousCurve = maybePreviousCurve
                                    }
                                    (Curve2d.reverseIf stuff.reversed curve)
                                )
                                (endPointWith
                                    { nextCurve = maybeFirstCurve
                                    , previousCurve = maybePreviousCurve
                                    }
                                    (Curve2d.reverseIf stuff.reversed curve)
                                )
                                |> Maybe.withDefault (Err DisconnectedCurves)

                        toLastCurve2d curve =
                            case curve of
                                LineSegment2d _ ->
                                    LastLineSegment2d

                                QuadraticSpline2d quadraticSpline2d ->
                                    LastQuadraticSpline2d
                                        { secondControlPoint = QuadraticSpline2d.secondControlPoint quadraticSpline2d
                                        }

                                CubicSpline2d cubicSpline2d ->
                                    LastCubicSpline2d
                                        { secondControlPoint = CubicSpline2d.secondControlPoint cubicSpline2d
                                        , thirdControlPoint = CubicSpline2d.thirdControlPoint cubicSpline2d
                                        }
                    in
                    StateResult.ok toLastCurve
                        |> StateResult.with
                            (case List.reverse info.nextCurves of
                                (NextReferencedCurve nextStuff) :: _ ->
                                    StateResult.map Just
                                        (curve2d nextStuff.curve
                                            |> StateResult.map
                                                (Curve2d.reverseIf nextStuff.reversed)
                                        )

                                _ ->
                                    StateResult.ok Nothing
                            )
                        |> StateResult.with (curve2d stuff.curve)
                        |> StateResult.with
                            (case info.firstCurve of
                                FirstReferencedCurve firstStuff ->
                                    StateResult.map Just
                                        (curve2d firstStuff.curve
                                            |> StateResult.map
                                                (Curve2d.reverseIf firstStuff.reversed)
                                        )

                                _ ->
                                    StateResult.ok Nothing
                            )
                        |> StateResult.join
            )


secondCurve2d :
    FirstCurve
    -> List NextCurve
    -> LastCurve
    -> State (Pattern coordinates) (Result ComputeHelp (NextCurve2d Meters coordinates))
secondCurve2d firstCurve nextCurves lastCurve =
    case firstCurve of
        FirstStraight stuff ->
            let
                toCurve endPoint_ =
                    NextLineSegment2d
                        { endPoint = endPoint_ }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.endPoint)

        FirstQuadratic stuff ->
            let
                toCurve controlPoint endPoint_ =
                    NextQuadraticSpline2d
                        { secondControlPoint = controlPoint
                        , thirdControlPoint = endPoint_
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.controlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        FirstCubic stuff ->
            let
                toCurve startControlPoint endControlPoint endPoint_ =
                    NextCubicSpline2d
                        { secondControlPoint = startControlPoint
                        , thirdControlPoint = endControlPoint
                        , fourthControlPoint = endPoint_
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.startControlPoint)
                |> StateResult.with (point2d stuff.endControlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        FirstReferencedCurve stuff ->
            let
                toFirstCurve curve maybeNextCurve maybeLastCurve =
                    Maybe.map2
                        (\actualStartPoint actualEndPoint ->
                            if
                                (actualStartPoint == Curve2d.startPoint curve)
                                    && (actualEndPoint == Curve2d.endPoint curve)
                            then
                                Ok (toFirstCurve2d curve)

                            else if
                                (actualStartPoint == Curve2d.endPoint curve)
                                    && (actualEndPoint == Curve2d.startPoint curve)
                            then
                                Ok (toFirstCurve2d (Curve2d.reverse curve))

                            else
                                Err DisconnectedCurves
                        )
                        (startPointWith
                            { nextCurve = maybeNextCurve
                            , previousCurve = maybeLastCurve
                            }
                            curve
                        )
                        (endPointWith
                            { nextCurve = maybeNextCurve
                            , previousCurve = maybeLastCurve
                            }
                            curve
                        )
                        |> Maybe.withDefault (Err DisconnectedCurves)

                toFirstCurve2d curve =
                    case curve of
                        LineSegment2d lineSegment2d ->
                            NextLineSegment2d
                                { endPoint = LineSegment2d.endPoint lineSegment2d }

                        QuadraticSpline2d quadraticSpline2d ->
                            NextQuadraticSpline2d
                                { secondControlPoint = QuadraticSpline2d.secondControlPoint quadraticSpline2d
                                , thirdControlPoint = QuadraticSpline2d.endPoint quadraticSpline2d
                                }

                        CubicSpline2d cubicSpline2d ->
                            NextCubicSpline2d
                                { secondControlPoint = CubicSpline2d.secondControlPoint cubicSpline2d
                                , thirdControlPoint = CubicSpline2d.thirdControlPoint cubicSpline2d
                                , fourthControlPoint = CubicSpline2d.endPoint cubicSpline2d
                                }
            in
            StateResult.ok toFirstCurve
                |> StateResult.with
                    (curve2d stuff.curve
                        |> StateResult.map (Curve2d.reverseIf stuff.reversed)
                    )
                |> StateResult.with
                    (case nextCurves of
                        (NextReferencedCurve nextStuff) :: _ ->
                            StateResult.map Just
                                (curve2d nextStuff.curve
                                    |> StateResult.map (Curve2d.reverseIf nextStuff.reversed)
                                )

                        _ ->
                            StateResult.ok Nothing
                    )
                |> StateResult.with
                    (case lastCurve of
                        LastReferencedCurve lastStuff ->
                            StateResult.map Just
                                (curve2d lastStuff.curve
                                    |> StateResult.map (Curve2d.reverseIf lastStuff.reversed)
                                )

                        _ ->
                            StateResult.ok Nothing
                    )
                |> StateResult.join


nextCurve2d : NextCurve -> State (Pattern coordinates) (Result ComputeHelp (NextCurve2d Meters coordinates))
nextCurve2d nextCurve =
    case nextCurve of
        NextStraight stuff ->
            let
                toCurve endPoint_ =
                    NextLineSegment2d
                        { endPoint = endPoint_ }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.endPoint)

        NextQuadratic stuff ->
            let
                toCurve controlPoint endPoint_ =
                    NextQuadraticSpline2d
                        { secondControlPoint = controlPoint
                        , thirdControlPoint = endPoint_
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.controlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        NextCubic stuff ->
            let
                toCurve startControlPoint endControlPoint endPoint_ =
                    NextCubicSpline2d
                        { secondControlPoint = startControlPoint
                        , thirdControlPoint = endControlPoint
                        , fourthControlPoint = endPoint_
                        }
            in
            StateResult.ok toCurve
                |> StateResult.with (point2d stuff.startControlPoint)
                |> StateResult.with (point2d stuff.endControlPoint)
                |> StateResult.with (point2d stuff.endPoint)

        NextReferencedCurve stuff ->
            let
                toNextCurve curve =
                    toNextCurve2d curve

                toNextCurve2d curve =
                    case curve of
                        LineSegment2d lineSegment2d ->
                            NextLineSegment2d
                                { endPoint = LineSegment2d.endPoint lineSegment2d }

                        QuadraticSpline2d quadraticSpline2d ->
                            NextQuadraticSpline2d
                                { secondControlPoint = QuadraticSpline2d.secondControlPoint quadraticSpline2d
                                , thirdControlPoint = QuadraticSpline2d.thirdControlPoint quadraticSpline2d
                                }

                        CubicSpline2d cubicSpline2d ->
                            NextCubicSpline2d
                                { secondControlPoint = CubicSpline2d.secondControlPoint cubicSpline2d
                                , thirdControlPoint = CubicSpline2d.thirdControlPoint cubicSpline2d
                                , fourthControlPoint = CubicSpline2d.fourthControlPoint cubicSpline2d
                                }
            in
            StateResult.ok toNextCurve
                |> StateResult.with
                    (curve2d stuff.curve
                        |> StateResult.map (Curve2d.reverseIf stuff.reversed)
                    )


startPointWith :
    { nextCurve : Maybe (Curve2d Meters coordinates)
    , previousCurve : Maybe (Curve2d Meters coordinates)
    }
    -> Curve2d Meters coordinates
    -> Maybe (Point2d Meters coordinates)
startPointWith constraints curve =
    let
        connectedTo curve_ point =
            (Curve2d.startPoint curve_ == point)
                || (Curve2d.endPoint curve_ == point)
    in
    case ( constraints.nextCurve, constraints.previousCurve ) of
        ( Nothing, Nothing ) ->
            Just (Curve2d.startPoint curve)

        ( Just nextCurve, Nothing ) ->
            if Curve2d.endPoint curve |> connectedTo nextCurve then
                Just (Curve2d.startPoint curve)

            else if Curve2d.startPoint curve |> connectedTo nextCurve then
                Just (Curve2d.endPoint curve)

            else
                Nothing

        ( Nothing, Just previousCurve ) ->
            if Curve2d.startPoint curve |> connectedTo previousCurve then
                Just (Curve2d.startPoint curve)

            else if Curve2d.endPoint curve |> connectedTo previousCurve then
                Just (Curve2d.endPoint curve)

            else
                Nothing

        ( Just nextCurve, Just previousCurve ) ->
            if
                (Curve2d.endPoint curve |> connectedTo nextCurve)
                    && (Curve2d.startPoint curve |> connectedTo previousCurve)
            then
                Just (Curve2d.startPoint curve)

            else if
                (Curve2d.endPoint curve |> connectedTo previousCurve)
                    && (Curve2d.startPoint curve |> connectedTo nextCurve)
            then
                Just (Curve2d.endPoint curve)

            else
                Nothing


endPointWith :
    { nextCurve : Maybe (Curve2d Meters coordinates)
    , previousCurve : Maybe (Curve2d Meters coordinates)
    }
    -> Curve2d Meters coordinates
    -> Maybe (Point2d Meters coordinates)
endPointWith constraints curve =
    let
        connectedTo curve_ point =
            (Curve2d.startPoint curve_ == point)
                || (Curve2d.endPoint curve_ == point)
    in
    case ( constraints.nextCurve, constraints.previousCurve ) of
        ( Nothing, Nothing ) ->
            Just (Curve2d.endPoint curve)

        ( Just nextCurve, Nothing ) ->
            if Curve2d.endPoint curve |> connectedTo nextCurve then
                Just (Curve2d.endPoint curve)

            else if Curve2d.startPoint curve |> connectedTo nextCurve then
                Just (Curve2d.startPoint curve)

            else
                Nothing

        ( Nothing, Just previousCurve ) ->
            if Curve2d.startPoint curve |> connectedTo previousCurve then
                Just (Curve2d.endPoint curve)

            else if Curve2d.endPoint curve |> connectedTo previousCurve then
                Just (Curve2d.startPoint curve)

            else
                Nothing

        ( Just nextCurve, Just previousCurve ) ->
            if
                (Curve2d.endPoint curve |> connectedTo nextCurve)
                    && (Curve2d.startPoint curve |> connectedTo previousCurve)
            then
                Just (Curve2d.endPoint curve)

            else if
                (Curve2d.endPoint curve |> connectedTo previousCurve)
                    && (Curve2d.startPoint curve |> connectedTo nextCurve)
            then
                Just (Curve2d.startPoint curve)

            else
                Nothing


{-| -}
float : String -> State (Pattern coordinates) (Result ComputeHelp Float)
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


computeDirection : Direction -> State (Pattern coordinates) (Result ComputeHelp Angle)
computeDirection direction =
    case direction of
        Leftward ->
            StateResult.ok (Angle.degrees 180)

        Rightward ->
            StateResult.ok (Angle.degrees 0)

        Up ->
            StateResult.ok (Angle.degrees 270)

        Down ->
            StateResult.ok (Angle.degrees 90)

        DirectionAngle angle ->
            computeExpr angle
                |> StateResult.map Angle.degrees


computeOrientation : Orientation -> State (Pattern coordinates) (Result ComputeHelp (Direction2d c))
computeOrientation orientation =
    case orientation of
        Horizontal ->
            StateResult.ok (Direction2d.degrees 0)

        Vertical ->
            StateResult.ok (Direction2d.degrees 90)

        OrientationAngle angle ->
            computeExpr angle
                |> StateResult.map Direction2d.degrees


computeExpr : String -> State (Pattern coordinates) (Result ComputeHelp Float)
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


evaluateExpr : Expr -> State (Pattern coordinates) (Result ComputeHelp Float)
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
                                |> StateResult.map Length.inMeters

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
                                        |> Maybe.map (Direction2d.toAngle >> Angle.inDegrees)

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


evaluateVariable : String -> State (Pattern coordinates) (Result ComputeHelp Float)
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


evaluateBoolExpr : BoolExpr -> State (Pattern coordinates) (Result ComputeHelp Bool)
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


{-| -}
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
    | DisconnectedCurves
    | NotComputableYet



---- DEPENDENCIES


{-| -}
type alias Objects =
    { points : List (A Point)
    , axes : List (A Axis)
    , circles : List (A Circle)
    , curves : List (A Curve)
    , details : List (A Detail)
    , variables : List String
    }


noObjects : Objects
noObjects =
    { points = []
    , axes = []
    , circles = []
    , curves = []
    , details = []
    , variables = []
    }


{-| -}
objectsDependingOnPoint : Pattern coordinates -> A Point -> Objects
objectsDependingOnPoint ((Pattern data) as pattern) aPoint =
    case name aPoint of
        Nothing ->
            noObjects

        Just name_ ->
            pattern
                |> points
                |> State.traverse (collectObjectsDependingOnPoint data name_ noChains)
                |> State.finalState emptyCollection
                |> objectsFromCollection


{-| -}
objectsNotDependingOnPoint : Pattern coordinates -> A Point -> Objects
objectsNotDependingOnPoint pattern aPoint =
    objects pattern
        |> withoutObjects (objectsDependingOnPoint pattern aPoint)


{-| -}
objectsDependingOnAxis : Pattern coordinates -> A Axis -> Objects
objectsDependingOnAxis ((Pattern data) as pattern) aAxis =
    case name aAxis of
        Nothing ->
            noObjects

        Just name_ ->
            pattern
                |> axes
                |> State.traverse (collectObjectsDependingOnAxis data name_ noChains)
                |> State.finalState emptyCollection
                |> objectsFromCollection


{-| -}
objectsNotDependingOnAxis : Pattern coordinates -> A Axis -> Objects
objectsNotDependingOnAxis pattern aAxis =
    objects pattern
        |> withoutObjects (objectsDependingOnAxis pattern aAxis)


{-| -}
objectsDependingOnCircle : Pattern coordinates -> A Circle -> Objects
objectsDependingOnCircle ((Pattern data) as pattern) aCircle =
    case name aCircle of
        Nothing ->
            noObjects

        Just name_ ->
            pattern
                |> circles
                |> State.traverse (collectObjectsDependingOnCircle data name_ noChains)
                |> State.finalState emptyCollection
                |> objectsFromCollection


{-| -}
objectsNotDependingOnCircle : Pattern coordinates -> A Circle -> Objects
objectsNotDependingOnCircle pattern aCircle =
    objects pattern
        |> withoutObjects (objectsDependingOnCircle pattern aCircle)


{-| -}
objectsDependingOnCurve : Pattern coordinates -> A Curve -> Objects
objectsDependingOnCurve ((Pattern data) as pattern) aCurve =
    case name aCurve of
        Nothing ->
            noObjects

        Just name_ ->
            pattern
                |> curves
                |> State.traverse (collectObjectsDependingOnCurve data name_ noChains)
                |> State.finalState emptyCollection
                |> objectsFromCollection


{-| -}
objectsNotDependingOnCurve : Pattern coordinates -> A Curve -> Objects
objectsNotDependingOnCurve pattern aCurve =
    objects pattern
        |> withoutObjects (objectsDependingOnCurve pattern aCurve)


{-| -}
objectsDependingOnDetail : Pattern coordinates -> A Detail -> Objects
objectsDependingOnDetail ((Pattern data) as pattern) aDetail =
    case name aDetail of
        Nothing ->
            noObjects

        Just name_ ->
            pattern
                |> details
                |> State.traverse (collectObjectsDependingOnDetail data name_ noChains)
                |> State.finalState emptyCollection
                |> objectsFromCollection


{-| -}
objectsNotDependingOnDetail : Pattern coordinates -> A Detail -> Objects
objectsNotDependingOnDetail pattern aDetail =
    objects pattern
        |> withoutObjects (objectsDependingOnDetail pattern aDetail)


{-| -}
objectsDependingOnVariable : Pattern coordinates -> String -> Objects
objectsDependingOnVariable ((Pattern data) as pattern) variable =
    pattern
        |> variables
        |> State.traverse (collectObjectsDependingOnVariable data variable noChains)
        |> State.finalState emptyCollection
        |> objectsFromCollection


withoutObjects : Objects -> Objects -> Objects
withoutObjects b a =
    { points = a.points |> without b.points
    , axes = a.axes |> without b.axes
    , circles = a.circles |> without b.circles
    , curves = a.curves |> without b.curves
    , details = a.details |> without b.details
    , variables =
        a.variables
            |> List.filter (\varA -> not (List.member varA b.variables))
    }


without : List (A object) -> List (A object) -> List (A object)
without bList aList =
    let
        containedInBList a =
            case a of
                That aName ->
                    List.any (isReferenceTo aName) bList

                This _ ->
                    True

        isReferenceTo aName b =
            case b of
                That bName ->
                    aName == bName

                This _ ->
                    False
    in
    List.filter (not << containedInBList) aList


type alias Collection =
    { checkedPoints : Set String
    , checkedAxes : Set String
    , checkedCircles : Set String
    , checkedCurves : Set String
    , checkedDetails : Set String
    , checkedVariables : Set String
    , dependingPoints : Set String
    , dependingAxes : Set String
    , dependingCircles : Set String
    , dependingCurves : Set String
    , dependingDetails : Set String
    , dependingVariables : Set String
    }


emptyCollection : Collection
emptyCollection =
    { checkedPoints = Set.empty
    , checkedAxes = Set.empty
    , checkedCircles = Set.empty
    , checkedCurves = Set.empty
    , checkedDetails = Set.empty
    , checkedVariables = Set.empty
    , dependingPoints = Set.empty
    , dependingAxes = Set.empty
    , dependingCircles = Set.empty
    , dependingCurves = Set.empty
    , dependingDetails = Set.empty
    , dependingVariables = Set.empty
    }


objectsFromCollection : Collection -> Objects
objectsFromCollection collection =
    { points = List.map That (Set.toList collection.dependingPoints)
    , axes = List.map That (Set.toList collection.dependingAxes)
    , circles = List.map That (Set.toList collection.dependingCircles)
    , curves = List.map That (Set.toList collection.dependingCurves)
    , details = List.map That (Set.toList collection.dependingDetails)
    , variables = Set.toList collection.dependingVariables
    }


type alias Chains =
    { points : Set String
    , axes : Set String
    , circles : Set String
    , curves : Set String
    , details : Set String
    }


noChains : Chains
noChains =
    { points = Set.empty
    , axes = Set.empty
    , circles = Set.empty
    , curves = Set.empty
    , details = Set.empty
    }


collectObjectsDependingOnPoint :
    PatternData coordinates
    -> String
    -> Chains
    -> A Point
    -> State Collection ()
collectObjectsDependingOnPoint data pointName chains aPoint =
    case aPoint of
        That name_ ->
            let
                safeChainAsDependent state =
                    { state
                        | dependingPoints = Set.union chains.points state.dependingPoints
                    }

                addAsChecked state =
                    { state
                        | checkedPoints = Set.insert name_ state.checkedPoints
                    }
            in
            if name_ == pointName then
                State.modify safeChainAsDependent

            else
                State.get
                    |> State.andThen
                        (\state ->
                            if Set.member name_ state.checkedPoints then
                                State.state ()

                            else
                                case Dict.get name_ data.points of
                                    Nothing ->
                                        State.modify addAsChecked

                                    Just (Point_ info) ->
                                        collectObjectsDependingOnPointInfo data
                                            pointName
                                            { chains
                                                | points = Set.insert name_ chains.points
                                            }
                                            info
                        )

        This (Point_ info) ->
            collectObjectsDependingOnPointInfo data pointName chains info


collectObjectsDependingOnAxis :
    PatternData coordinates
    -> String
    -> Chains
    -> A Axis
    -> State Collection ()
collectObjectsDependingOnAxis data pointName chains aAxis =
    State.state ()


collectObjectsDependingOnCircle :
    PatternData coordinates
    -> String
    -> Chains
    -> A Circle
    -> State Collection ()
collectObjectsDependingOnCircle data pointName chains aCircle =
    State.state ()


collectObjectsDependingOnCurve :
    PatternData coordinates
    -> String
    -> Chains
    -> A Curve
    -> State Collection ()
collectObjectsDependingOnCurve data pointName chains aCurve =
    State.state ()


collectObjectsDependingOnDetail :
    PatternData coordinates
    -> String
    -> Chains
    -> A Detail
    -> State Collection ()
collectObjectsDependingOnDetail data pointName chains aDetail =
    State.state ()


collectObjectsDependingOnVariable :
    PatternData coordinates
    -> String
    -> Chains
    -> String
    -> State Collection ()
collectObjectsDependingOnVariable data pointName chains variable =
    State.state ()


collectObjectsDependingOnPointInfo :
    PatternData coordinates
    -> String
    -> Chains
    -> PointInfo
    -> State Collection ()
collectObjectsDependingOnPointInfo data pointName chains info =
    let
        and func =
            State.andThen (\_ -> func)
    in
    case info of
        Origin _ ->
            State.state ()

        FromOnePoint stuff ->
            collectObjectsDependingOnPoint data pointName chains stuff.basePoint
                |> and (collectObjectsDependingOnExpr data pointName chains stuff.distance)

        BetweenRatio stuff ->
            collectObjectsDependingOnPoint data pointName chains stuff.basePointA
                |> and (collectObjectsDependingOnPoint data pointName chains stuff.basePointB)
                |> and (collectObjectsDependingOnExpr data pointName chains stuff.ratio)

        BetweenLength stuff ->
            collectObjectsDependingOnPoint data pointName chains stuff.basePointA
                |> and (collectObjectsDependingOnPoint data pointName chains stuff.basePointB)
                |> and (collectObjectsDependingOnExpr data pointName chains stuff.distance)

        Intersection stuff ->
            State.state ()

        TransformedPoint stuff ->
            State.state ()


collectObjectsDependingOnExpr : PatternData coordinates -> String -> Chains -> String -> State Collection ()
collectObjectsDependingOnExpr data pointName chains expr =
    State.state ()



---- CONSTRUCT


{-| -}
origin : Float -> Float -> Point
origin x y =
    Point_ <|
        Origin
            { x = x
            , y = y
            }


{-| -}
fromOnePoint :
    A Point
    -> Direction
    -> String
    -> Pattern coordinates
    -> Result FromOnePointHelp Point
fromOnePoint aBasePoint direction distance pattern =
    Ok FromOnePointHelp
        |> collectMaybe (checkDirection pattern direction)
        |> collectMaybe (checkExpr pattern distance)
        |> Result.map
            (always <|
                Point_ <|
                    FromOnePoint
                        { basePoint = aBasePoint
                        , direction = direction
                        , distance = distance
                        }
            )


{-| -}
type alias FromOnePointHelp =
    { computeDirection : Maybe ComputeHelp
    , computeDistance : Maybe ComputeHelp
    }


{-| -}
betweenRatio : A Point -> A Point -> String -> Pattern coordinates -> Result BetweenRatioHelp Point
betweenRatio aBasePointA aBasePointB ratio pattern =
    Ok BetweenRatioHelp
        |> collectMaybe (checkExpr pattern ratio)
        |> collectBool (checkObjectsCoincidence aBasePointA aBasePointB)
        |> Result.map
            (always <|
                Point_ <|
                    BetweenRatio
                        { basePointA = aBasePointA
                        , basePointB = aBasePointB
                        , ratio = ratio
                        }
            )


{-| -}
type alias BetweenRatioHelp =
    { computeRatio : Maybe ComputeHelp
    , basePointsCoincide : Bool
    }


{-| -}
betweenLength :
    A Point
    -> A Point
    -> String
    -> OneInTwo
    -> Pattern coordinates
    -> Result BetweenLengthHelp Point
betweenLength aBasePointA aBasePointB distance from pattern =
    Ok BetweenLengthHelp
        |> collectMaybe (checkExpr pattern distance)
        |> collectBool (checkObjectsCoincidence aBasePointA aBasePointB)
        |> Result.map
            (always <|
                Point_ <|
                    BetweenLength
                        { basePointA = aBasePointA
                        , basePointB = aBasePointB
                        , distance = distance
                        , from = from
                        }
            )


{-| -}
type alias BetweenLengthHelp =
    { computeDistance : Maybe ComputeHelp
    , basePointsCoincide : Bool
    }


{-| -}
intersection :
    Intersectable
    -> Intersectable
    -> Int
    -> Pattern coordinates
    -> Result IntersectionHelp Point
intersection intersectableA intersectableB which pattern =
    Ok IntersectionHelp
        |> collectBool (checkIntersectablesCoincidence intersectableA intersectableB)
        |> collectBool (checkWhichInBound intersectableA intersectableB pattern which)
        |> Result.map
            (always <|
                Point_ <|
                    Intersection
                        { intersectableA = intersectableA
                        , intersectableB = intersectableB
                        , which = which
                        }
            )


checkWhichInBound : Intersectable -> Intersectable -> Pattern coordinates -> Int -> Bool
checkWhichInBound intersectableA intersectableB pattern which =
    0 >= which && which < whichSize (intersectableTag intersectableA) (intersectableTag intersectableB)


{-| -}
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
            1


{-| -}
type alias IntersectionHelp =
    { objectsCoincide : Bool
    , whichOutOfBound : Bool
    }


{-| -}
transformedPoint : A Point -> A Transformation -> Pattern coordinates -> Result () Point
transformedPoint aPoint aTransformation pattern =
    Err ()


{-| -}
throughOnePoint : A Point -> Orientation -> Pattern coordinates -> Result ThroughOnePointHelp Axis
throughOnePoint aPoint orientation pattern =
    Ok ThroughOnePointHelp
        |> collectMaybe (checkOrientation pattern orientation)
        |> Result.map
            (always <|
                Axis_ <|
                    ThroughOnePoint
                        { point = aPoint
                        , orientation = orientation
                        }
            )


{-| -}
type alias ThroughOnePointHelp =
    { computeAngle : Maybe ComputeHelp
    }


{-| -}
throughTwoPoints : A Point -> A Point -> Pattern coordinates -> Result ThroughTwoPointsHelp Axis
throughTwoPoints aPointA aPointB pattern =
    Ok ThroughTwoPointsHelp
        |> collectBool (checkObjectsCoincidence aPointA aPointB)
        |> Result.map
            (always <|
                Axis_ <|
                    ThroughTwoPoints
                        { pointA = aPointA
                        , pointB = aPointB
                        }
            )


{-| -}
type alias ThroughTwoPointsHelp =
    { basePointsCoincide : Bool
    }


{-| -}
transformedAxis : A Axis -> A Transformation -> Pattern coordinates -> Result () Axis
transformedAxis aAxis aTransformation pattern =
    Err ()


{-| -}
withRadius : String -> A Point -> Pattern coordinates -> Result WithRadiusHelp Circle
withRadius radius aCenterPoint pattern =
    Ok WithRadiusHelp
        |> collectMaybe (checkExpr pattern radius)
        |> Result.map
            (always <|
                Circle_ <|
                    WithRadius
                        { centerPoint = aCenterPoint
                        , radius = radius
                        }
            )


{-| -}
type alias WithRadiusHelp =
    { computeRadius : Maybe ComputeHelp
    }


{-| -}
throughThreePoints :
    A Point
    -> A Point
    -> A Point
    -> Pattern coordinates
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
                Circle_ <|
                    ThroughThreePoints
                        { pointA = aPointA
                        , pointB = aPointB
                        , pointC = aPointC
                        }
            )


{-| -}
type alias ThroughThreePointsHelp =
    { pointsCoincide : Bool
    }


{-| -}
transformedCircle : A Circle -> A Transformation -> Pattern coordinates -> Result () Circle
transformedCircle aCircle aTransformation pattern =
    Err ()


{-| -}
straight : A Point -> A Point -> Pattern coordinates -> Result StraightHelp Curve
straight aStartPoint aEndPoint pattern =
    Ok StraightHelp
        |> collectBool (checkObjectsCoincidence aStartPoint aEndPoint)
        |> Result.map
            (always <|
                Curve_ <|
                    Straight
                        { startPoint = aStartPoint
                        , endPoint = aEndPoint
                        }
            )


{-| -}
type alias StraightHelp =
    { pointsCoincide : Bool
    }


{-| -}
quadratic : A Point -> A Point -> A Point -> Pattern coordinates -> Result QuadraticHelp Curve
quadratic aStartPoint aControlPoint aEndPoint pattern =
    Ok QuadraticHelp
        |> collectBool (checkObjectsCoincidence aStartPoint aEndPoint)
        |> Result.map
            (always <|
                Curve_ <|
                    Quadratic
                        { startPoint = aStartPoint
                        , controlPoint = aControlPoint
                        , endPoint = aEndPoint
                        }
            )


{-| -}
type alias QuadraticHelp =
    { pointsCoincide : Bool
    }


{-| -}
cubic : A Point -> A Point -> A Point -> A Point -> Pattern coordinates -> Result CubicHelp Curve
cubic aStartPoint aStartControlPoint aEndControlPoint aEndPoint pattern =
    Ok CubicHelp
        |> collectBool (checkObjectsCoincidence aStartPoint aEndPoint)
        |> Result.map
            (always <|
                Curve_ <|
                    Cubic
                        { startPoint = aStartPoint
                        , startControlPoint = aStartControlPoint
                        , endControlPoint = aEndControlPoint
                        , endPoint = aEndPoint
                        }
            )


{-| -}
type alias CubicHelp =
    { pointsCoincide : Bool
    }


{-| -}
transformedCurve : A Curve -> A Transformation -> Pattern coordinates -> Result () Curve
transformedCurve aCurve aTransformation pattern =
    Err ()


{-| -}
detail : FirstCurve -> List NextCurve -> LastCurve -> Pattern coordinates -> Result DetailHelp Detail
detail firstCurve nextCurves lastCurve pattern =
    Ok <|
        Detail_
            { firstCurve = firstCurve
            , nextCurves = nextCurves
            , lastCurve = lastCurve
            }


{-| -}
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


{-| -}
type ExprHelp
    = SyntaxHelp (List DeadEnd)
    | UnknownFunction String
    | WrongArguments
        { function : String
        , args : List String
        }
    | CannotComputeFunction String


{-| -}
checkExpr : Pattern coordinates -> String -> Maybe ComputeHelp
checkExpr pattern rawExpr =
    case State.finalValue pattern (computeExpr rawExpr) of
        Ok _ ->
            Nothing

        Err computeHelp ->
            Just computeHelp


checkDirection : Pattern coordinates -> Direction -> Maybe ComputeHelp
checkDirection pattern direction =
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
            checkExpr pattern rawExpr


checkOrientation : Pattern coordinates -> Orientation -> Maybe ComputeHelp
checkOrientation pattern orientation =
    case orientation of
        Horizontal ->
            Nothing

        Vertical ->
            Nothing

        OrientationAngle rawAngle ->
            checkExpr pattern rawAngle


checkObjectsCoincidence : A object -> A object -> Bool
checkObjectsCoincidence aObjectA aObjectB =
    case ( aObjectA, aObjectB ) of
        ( That nameA, That nameB ) ->
            nameA == nameB

        _ ->
            False


checkIntersectablesCoincidence : Intersectable -> Intersectable -> Bool
checkIntersectablesCoincidence interectableA interectableB =
    case ( interectableA, interectableB ) of
        ( IntersectableAxis aAxisA, IntersectableAxis aAxisB ) ->
            checkObjectsCoincidence aAxisA aAxisB

        ( IntersectableCircle aCircleA, IntersectableCircle aCircleB ) ->
            checkObjectsCoincidence aCircleA aCircleB

        ( IntersectableCurve aCurveA, IntersectableCurve aCurveB ) ->
            checkObjectsCoincidence aCurveA aCurveB

        _ ->
            False



---- ENCODE


{-| -}
encode : Pattern coordinates -> Value
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
encodePoint (Point_ info) =
    case info of
        Origin stuff ->
            Encode.withType "origin"
                [ ( "x", Encode.float stuff.x )
                , ( "y", Encode.float stuff.y )
                ]

        FromOnePoint stuff ->
            Encode.withType "fromOnePoint"
                [ ( "basePoint", encodeAPoint stuff.basePoint )
                , ( "direction", encodeDirection stuff.direction )
                , ( "distance", Encode.string stuff.distance )
                ]

        BetweenRatio stuff ->
            Encode.withType "betweenRatio"
                [ ( "basePointA", encodeAPoint stuff.basePointA )
                , ( "basePointB", encodeAPoint stuff.basePointB )
                , ( "ratio", Encode.string stuff.ratio )
                ]

        BetweenLength stuff ->
            Encode.withType "betweenLength"
                [ ( "basePointA", encodeAPoint stuff.basePointA )
                , ( "basePointB", encodeAPoint stuff.basePointB )
                , ( "distance", Encode.string stuff.distance )
                , ( "from", encodeOneInTwo stuff.from )
                ]

        Intersection stuff ->
            Encode.withType "intersection"
                [ ( "intersectableA", encodeIntersectable stuff.intersectableA )
                , ( "intersectableB", encodeIntersectable stuff.intersectableB )
                , ( "which", Encode.int stuff.which )
                ]

        TransformedPoint stuff ->
            Encode.withType "transformedPoint"
                [ ( "point", encodeAPoint stuff.point )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeAxis : Axis -> Value
encodeAxis (Axis_ info) =
    case info of
        ThroughOnePoint stuff ->
            Encode.withType "throughOnePoint"
                [ ( "point", encodeAPoint stuff.point )
                , ( "orientation", encodeOrientation stuff.orientation )
                ]

        ThroughTwoPoints stuff ->
            Encode.withType "throughTwoPoints"
                [ ( "pointA", encodeAPoint stuff.pointA )
                , ( "pointB", encodeAPoint stuff.pointB )
                ]

        TransformedAxis stuff ->
            Encode.withType "transformedAxis"
                [ ( "axis", encodeAAxis stuff.axis )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeCircle : Circle -> Value
encodeCircle (Circle_ info) =
    case info of
        WithRadius stuff ->
            Encode.withType "withRadius"
                [ ( "centerPoint", encodeAPoint stuff.centerPoint )
                , ( "radius", Encode.string stuff.radius )
                ]

        ThroughThreePoints stuff ->
            Encode.withType "throughThreePoints"
                [ ( "pointA", encodeAPoint stuff.pointA )
                , ( "pointB", encodeAPoint stuff.pointB )
                , ( "pointC", encodeAPoint stuff.pointC )
                ]

        TransformedCircle stuff ->
            Encode.withType "transformedCircle"
                [ ( "circle", encodeACircle stuff.circle )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeCurve : Curve -> Value
encodeCurve (Curve_ info) =
    case info of
        Straight stuff ->
            Encode.withType "straight"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        Quadratic stuff ->
            Encode.withType "quadratic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "controlPoint", encodeAPoint stuff.controlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        Cubic stuff ->
            Encode.withType "cubic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        TransformedCurve stuff ->
            Encode.withType "transformedCurve"
                [ ( "curve", encodeACurve stuff.curve )
                , ( "transformation", encodeATransformation stuff.transformation )
                ]


encodeDetail : Detail -> Value
encodeDetail (Detail_ stuff) =
    Encode.object
        [ ( "firstCurve", encodeFirstCurve stuff.firstCurve )
        , ( "nextCurves", Encode.list encodeNextCurve stuff.nextCurves )
        , ( "lastCurve", encodeLastCurve stuff.lastCurve )
        ]


encodeFirstCurve : FirstCurve -> Value
encodeFirstCurve firstCurve =
    case firstCurve of
        FirstStraight stuff ->
            Encode.withType "firstStraight"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        FirstQuadratic stuff ->
            Encode.withType "firstQuadratic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "controlPoint", encodeAPoint stuff.controlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        FirstCubic stuff ->
            Encode.withType "firstCubic"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        FirstReferencedCurve stuff ->
            Encode.withType "firstReferencedCurve"
                [ ( "curve", encodeACurve stuff.curve )
                , ( "reversed", Encode.bool stuff.reversed )
                ]


encodeNextCurve : NextCurve -> Value
encodeNextCurve nextCurve =
    case nextCurve of
        NextStraight stuff ->
            Encode.withType "nextStraight"
                [ ( "endPoint", encodeAPoint stuff.endPoint ) ]

        NextQuadratic stuff ->
            Encode.withType "nextQuadratic"
                [ ( "controlPoint", encodeAPoint stuff.controlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        NextCubic stuff ->
            Encode.withType "nextCubic"
                [ ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        NextReferencedCurve stuff ->
            Encode.withType "nextReferencedCurve"
                [ ( "curve", encodeACurve stuff.curve )
                , ( "reversed", Encode.bool stuff.reversed )
                ]


encodeLastCurve : LastCurve -> Value
encodeLastCurve lastCurve =
    case lastCurve of
        LastStraight ->
            Encode.withType "lastStraight" []

        LastQuadratic stuff ->
            Encode.withType "lastQuadratic"
                [ ( "controlPoint", encodeAPoint stuff.controlPoint ) ]

        LastCubic stuff ->
            Encode.withType "lastCubic"
                [ ( "startControlPoint", encodeAPoint stuff.startControlPoint )
                , ( "endControlPoint", encodeAPoint stuff.endControlPoint )
                ]

        LastReferencedCurve stuff ->
            Encode.withType "lastReferencedCurve"
                [ ( "curve", encodeACurve stuff.curve )
                , ( "reversed", Encode.bool stuff.reversed )
                ]


encodeTransformation : Transformation -> Value
encodeTransformation (Transformation info) =
    case info of
        TranslateBy stuff ->
            Encode.withType "translateBy"
                [ ( "direction", encodeDirection stuff.direction )
                , ( "distance", Encode.string stuff.distance )
                ]

        TranslateFromTo stuff ->
            Encode.withType "translateFromTo"
                [ ( "startPoint", encodeAPoint stuff.startPoint )
                , ( "endPoint", encodeAPoint stuff.endPoint )
                ]

        RotateAround stuff ->
            Encode.withType "rotateAround"
                [ ( "centerPoint", encodeAPoint stuff.centerPoint )
                , ( "angle", Encode.string stuff.angle )
                ]


encodeIntersectable : Intersectable -> Value
encodeIntersectable intersectable =
    case intersectable of
        IntersectableAxis aAxis ->
            Encode.withType "intersectableAxis"
                [ ( "axis", encodeAAxis aAxis ) ]

        IntersectableCircle aCircle ->
            Encode.withType "intersectableCircle"
                [ ( "circle", encodeACircle aCircle ) ]

        IntersectableCurve aCurve ->
            Encode.withType "intersectableCurve"
                [ ( "curve", encodeACurve aCurve ) ]



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


encodeAObject : String -> (object -> Value) -> A object -> Value
encodeAObject objectType encodeObject aObject =
    case aObject of
        That name_ ->
            Encode.withType "that"
                [ ( "name", Encode.string name_ ) ]

        This object ->
            Encode.withType "this"
                [ ( objectType, encodeObject object ) ]



-- SHARED


encodeDirection : Direction -> Value
encodeDirection direction =
    case direction of
        Leftward ->
            Encode.withType "leftward" []

        Rightward ->
            Encode.withType "rightward" []

        Up ->
            Encode.withType "up" []

        Down ->
            Encode.withType "down" []

        DirectionAngle angle ->
            Encode.withType "directionAngle"
                [ ( "angle", Encode.string angle ) ]


encodeOrientation : Orientation -> Value
encodeOrientation orientation =
    case orientation of
        Horizontal ->
            Encode.withType "horizontal" []

        Vertical ->
            Encode.withType "vertical" []

        OrientationAngle angle ->
            Encode.withType "orientationAngle"
                [ ( "angle", Encode.string angle ) ]


encodeOneInTwo : OneInTwo -> Value
encodeOneInTwo oneInTwo =
    case oneInTwo of
        FirstInTwo ->
            Encode.withType "firstInTwo" []

        SecondInTwo ->
            Encode.withType "secondInTwo" []



---- DECODER


{-| -}
decoder : Decoder (Pattern coordinates)
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
            |> Decode.ensureType "origin"
        , Decode.succeed FromOnePointStuff
            |> Decode.required "basePoint" aPointDecoder
            |> Decode.required "direction" directionDecoder
            |> Decode.required "distance" Decode.string
            |> Decode.map FromOnePoint
            |> Decode.ensureType "fromOnePoint"
        , Decode.succeed BetweenRatioStuff
            |> Decode.required "basePointA" aPointDecoder
            |> Decode.required "basePointB" aPointDecoder
            |> Decode.required "ratio" Decode.string
            |> Decode.map BetweenRatio
            |> Decode.ensureType "betweenRatio"
        , Decode.succeed BetweenLengthStuff
            |> Decode.required "basePointA" aPointDecoder
            |> Decode.required "basePointB" aPointDecoder
            |> Decode.required "distance" Decode.string
            |> Decode.required "from" oneInTwoDecoder
            |> Decode.map BetweenLength
            |> Decode.ensureType "betweenLength"
        , Decode.succeed IntersectionStuff
            |> Decode.required "intersectableA" intersectableDecoder
            |> Decode.required "intersectableB" intersectableDecoder
            |> Decode.required "which" Decode.int
            |> Decode.map Intersection
            |> Decode.ensureType "intersection"
        ]
        |> Decode.map Point_


axisDecoder : Decoder Axis
axisDecoder =
    Decode.oneOf
        [ Decode.succeed ThroughOnePointStuff
            |> Decode.required "point" aPointDecoder
            |> Decode.required "orientation" orientationDecoder
            |> Decode.map ThroughOnePoint
            |> Decode.ensureType "throughOnePoint"
        , Decode.succeed ThroughTwoPointsStuff
            |> Decode.required "pointA" aPointDecoder
            |> Decode.required "pointB" aPointDecoder
            |> Decode.map ThroughTwoPoints
            |> Decode.ensureType "throughTwoPoints"
        , Decode.succeed TransformedAxisStuff
            |> Decode.required "axis" aAxisDecoder
            |> Decode.required "aTransformation" aTransformationDecoder
            |> Decode.map TransformedAxis
            |> Decode.ensureType "transformedAxis"
        ]
        |> Decode.map Axis_


circleDecoder : Decoder Circle
circleDecoder =
    Decode.oneOf
        [ Decode.succeed WithRadiusStuff
            |> Decode.required "centerPoint" aPointDecoder
            |> Decode.required "radius" Decode.string
            |> Decode.map WithRadius
            |> Decode.ensureType "withRadius"
        , Decode.succeed ThroughThreePointsStuff
            |> Decode.required "pointA" aPointDecoder
            |> Decode.required "pointB" aPointDecoder
            |> Decode.required "pointC" aPointDecoder
            |> Decode.map ThroughThreePoints
            |> Decode.ensureType "throughThreePoints"
        , Decode.succeed TransformedCircleStuff
            |> Decode.required "circle" aCircleDecoder
            |> Decode.required "transformation" aTransformationDecoder
            |> Decode.map TransformedCircle
            |> Decode.ensureType "transformedCircle"
        ]
        |> Decode.map Circle_


curveDecoder : Decoder Curve
curveDecoder =
    Decode.oneOf
        [ Decode.succeed StraightStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map Straight
            |> Decode.ensureType "straight"
        , Decode.succeed QuadraticStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map Quadratic
            |> Decode.ensureType "quadratic"
        , Decode.succeed CubicStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map Cubic
            |> Decode.ensureType "cubic"
        , Decode.succeed TransformedCurveStuff
            |> Decode.required "curve" aCurveDecoder
            |> Decode.required "transformation" aTransformationDecoder
            |> Decode.map TransformedCurve
            |> Decode.ensureType "transformedCurve"
        ]
        |> Decode.map Curve_


detailDecoder : Decoder Detail
detailDecoder =
    Decode.succeed DetailInfo
        |> Decode.required "firstCurve" firstCurveDecoder
        |> Decode.required "nextCurves" (Decode.list nextCurveDecoder)
        |> Decode.required "lastCurve" lastCurveDecoder
        |> Decode.map Detail_


firstCurveDecoder : Decoder FirstCurve
firstCurveDecoder =
    Decode.oneOf
        [ Decode.succeed FirstStraightStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map FirstStraight
            |> Decode.ensureType "firstStraight"
        , Decode.succeed FirstQuadraticStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map FirstQuadratic
            |> Decode.ensureType "firstQuadratic"
        , Decode.succeed FirstCubicStuff
            |> Decode.required "startPoint" aPointDecoder
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map FirstCubic
            |> Decode.ensureType "firstCubic"
        , Decode.succeed FirstReferencedCurveStuff
            |> Decode.required "curve" aCurveDecoder
            |> Decode.required "reversed" Decode.bool
            |> Decode.map FirstReferencedCurve
            |> Decode.ensureType "firstReferencedCurve"
        ]


nextCurveDecoder : Decoder NextCurve
nextCurveDecoder =
    Decode.oneOf
        [ Decode.succeed NextStraightStuff
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map NextStraight
            |> Decode.ensureType "nextStraight"
        , Decode.succeed NextQuadraticStuff
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map NextQuadratic
            |> Decode.ensureType "nextQuadratic"
        , Decode.succeed NextCubicStuff
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.required "endPoint" aPointDecoder
            |> Decode.map NextCubic
            |> Decode.ensureType "nextCubic"
        , Decode.succeed NextReferencedCurveStuff
            |> Decode.required "curve" aCurveDecoder
            |> Decode.required "reversed" Decode.bool
            |> Decode.map NextReferencedCurve
            |> Decode.ensureType "nextReferencedCurve"
        ]


lastCurveDecoder : Decoder LastCurve
lastCurveDecoder =
    Decode.oneOf
        [ Decode.succeed LastStraight
            |> Decode.ensureType "lastStraight"
        , Decode.succeed LastQuadraticStuff
            |> Decode.required "controlPoint" aPointDecoder
            |> Decode.map LastQuadratic
            |> Decode.ensureType "lastQuadratic"
        , Decode.succeed LastCubicStuff
            |> Decode.required "startControlPoint" aPointDecoder
            |> Decode.required "endControlPoint" aPointDecoder
            |> Decode.map LastCubic
            |> Decode.ensureType "lastCubic"
        , Decode.succeed LastReferencedCurveStuff
            |> Decode.required "curve" aCurveDecoder
            |> Decode.required "reversed" Decode.bool
            |> Decode.map LastReferencedCurve
            |> Decode.ensureType "lastReferencedCurve"
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
            |> Decode.required "axis" aAxisDecoder
            |> Decode.ensureType "intersectableAxis"
        , Decode.succeed IntersectableCircle
            |> Decode.required "circle" aCircleDecoder
            |> Decode.ensureType "intersectableCircle"
        , Decode.succeed IntersectableCurve
            |> Decode.required "curve" aCurveDecoder
            |> Decode.ensureType "intersectableCurve"
        ]



-- SHARED


directionDecoder : Decoder Direction
directionDecoder =
    Decode.oneOf
        [ Decode.succeed Leftward
            |> Decode.ensureType "leftward"
        , Decode.succeed Rightward
            |> Decode.ensureType "rightward"
        , Decode.succeed Up
            |> Decode.ensureType "up"
        , Decode.succeed Down
            |> Decode.ensureType "down"
        , Decode.succeed DirectionAngle
            |> Decode.required "angle" Decode.string
            |> Decode.ensureType "directionAngle"
        ]


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.oneOf
        [ Decode.succeed Horizontal
            |> Decode.ensureType "horizontal"
        , Decode.succeed Vertical
            |> Decode.ensureType "vertical"
        , Decode.succeed OrientationAngle
            |> Decode.required "angle" Decode.string
            |> Decode.ensureType "orientationAngle"
        ]


oneInTwoDecoder : Decoder OneInTwo
oneInTwoDecoder =
    Decode.oneOf
        [ Decode.succeed FirstInTwo
            |> Decode.ensureType "firstInTwo"
        , Decode.succeed SecondInTwo
            |> Decode.ensureType "secondInTwo"
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


aObjectDecoder : String -> Decoder object -> Decoder (A object)
aObjectDecoder objectType objectDecoder =
    Decode.oneOf
        [ Decode.succeed That
            |> Decode.required "name" Decode.string
            |> Decode.ensureType "that"
        , Decode.succeed This
            |> Decode.required objectType objectDecoder
            |> Decode.ensureType "this"
        ]
