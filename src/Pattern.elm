module Pattern exposing
    ( Pattern
    , empty
    , Point(..), points, insertPoint, getPoint, updatePoint
    , leftOf, rightOf, above, below
    , atAngle
    , betweenRatio, betweenLength
    , firstCircleCircle, secondCircleCircle
    , Circle(..), circles, insertCircle, getCircle
    , centeredAt
    , variables
    , lines
    , getLine
    , geometry, Geometry, Problems
    , insertVariable, removeVariable
    , insertLine
    , Detail(..), Length(..), Line(..), LineSegment(..), Transformation(..), computeLength, decoder, details, encode, exprFromFloat, getDetail, getLineSegment, getPointGeometries, getPointGeometry, insertDetail, insertLineSegment, insertTransformation, lastState, lineSegments, origin, point2d
    )

{-|

@docs Pattern

@docs empty


## Objects


# Points

@docs Point, points, insertPoint, getPoint, updatePoint

@docs leftOf, rightOf, above, below

@docs atAngle

@docs betweenRatio, betweenLength

@docs firstCircleCircle, secondCircleCircle


# Circles

@docs Circle, circles, insertCircle, getCircle

@docs centeredAt


# Read

@docs variables

@docs lines

@docs getLine

@docs geometry, Geometry, Problems


# Modify

@docs insertVariable, removeVariable, renameVariable

@docs replacePoint, removePoint

@docs replaceCircle, removeCircle

@docs insertLine, replaceLine, removeLine

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
import Those exposing (Those)
import Vector2d


type Pattern
    = Pattern PatternData


type alias PatternData =
    { points : Store Point
    , circles : Store Circle
    , lines : Store Line
    , lineSegments : Store LineSegment
    , details : Store Detail
    , variables : Dict String String
    , transformations : Transformations
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
        , transformations = noTransformations
        }



---- OBJECTS


type Point
    = Origin Float Float
    | LeftOf (That Point) String
    | RightOf (That Point) String
    | Above (That Point) String
    | Below (That Point) String
    | AtAngle (That Point) String String
    | BetweenRatio (That Point) (That Point) String
    | BetweenLength (That Point) (That Point) String
      -- ON OBJECT
    | OnLineThatX (That Line) Coordinate
    | OnLineThatY (That Line) Coordinate
    | OnCurve (That Curve) Constraint
    | OnLineSegment (That LineSegment) Constraint
    | OnCircle (That Circle) Constraint
    | OnCircleFirstTangent (That Circle) (That Point)
    | OnCircleSecondTangent (That Circle) (That Point)
    | OnCircleFirstChord (That Circle) Angle
    | OnCircleSecondChord (That Circle) Angle
      -- BY INTERSECTION
    | FirstCircleCircle (That Circle) (That Circle)
    | SecondCircleCircle (That Circle) (That Circle)
    | LineLine (That Line) (That Line)
    | FirstCircleLine (That Circle) (That Line)
    | SecondCircleLine (That Circle) (That Line)
      -- BY TRANSFORMATION
    | Transformed (That Point) Transformation


type Constraint
    = X Coordinate
    | Y Coordinate
    | MinX
    | MaxX
    | MinY
    | MaxY


type Line
    = ThroughOnePoint (That Point) Angle
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
    = CounterClockwise (List (That Point))



---- TRANSFORMATIONS


type Transformation
    = MirrorAt (That Line) (Those Point)
    | RotateAround (That Point) Angle (Those Point)
    | CutAlong (That Line) (That Detail)
    | CutAlongLineSegment (That LineSegment) (That Detail)
    | CutCurve (That Curve) (That Detail)


applyTransformationToPoint2d : Pattern -> Transformation -> Point2d -> Point2d
applyTransformationToPoint2d pattern transformation previousPoint2d =
    case transformation of
        MirrorAt thatLine _ ->
            case axis2d pattern thatLine of
                Just axis ->
                    previousPoint2d
                        |> Point2d.mirrorAcross axis

                Nothing ->
                    previousPoint2d

        _ ->
            previousPoint2d


addTransformationToPoint :
    Pattern
    -> Int
    -> Transformation
    -> ( That Point, Entry Point )
    -> List ( That Point, Entry Point )
addTransformationToPoint p transformationId transformation ( previousThat, entry ) =
    case transformation of
        MirrorAt thatLine thosePoints ->
            if Those.member previousThat thosePoints then
                [ addChange transformationId Nothing previousThat entry ]

            else
                [ ( previousThat, entry ) ]

        CutAlongLineSegment thatLineSegment _ ->
            case getLineSegment p thatLineSegment of
                Nothing ->
                    [ ( previousThat, entry ) ]

                Just lineSegment ->
                    let
                        thatPointA =
                            case lineSegment.value of
                                FromTo from _ ->
                                    from

                        thatPointB =
                            case lineSegment.value of
                                FromTo _ to ->
                                    to
                    in
                    if That.areEqual previousThat thatPointA then
                        [ addChange transformationId (Just 0) previousThat entry ]

                    else if That.areEqual previousThat thatPointB then
                        [ addChange transformationId (Just 1) previousThat entry ]

                    else
                        [ ( previousThat, entry ) ]

        _ ->
            [ ( previousThat, entry ) ]


addTransformationToDetail :
    Pattern
    -> Int
    -> Transformation
    -> ( That Detail, Entry Detail )
    -> List ( That Detail, Entry Detail )
addTransformationToDetail p transformationId transformation ( previousThat, entry ) =
    case transformation of
        CutAlongLineSegment thatLineSegment thatDetail ->
            case ( getDetail p thatDetail, getLineSegment p thatLineSegment ) of
                ( Just detail, Just lineSegment ) ->
                    let
                        thatPointA =
                            case lineSegment.value of
                                FromTo from _ ->
                                    from

                        thatPointB =
                            case lineSegment.value of
                                FromTo _ to ->
                                    to

                        targets =
                            case detail.value of
                                CounterClockwise thatPoints ->
                                    Those.fromList thatPoints
                    in
                    if
                        Those.member thatPointA targets
                            && Those.member thatPointB targets
                    then
                        [ addChange transformationId (Just 0) previousThat entry
                        , addChange transformationId (Just 1) previousThat entry
                        ]

                    else
                        [ ( previousThat, entry ) ]

                _ ->
                    [ ( previousThat, entry ) ]

        _ ->
            [ ( previousThat, entry ) ]



---- EXPRESSIONS


compute : Pattern -> Expr -> Result DoesNotCompute Float
compute (Pattern pattern) expr =
    let
        functions name args =
            Nothing

        parsedVars =
            pattern.variables
                |> Dict.toList
                |> List.filterMap
                    (\( name, string ) ->
                        case Expr.parse [] string of
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


exprFromFloat : Float -> Expr
exprFromFloat f =
    Number f



----


type Angle
    = Angle Expr


type Coordinate
    = Coordinate Expr


type Length
    = Length Expr


computeLength : Pattern -> Length -> Maybe Float
computeLength pattern (Length expr) =
    compute pattern expr
        |> Result.toMaybe


type Ratio
    = Ratio Expr



---- TRANSFORMATIONS


type alias Transformations =
    { entries : List ( Int, Transformation )
    , nextId : Int
    }


noTransformations : Transformations
noTransformations =
    { entries = []
    , nextId = 0
    }


type State
    = State Int


start : State
start =
    State -1


append : Transformation -> Transformations -> Transformations
append newTransformation ts =
    { ts
        | entries = ( ts.nextId, newTransformation ) :: ts.entries
        , nextId = ts.nextId + 1
    }


lastState : Pattern -> State
lastState (Pattern { transformations }) =
    transformations.entries
        |> List.head
        |> Maybe.map (Tuple.first >> State)
        |> Maybe.withDefault start


last : Transformations -> Maybe ( State, Transformation )
last { entries } =
    entries
        |> List.head
        |> Maybe.map (Tuple.mapFirst State)


allUntil : State -> Transformations -> List ( State, Transformation )
allUntil (State id) { entries } =
    if id == -1 then
        []

    else
        allUntilHelp [] False id entries


allUntilHelp :
    List ( State, Transformation )
    -> Bool
    -> Int
    -> List ( Int, Transformation )
    -> List ( State, Transformation )
allUntilHelp sum found id entries =
    case entries of
        [] ->
            if found then
                sum

            else
                []

        ( nextId, entry ) :: rest ->
            if nextId == id then
                ( State nextId, entry ) :: sum

            else
                allUntilHelp (( State nextId, entry ) :: sum) found id rest


firstToLast : Transformations -> List ( State, Transformation )
firstToLast { entries } =
    List.foldl
        (\( id, entry ) sum -> ( State id, entry ) :: sum)
        []
        entries


lastToFirst : Transformations -> List ( State, Transformation )
lastToFirst { entries } =
    List.map (Tuple.mapFirst State) entries


getTransformation : Pattern -> Int -> Maybe Transformation
getTransformation (Pattern pattern) id =
    pattern.transformations.entries
        |> List.filterMap
            (\( nextId, transformation ) ->
                if nextId == id then
                    Just transformation

                else
                    Nothing
            )
        |> List.head



---- GEOMETRY


type alias Geometry =
    { points : List ( That Point, Maybe String, Point2d )
    , circles : List ( That Circle, Maybe String, Circle2d )
    , lines : List ( That Line, Maybe String, Axis2d )
    , lineSegments : List ( That LineSegment, Maybe String, LineSegment2d )
    , details : List ( That Detail, Maybe String, Polygon2d )
    }


type alias Problems =
    { doNotCompute : List ( String, DoesNotCompute )
    , missingPoints : List (That Point)
    , missingCircles : List (That Circle)
    , missingLines : List (That Line)
    }


geometry : Pattern -> ( Geometry, Problems )
geometry pattern =
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
                |> List.map (\p2d -> ( thatDetail, name, p2d ))
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
                |> List.map geometryDetail
                |> List.concat
      }
    , { doNotCompute = []
      , missingPoints = []
      , missingCircles = []
      , missingLines = []
      }
    )


point2d : Pattern -> That Point -> Maybe Point2d
point2d pattern thatPoint =
    let
        simpleDistance anchor rawLength toDirection =
            case Expr.parse [] rawLength of
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
            thatPoint
                |> That.changes
                |> List.filterMap (.transformationId >> getTransformation pattern)
                |> List.foldr (applyTransformationToPoint2d pattern) point
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
                    (Result.toMaybe (Expr.parse [] rawAngle))
                    (Result.toMaybe (Expr.parse [] rawDistance))
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
                        |> Expr.parse []
                        |> Result.toMaybe
                        |> Maybe.andThen (compute pattern >> Result.toMaybe)
                    )

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

            _ ->
                Nothing


circle2d : Pattern -> That Circle -> Maybe Circle2d
circle2d pattern thatCircle =
    case Maybe.map .value (getCircle pattern thatCircle) of
        Just (CenteredAt thatCenter rawRadius) ->
            Maybe.map2 Circle2d.withRadius
                (rawRadius
                    |> Expr.parse []
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

        _ ->
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


type Location
    = InDetail
    | OutsideDetail
    | Unknown (List (That Point))


polygon2d : Pattern -> That Detail -> List Polygon2d
polygon2d ((Pattern pattern) as p) thatDetail =
    let
        transformations =
            List.filterMap
                (\{ transformationId, branch } ->
                    Maybe.map2 Tuple.pair
                        (getTransformation p transformationId)
                        branch
                )
                (That.changes thatDetail)

        applyTransformations detail =
            case detail of
                Just (CounterClockwise targets) ->
                    List.foldr applyTransformation [ targets ] transformations

                _ ->
                    []

        applyTransformation ( transformation, branch ) targetsList =
            case transformation of
                CutAlongLineSegment thatLineSegment _ ->
                    case getLineSegment p thatLineSegment of
                        Nothing ->
                            targetsList

                        Just lineSegment ->
                            let
                                maybePoints =
                                    if branch == 0 then
                                        Just
                                            ( case lineSegment.value of
                                                FromTo from _ ->
                                                    from
                                            , case lineSegment.value of
                                                FromTo _ to ->
                                                    to
                                            )

                                    else if branch == 1 then
                                        Just
                                            ( case lineSegment.value of
                                                FromTo _ to ->
                                                    to
                                            , case lineSegment.value of
                                                FromTo from _ ->
                                                    from
                                            )

                                    else
                                        Nothing
                            in
                            case maybePoints of
                                Nothing ->
                                    targetsList

                                Just ( startPoint, endPoint ) ->
                                    targetsList
                                        |> List.map
                                            (List.foldl
                                                (\target ( targetsDetail, location ) ->
                                                    case location of
                                                        Unknown targetsUnknown ->
                                                            if That.areEqual target startPoint then
                                                                ( target :: targetsUnknown
                                                                , InDetail
                                                                )

                                                            else if That.areEqual target endPoint then
                                                                ( targetsUnknown ++ [ target ]
                                                                , OutsideDetail
                                                                )

                                                            else
                                                                ( []
                                                                , Unknown (target :: targetsUnknown)
                                                                )

                                                        InDetail ->
                                                            if That.areEqual target endPoint then
                                                                ( target :: targetsDetail
                                                                , OutsideDetail
                                                                )

                                                            else
                                                                ( target :: targetsDetail
                                                                , InDetail
                                                                )

                                                        OutsideDetail ->
                                                            if That.areEqual target startPoint then
                                                                ( target :: targetsDetail
                                                                , InDetail
                                                                )

                                                            else
                                                                ( targetsDetail
                                                                , OutsideDetail
                                                                )
                                                )
                                                ( [], Unknown [] )
                                                >> Tuple.first
                                            )

                _ ->
                    targetsList
    in
    thatDetail
        |> getDetail p
        |> Maybe.map .value
        |> applyTransformations
        |> List.map
            (\targets ->
                targets
                    |> List.filterMap (point2d p)
                    |> Polygon2d.singleLoop
            )



---- VARIABLES


variables : Pattern -> List { name : String, value : String, computed : Float }
variables ((Pattern data) as pattern) =
    data.variables
        |> Dict.toList
        |> List.filterMap
            (\( name, value ) ->
                value
                    |> Expr.parse []
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
    case Expr.parse [] value of
        Err _ ->
            pattern

        Ok _ ->
            Pattern { data | variables = Dict.insert name value data.variables }


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
    case Expr.parse [] rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (LeftOf thatPoint rawLength)

            else
                Nothing


rightOf : Pattern -> That Point -> String -> Maybe Point
rightOf (Pattern pattern) thatPoint rawLength =
    case Expr.parse [] rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (RightOf thatPoint rawLength)

            else
                Nothing


above : Pattern -> That Point -> String -> Maybe Point
above (Pattern pattern) thatPoint rawLength =
    case Expr.parse [] rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (Above thatPoint rawLength)

            else
                Nothing


below : Pattern -> That Point -> String -> Maybe Point
below (Pattern pattern) thatPoint rawLength =
    case Expr.parse [] rawLength of
        Err _ ->
            Nothing

        Ok _ ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (Below thatPoint rawLength)

            else
                Nothing


atAngle : Pattern -> That Point -> String -> String -> Maybe Point
atAngle (Pattern pattern) thatPoint rawAngle rawDistance =
    case ( Expr.parse [] rawAngle, Expr.parse [] rawDistance ) of
        ( Ok _, Ok _ ) ->
            if Store.member pattern.points (That.objectId thatPoint) then
                Just (AtAngle thatPoint rawAngle rawDistance)

            else
                Nothing

        _ ->
            Nothing


betweenRatio : Pattern -> That Point -> That Point -> String -> Maybe Point
betweenRatio (Pattern pattern) thatAnchorA thatAnchorB rawRatio =
    case Expr.parse [] rawRatio of
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
    case Expr.parse [] rawLength of
        Ok _ ->
            if
                Store.member pattern.points (That.objectId thatAnchorA)
                    && Store.member pattern.points (That.objectId thatAnchorB)
            then
                Just (BetweenRatio thatAnchorA thatAnchorB rawLength)

            else
                Nothing

        Err _ ->
            Nothing


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


centeredAt : Pattern -> That Point -> String -> Maybe Circle
centeredAt (Pattern pattern) thatCenter rawRadius =
    case Expr.parse [] rawRadius of
        Ok _ ->
            if Store.member pattern.points (That.objectId thatCenter) then
                Just (CenteredAt thatCenter rawRadius)

            else
                Nothing

        Err _ ->
            Nothing



--


points : Pattern -> List ( That Point, Entry Point )
points =
    objects .points addTransformationToPoint


getPoint : Pattern -> That Point -> Maybe (Entry Point)
getPoint (Pattern pattern) =
    Store.get pattern.points << That.objectId


updatePoint : That Point -> Point -> Pattern -> Pattern
updatePoint thatPoint point (Pattern pattern) =
    Pattern
        { pattern
            | points =
                Store.updateValue (That.objectId thatPoint) point pattern.points
        }


getPointGeometry : Pattern -> That Point -> Maybe Point2d
getPointGeometry pattern thatPoint =
    point2d pattern thatPoint


getPointGeometries : Pattern -> That Point -> List Point2d
getPointGeometries pattern thatPoint =
    getPointGeometriesHelp [] pattern thatPoint


getPointGeometriesHelp : List (Maybe Point2d) -> Pattern -> That Point -> List Point2d
getPointGeometriesHelp geometries pattern thatPoint =
    if List.isEmpty (That.changes thatPoint) then
        point2d pattern thatPoint
            :: geometries
            |> List.filterMap identity

    else
        getPointGeometriesHelp
            (point2d pattern thatPoint :: geometries)
            pattern
            (That.dropChanges 1 thatPoint)


insertPoint : Maybe String -> Point -> Pattern -> Pattern
insertPoint name point (Pattern pattern) =
    Pattern
        { pattern | points = Store.insert name point pattern.points }



---- CIRCLES


circles : Pattern -> List ( That Circle, Entry Circle )
circles ((Pattern pattern) as p) =
    pattern.circles
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatCircleFromId p))


thatCircleFromId : Pattern -> Int -> That Circle
thatCircleFromId (Pattern pattern) id =
    that [] id


getCircle : Pattern -> That Circle -> Maybe (Entry Circle)
getCircle (Pattern pattern) =
    Store.get pattern.circles << That.objectId


insertCircle : Maybe String -> Circle -> Pattern -> Pattern
insertCircle name circle (Pattern pattern) =
    Pattern
        { pattern | circles = Store.insert name circle pattern.circles }



---- LINES


lines : Pattern -> List ( That Line, Entry Line )
lines ((Pattern pattern) as p) =
    pattern.lines
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatLineFromId p))


thatLineFromId : Pattern -> Int -> That Line
thatLineFromId (Pattern pattern) id =
    that [] id


getLine : Pattern -> That Line -> Maybe (Entry Line)
getLine (Pattern pattern) =
    Store.get pattern.lines << That.objectId


insertLine : Maybe String -> Line -> Pattern -> Pattern
insertLine maybeName line (Pattern pattern) =
    Pattern
        { pattern | lines = Store.insert maybeName line pattern.lines }



---- LINE SEGMENTS


lineSegments : Pattern -> List ( That LineSegment, Entry LineSegment )
lineSegments ((Pattern pattern) as p) =
    pattern.lineSegments
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatLineSegmentFromId p))


thatLineSegmentFromId : Pattern -> Int -> That LineSegment
thatLineSegmentFromId (Pattern pattern) id =
    that [] id


getLineSegment : Pattern -> That LineSegment -> Maybe (Entry LineSegment)
getLineSegment (Pattern pattern) =
    Store.get pattern.lineSegments << That.objectId


insertLineSegment : Maybe String -> LineSegment -> Pattern -> Pattern
insertLineSegment maybeName lineSegment (Pattern pattern) =
    Pattern
        { pattern | lineSegments = Store.insert maybeName lineSegment pattern.lineSegments }



---- TRANSFORMATIONS


insertTransformation : Transformation -> Pattern -> Pattern
insertTransformation transformation (Pattern pattern) =
    Pattern
        { pattern | transformations = append transformation pattern.transformations }



---- DETAILS


details : Pattern -> List ( That Detail, Entry Detail )
details pattern =
    objects .details addTransformationToDetail pattern


insertDetail : Detail -> Pattern -> Pattern
insertDetail detail (Pattern pattern) =
    Pattern
        { pattern | details = Store.insert Nothing detail pattern.details }


getDetail : Pattern -> That Detail -> Maybe (Entry Detail)
getDetail (Pattern pattern) =
    Store.get pattern.details << That.objectId



---- OBJECTS


objects :
    (PatternData -> Store a)
    -> (Pattern -> Int -> Transformation -> ( That a, Entry a ) -> List ( That a, Entry a ))
    -> Pattern
    -> List ( That a, Entry a )
objects objectsAccessor addTransformationToObject ((Pattern pattern) as p) =
    let
        apply transformations thats =
            case transformations of
                [] ->
                    thats

                ( transformationId, transformation ) :: rest ->
                    thats
                        |> List.map (addTransformationToObject p transformationId transformation)
                        |> List.concat
                        |> apply rest
    in
    pattern
        |> objectsAccessor
        |> Store.toList
        |> List.map (Tuple.mapFirst (that []))
        |> apply pattern.transformations.entries


addChange : Int -> Maybe Int -> That a -> Entry a -> ( That a, Entry a )
addChange transformationId branch previousThat entry =
    ( that
        ({ transformationId = transformationId
         , branch = branch
         }
            :: That.changes previousThat
        )
        (That.objectId previousThat)
    , entry
    )



---- ENCODER


encode : Pattern -> Value
encode (Pattern pattern) =
    Encode.object
        [ ( "points", Store.encode encodePoint pattern.points )
        , ( "circles", Store.encode encodeCircle pattern.circles )
        , ( "lines", Store.encode encodeLine pattern.lines )
        , ( "lineSegments", Store.encode encodeLineSegment pattern.lineSegments )
        , ( "details", Store.encode encodeDetail pattern.details )
        , ( "transformations", encodeTransformations pattern.transformations )
        , ( "variables", encodeVariables pattern.variables )
        ]


encodeTransformations : Transformations -> Value
encodeTransformations { entries, nextId } =
    let
        encodeEntry ( id, transformation ) =
            Encode.object
                [ ( "id", Encode.int id )
                , ( "transformation", encodeTransformation transformation )
                ]
    in
    Encode.object
        [ ( "entries"
          , Encode.list encodeEntry entries
          )
        , ( "nextId", Encode.int nextId )
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

        _ ->
            Encode.null


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
    case detail of
        CounterClockwise thatPoints ->
            withType "counterClockwise"
                [ ( "points", Encode.list That.encode thatPoints ) ]


encodeTransformation : Transformation -> Value
encodeTransformation transformation =
    case transformation of
        MirrorAt line targets ->
            withType "mirrorAt"
                [ ( "line", That.encode line )
                , ( "targets", Those.encode targets )
                ]

        CutAlongLineSegment lineSegment detail ->
            withType "cutAlongLineSegment"
                [ ( "lineSegment", That.encode lineSegment )
                , ( "detail", That.encode detail )
                ]

        _ ->
            Encode.null


encodeAngle : Angle -> Value
encodeAngle (Angle expr) =
    Encode.object
        [ ( "angle"
          , encodeExpr expr
          )
        ]


encodeLength : Length -> Value
encodeLength (Length expr) =
    Encode.object
        [ ( "length"
          , encodeExpr expr
          )
        ]


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
        |> Decode.required "variables" variablesDecoder
        |> Decode.required "transformations" transformationsDecoder
        |> Decode.map Pattern


transformationsDecoder : Decoder Transformations
transformationsDecoder =
    let
        entryDecoder =
            Decode.succeed Tuple.pair
                |> Decode.required "id" Decode.int
                |> Decode.required "transformation" transformationDecoder
    in
    Decode.succeed Transformations
        |> Decode.required "entries" (Decode.list entryDecoder)
        |> Decode.required "nextId" Decode.int


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
    Decode.oneOf
        [ typeDecoder "counterClockwise" <|
            Decode.map CounterClockwise
                (Decode.field "points" (Decode.list That.decoder))
        ]


transformationDecoder : Decoder Transformation
transformationDecoder =
    Decode.oneOf
        [ typeDecoder "mirrorAt" <|
            Decode.map2 MirrorAt
                (Decode.field "line" That.decoder)
                (Decode.field "targets" Those.decoder)
        , typeDecoder "cutAlongLineSegment" <|
            Decode.map2 CutAlongLineSegment
                (Decode.field "lineSegment" That.decoder)
                (Decode.field "detail" That.decoder)
        ]


lengthDecoder : Decoder Length
lengthDecoder =
    Decode.map Length <|
        Decode.field "length" exprDecoder


angleDecoder : Decoder Angle
angleDecoder =
    Decode.map Angle <|
        Decode.field "angle" exprDecoder


exprDecoder : Decoder Expr
exprDecoder =
    Decode.oneOf
        [ typeDecoder "number" <|
            Decode.map Number (Decode.field "value" Decode.float)
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
