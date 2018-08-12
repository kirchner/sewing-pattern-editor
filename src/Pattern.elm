module Pattern
    exposing
        ( Detail(..)
        , Geometry
        , Length(..)
        , Line(..)
        , Pattern
        , Point(..)
        , Problems
        , Transformation(..)
        , Version
        , circles
        , empty
        , exprFromFloat
        , geometry
        , getCircle
        , getLine
        , getPoint
        , getPointVersion
        , insertCircle
        , insertDetail
        , insertLine
        , insertPoint
        , insertTransformation
        , lines
        , points
        , removeCircle
        , removeLine
        , removePoint
        , replaceCircle
        , replaceLine
        , replacePoint
        , variables
        )

{-|

@docs Pattern

@docs empty


# Read

@docs points, circles, lines, variables

@docs getPoint, getCircle, getLine

@docs geometry, Geometry, Problems


# Modify

@docs insertVariable, removeVariable, renameVariable

@docs insertPoint, replacePoint, removePoint

@docs insertCircle, replaceCircle, removeCircle

@docs insertLine, replaceLine, removeLine

-}

import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Direction2d
import Parser
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Those exposing (That, These, Those)
import Vector2d


type Pattern
    = Pattern
        { points : Those Point
        , circles : Those Circle
        , lines : Those Line
        , variables : Dict String Expr
        , details : Those Detail
        , transformations : Those Transformation
        }


empty : Pattern
empty =
    Pattern
        { points = Those.empty
        , circles = Those.empty
        , lines = Those.empty
        , variables = Dict.empty
        , details = Those.empty
        , transformations = Those.empty
        }


type alias Geometry =
    { points : List ( That Point, Maybe String, Point2d )
    , circles : List ( That Circle, Maybe String, Circle2d )
    , lines : List ( That Line, Maybe String, Axis2d )
    , details : List ( That Detail, Maybe String, Polygon2d )
    }


type alias Problems =
    { doNotCompute : List ( String, DoesNotCompute )
    , missingPoints : List (That Point)
    , missingCircles : List (That Circle)
    , missingLines : List (That Line)
    }


geometry : Pattern -> ( Geometry, Problems )
geometry ((Pattern pattern) as p) =
    let
        geometryPoint ( thatPoint, { name } ) =
            point2d p thatPoint
                |> Maybe.map (\p2d -> ( thatPoint, name, p2d ))

        geometryLine ( thatLine, { name } ) =
            axis2d p thatLine
                |> Maybe.map (\a2d -> ( thatLine, name, a2d ))

        geometryDetail ( thatDetail, { name } ) =
            polygon2d p thatDetail
                |> Maybe.map (\p2d -> ( thatDetail, name, p2d ))
    in
    ( { points =
            pattern.points
                |> Those.toList
                |> List.filterMap geometryPoint
      , circles = []
      , lines =
            pattern.lines
                |> Those.toList
                |> List.filterMap geometryLine
      , details =
            pattern.details
                |> Those.toList
                |> List.filterMap geometryDetail
      }
    , { doNotCompute = []
      , missingPoints = []
      , missingCircles = []
      , missingLines = []
      }
    )


point2d : Pattern -> That Point -> Maybe Point2d
point2d ((Pattern pattern) as p) thatPoint =
    let
        simpleDistance anchor expr toDirection =
            case compute Dict.empty expr of
                Err _ ->
                    Nothing

                Ok distance ->
                    let
                        v =
                            toDirection distance
                    in
                    anchor
                        |> point2d p
                        |> Maybe.map (Point2d.translateBy v)

        transformations =
            pattern.transformations
                |> Those.values
                |> List.filter targetPoint

        targetPoint { a } =
            case a of
                MirrorAt _ targets ->
                    Those.memberOfThese thatPoint targets

                _ ->
                    Debug.todo ""

        applyTransformations point =
            List.foldl applyTransformation point transformations

        applyTransformation { a } point =
            case a of
                MirrorAt line _ ->
                    case axis2d p line of
                        Just axis ->
                            point
                                |> Point2d.mirrorAcross axis

                        Nothing ->
                            point

                _ ->
                    Debug.todo ""
    in
    Maybe.map applyTransformations <|
        case getPoint p thatPoint of
            Just Origin ->
                Just Point2d.origin

            Just (LeftOf anchor (Length expr)) ->
                simpleDistance anchor expr <|
                    \distance -> Vector2d.fromComponents ( -1 * distance, 0 )

            Just (RightOf anchor (Length expr)) ->
                simpleDistance anchor expr <|
                    \distance -> Vector2d.fromComponents ( distance, 0 )

            Just (Above anchor (Length expr)) ->
                simpleDistance anchor expr <|
                    \distance -> Vector2d.fromComponents ( 0, -1 * distance )

            Just (Below anchor (Length expr)) ->
                simpleDistance anchor expr <|
                    \distance -> Vector2d.fromComponents ( 0, distance )

            _ ->
                Nothing


axis2d : Pattern -> That Line -> Maybe Axis2d
axis2d ((Pattern _) as p) thatLine =
    case getLine p thatLine of
        Just (ThroughTwoPoints thatPointA thatPointB) ->
            case ( point2d p thatPointA, point2d p thatPointB ) of
                ( Just point2dA, Just point2dB ) ->
                    Direction2d.from point2dA point2dB
                        |> Maybe.map (Axis2d.through point2dA)

                _ ->
                    Nothing

        _ ->
            Nothing


polygon2d : Pattern -> That Detail -> Maybe Polygon2d
polygon2d ((Pattern _) as p) thatDetail =
    case getDetail p thatDetail of
        Just (CounterClockwise targets) ->
            targets
                |> List.filterMap (point2d p)
                |> Polygon2d.singleLoop
                |> Just

        _ ->
            Nothing



---- EXPRESSIONS


type Expr
    = Number Float
    | Symbol String
    | Measurement Measurement
    | Sum Expr Expr
    | Difference Expr Expr
    | Product Expr Expr
    | Quotient Expr Expr


type Measurement
    = Distance (That Point) (That Point)
    | AngleOfLine (That Line)
    | AngleBetween (That Point) (That Point) (That Point)


parse : String -> Result Parser.DeadEnd Expr
parse =
    Debug.todo ""


compute : Dict String Expr -> Expr -> Result DoesNotCompute Float
compute vars expr =
    case expr of
        Number f ->
            Ok f

        _ ->
            Debug.todo ""


type DoesNotCompute
    = MissingVariable String
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


type Ratio
    = Ratio Expr



----


type Construction
    = Point Point
    | Line Line
    | LineSegment LineSegment
    | Curve Curve
    | Circle Circle
    | Ellipsis Ellipsis
    | Detail Detail


type Transformation
    = MirrorAt (That Line) (These Point)
    | RotateAround (That Point) Angle (These Point)
    | CutAlong (That Line) Detail
    | CutLineSegment (That LineSegment) Detail
    | CutCurve (That Curve) Detail



---- ELEMENTS


type Point
    = Origin
    | LeftOf (That Point) Length
    | RightOf (That Point) Length
    | Above (That Point) Length
    | Below (That Point) Length
    | AtAngle (That Point) Angle Length
    | BetweenRatio (That Point) (That Point) Ratio
    | BetweenLength (That Point) (That Point) Length
      -- ON OBJECT
    | OnLineAtX (That Line) Coordinate
    | OnLineAtY (That Line) Coordinate
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


type State
    = Initial
    | After (That Transformation)


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
    = CenteredAt (That Point) Length


type Curve
    = TODOCurve


type Ellipsis
    = TODOEllipsis


type Detail
    = CounterClockwise (List (That Point))



---- VARIABLES


variables : Pattern -> List ( String, Expr, Float )
variables =
    Debug.todo ""


insertVariable : String -> Expr -> Pattern -> Pattern
insertVariable =
    Debug.todo ""


removeVariable : String -> Pattern -> Pattern
removeVariable =
    Debug.todo ""


renameVariable : String -> String -> Pattern -> Pattern
renameVariable =
    Debug.todo ""



---- POINTS


type alias Version a =
    { name : Maybe String
    , initial : That a
    , transformations : List (That Transformation)
    }


points : Pattern -> List (Version Point)
points (Pattern pattern) =
    Those.toList pattern.points
        |> List.map
            (\( thatPoint, { name } ) ->
                { name = name
                , initial = thatPoint
                , transformations = []
                }
            )


getPoint : Pattern -> That Point -> Maybe Point
getPoint (Pattern pattern) thatPoint =
    Those.get thatPoint pattern.points
        |> Maybe.map .a


getPointVersion : Pattern -> That Point -> Maybe (Version Point)
getPointVersion (Pattern pattern) thatPoint =
    Those.get thatPoint pattern.points
        |> Maybe.map
            (\{ name, a } ->
                { name = name
                , initial = thatPoint
                , transformations = []
                }
            )


insertPoint : Point -> Pattern -> Pattern
insertPoint point (Pattern pattern) =
    Pattern
        { pattern | points = Those.insert Nothing point pattern.points }


replacePoint : That Point -> Point -> Pattern -> Pattern
replacePoint =
    Debug.todo ""


removePoint : That Point -> Pattern -> Pattern
removePoint =
    Debug.todo ""



---- CIRCLES


circles : Pattern -> List ( That Circle, Maybe String )
circles =
    Debug.todo ""


getCircle : Pattern -> That Circle -> Maybe Circle
getCircle =
    Debug.todo ""


insertCircle : Circle -> Pattern -> Pattern
insertCircle =
    Debug.todo ""


replaceCircle : That Circle -> Circle -> Pattern -> Pattern
replaceCircle =
    Debug.todo ""


removeCircle : That Circle -> Pattern -> Pattern
removeCircle =
    Debug.todo ""



---- LINES


lines : Pattern -> List ( That Line, Maybe String )
lines (Pattern pattern) =
    Those.toList pattern.lines
        |> List.map (Tuple.mapSecond .name)


getLine : Pattern -> That Line -> Maybe Line
getLine (Pattern pattern) thatLine =
    Those.get thatLine pattern.lines
        |> Maybe.map .a


insertLine : Line -> Pattern -> Pattern
insertLine line (Pattern pattern) =
    Pattern
        { pattern | lines = Those.insert Nothing line pattern.lines }


replaceLine : That Line -> Line -> Pattern -> Pattern
replaceLine =
    Debug.todo ""


removeLine : That Line -> Pattern -> Pattern
removeLine =
    Debug.todo ""



---- TRANSFORMATIONS


insertTransformation : Transformation -> Pattern -> Pattern
insertTransformation transformation (Pattern pattern) =
    Pattern
        { pattern | transformations = Those.insert Nothing transformation pattern.transformations }



---- DETAILS


insertDetail : Detail -> Pattern -> Pattern
insertDetail detail (Pattern pattern) =
    Pattern
        { pattern | details = Those.insert Nothing detail pattern.details }


getDetail : Pattern -> That Detail -> Maybe Detail
getDetail (Pattern pattern) thatDetail =
    Those.get thatDetail pattern.details
        |> Maybe.map .a
