module Pattern
    exposing
        ( At(..)
        , Detail(..)
        , Entry
        , Geometry
        , Length(..)
        , Line(..)
        , Pattern
        , Point(..)
        , Problems
        , That
        , Those
        , Transformation(..)
        , circles
        , empty
        , exprFromFloat
        , geometry
        , getCircle
        , getLine
        , getPoint
        , insertCircle
        , insertDetail
        , insertIntoThose
        , insertLine
        , insertPoint
        , insertTransformation
        , lastState
        , lines
        , memberOfThose
        , none
        , points
        , removeCircle
        , removeFromThose
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
import Set exposing (Set)
import Vector2d


type Pattern
    = Pattern
        { points : Store Point
        , circles : Store Circle
        , lines : Store Line
        , variables : Dict String Expr
        , details : Store Detail
        , transformations : Transformations
        }


empty : Pattern
empty =
    Pattern
        { points = emptyStore
        , circles = emptyStore
        , lines = emptyStore
        , variables = Dict.empty
        , details = emptyStore
        , transformations = noTransformations
        }



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



---- STORE


type alias Store a =
    { entries : Dict Int (Entry a)
    , nextId : Int
    }


type alias Entry a =
    { name : Maybe String
    , value : a
    }


emptyStore : Store a
emptyStore =
    { entries = Dict.empty
    , nextId = 0
    }


insert : Maybe String -> a -> Store a -> Store a
insert name value store =
    { store
        | entries = Dict.insert store.nextId (Entry name value) store.entries
        , nextId = store.nextId + 1
    }


get : That a -> Store a -> Maybe (Entry a)
get (That id) { entries } =
    Dict.get id entries


toList : Store a -> List ( That a, Entry a )
toList { entries } =
    entries
        |> Dict.toList
        |> List.map (Tuple.mapFirst That)


toListAt : State -> Store a -> List ( At (That a), Entry a )
toListAt state { entries } =
    entries
        |> Dict.toList
        |> List.map (Tuple.mapFirst (That >> At state))


values : Store a -> List (Entry a)
values { entries } =
    entries
        |> Dict.values


type That a
    = That Int


type Those a
    = Those (Set Int)


none : Those a
none =
    Those Set.empty


thoseFromList : List (That a) -> Those a
thoseFromList ids =
    let
        getId (That id) =
            id
    in
    ids
        |> List.map getId
        |> Set.fromList
        |> Those


insertIntoThose : That a -> Those a -> Those a
insertIntoThose (That id) (Those ids) =
    Those (Set.insert id ids)


removeFromThose : That a -> Those a -> Those a
removeFromThose (That id) (Those ids) =
    Those (Set.remove id ids)


memberOfThose : That a -> Those a -> Bool
memberOfThose (That id) (Those ids) =
    Set.member id ids



---- GEOMETRY


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
        geometryPoint ( (At _ thatPoint) as thatAtPoint, { name } ) =
            point2d p thatAtPoint
                |> Maybe.map (\p2d -> ( thatPoint, name, p2d ))

        geometryLine ( (At _ thatLine) as thatAtLine, { name } ) =
            axis2d p thatAtLine
                |> Maybe.map (\a2d -> ( thatLine, name, a2d ))

        geometryDetail ( (At _ thatDetail) as thatAtDetail, { name } ) =
            polygon2d p thatAtDetail
                |> Maybe.map (\p2d -> ( thatDetail, name, p2d ))

        state =
            lastState p
    in
    ( { points =
            pattern.points
                |> toListAt state
                |> List.filterMap geometryPoint
      , circles = []
      , lines =
            pattern.lines
                |> toListAt state
                |> List.filterMap geometryLine
      , details =
            pattern.details
                |> toListAt state
                |> List.filterMap geometryDetail
      }
    , { doNotCompute = []
      , missingPoints = []
      , missingCircles = []
      , missingLines = []
      }
    )


point2d : Pattern -> At (That Point) -> Maybe Point2d
point2d ((Pattern pattern) as p) (At state thatPoint) =
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
                |> allUntil state
                |> List.filter targetPoint

        targetPoint ( _, transformation ) =
            case transformation of
                MirrorAt _ (At _ targets) ->
                    memberOfThose thatPoint targets

                _ ->
                    Debug.todo ""

        applyTransformations point =
            List.foldl applyTransformation point transformations

        applyTransformation ( _, transformation ) point =
            case transformation of
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
        case Maybe.map .value (getPoint p thatPoint) of
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


axis2d : Pattern -> At (That Line) -> Maybe Axis2d
axis2d ((Pattern _) as p) (At state thatLine) =
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


polygon2d : Pattern -> At (That Detail) -> Maybe Polygon2d
polygon2d ((Pattern _) as p) (At state thatDetail) =
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


type At that
    = At State that


type Construction
    = Point Point
    | Line Line
    | LineSegment LineSegment
    | Curve Curve
    | Circle Circle
    | Ellipsis Ellipsis
    | Detail Detail


type Transformation
    = MirrorAt (At (That Line)) (At (Those Point))
    | RotateAround (At (That Point)) Angle (At (Those Point))
    | CutAlong (At (That Line)) Detail
    | CutLineSegment (At (That LineSegment)) Detail
    | CutCurve (At (That Curve)) Detail



---- ELEMENTS


type Point
    = Origin
    | LeftOf (At (That Point)) Length
    | RightOf (At (That Point)) Length
    | Above (At (That Point)) Length
    | Below (At (That Point)) Length
    | AtAngle (At (That Point)) Angle Length
    | BetweenRatio (At (That Point)) (At (That Point)) Ratio
    | BetweenLength (At (That Point)) (At (That Point)) Length
      -- ON OBJECT
    | OnLineAtX (At (That Line)) Coordinate
    | OnLineAtY (At (That Line)) Coordinate
    | OnCurve (At (That Curve)) Constraint
    | OnLineSegment (At (That LineSegment)) Constraint
    | OnCircle (At (That Circle)) Constraint
    | OnCircleFirstTangent (At (That Circle)) (At (That Point))
    | OnCircleSecondTangent (At (That Circle)) (At (That Point))
    | OnCircleFirstChord (At (That Circle)) Angle
    | OnCircleSecondChord (At (That Circle)) Angle
      -- BY INTERSECTION
    | FirstCircleCircle (At (That Circle)) (At (That Circle))
    | SecondCircleCircle (At (That Circle)) (At (That Circle))
    | LineLine (At (That Line)) (At (That Line))
    | FirstCircleLine (At (That Circle)) (At (That Line))
    | SecondCircleLine (At (That Circle)) (At (That Line))
      -- BY TRANSFORMATION
    | Transformed (At (That Point)) Transformation


type Constraint
    = X Coordinate
    | Y Coordinate
    | MinX
    | MaxX
    | MinY
    | MaxY


type Line
    = ThroughOnePoint (At (That Point)) Angle
    | ThroughTwoPoints (At (That Point)) (At (That Point))


type LineSegment
    = FromTo (At (That Point)) (At (That Point))


type Circle
    = CenteredAt (At (That Point)) Length


type Curve
    = TODOCurve


type Ellipsis
    = TODOEllipsis


type Detail
    = CounterClockwise (List (At (That Point)))



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


points : Pattern -> List ( That Point, Entry Point )
points (Pattern pattern) =
    toList pattern.points


getPoint : Pattern -> That Point -> Maybe (Entry Point)
getPoint (Pattern pattern) thatPoint =
    get thatPoint pattern.points


insertPoint : Point -> Pattern -> Pattern
insertPoint point (Pattern pattern) =
    Pattern
        { pattern | points = insert Nothing point pattern.points }


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


lines : Pattern -> List ( That Line, Entry Line )
lines (Pattern pattern) =
    toList pattern.lines


getLine : Pattern -> That Line -> Maybe Line
getLine (Pattern pattern) thatLine =
    get thatLine pattern.lines
        |> Maybe.map .value


insertLine : Line -> Pattern -> Pattern
insertLine line (Pattern pattern) =
    Pattern
        { pattern | lines = insert Nothing line pattern.lines }


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
        { pattern | transformations = append transformation pattern.transformations }



---- DETAILS


insertDetail : Detail -> Pattern -> Pattern
insertDetail detail (Pattern pattern) =
    Pattern
        { pattern | details = insert Nothing detail pattern.details }


getDetail : Pattern -> That Detail -> Maybe Detail
getDetail (Pattern pattern) thatDetail =
    get thatDetail pattern.details
        |> Maybe.map .value
