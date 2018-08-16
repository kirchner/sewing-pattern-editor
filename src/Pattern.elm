module Pattern
    exposing
        ( Detail(..)
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
        , decoder
        , empty
        , encode
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
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Parser
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Set exposing (Set)
import Vector2d


type Pattern
    = Pattern PatternData


type alias PatternData =
    { points : Store Point
    , circles : Store Circle
    , lines : Store Line
    , details : Store Detail
    , variables : Dict String Expr
    , transformations : Transformations
    }


empty : Pattern
empty =
    Pattern
        { points = emptyStore
        , circles = emptyStore
        , lines = emptyStore
        , details = emptyStore
        , variables = Dict.empty
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


get : Store a -> Int -> Maybe (Entry a)
get { entries } id =
    Dict.get id entries


toList : Store a -> List ( Int, Entry a )
toList { entries } =
    entries
        |> Dict.toList


values : Store a -> List (Entry a)
values { entries } =
    entries
        |> Dict.values



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
        geometryPoint ( id, { name } ) =
            let
                thatPoint =
                    thatPointFromId p id
            in
            Maybe.map
                (\p2d ->
                    ( thatPoint
                    , name
                    , p2d
                    )
                )
                (point2d p thatPoint)

        geometryLine ( id, { name } ) =
            let
                thatLine =
                    thatLineFromId p id
            in
            Maybe.map
                (\a2d ->
                    ( thatLine
                    , name
                    , a2d
                    )
                )
                (axis2d p thatLine)

        geometryDetail ( id, { name } ) =
            let
                thatDetail =
                    thatDetailFromId p id
            in
            Maybe.map
                (\p2d ->
                    ( thatDetail
                    , name
                    , p2d
                    )
                )
                (polygon2d p thatDetail)
    in
    ( { points =
            pattern.points
                |> toList
                |> List.filterMap geometryPoint
      , circles = []
      , lines =
            pattern.lines
                |> toList
                |> List.filterMap geometryLine
      , details =
            pattern.details
                |> toList
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
            pattern.transformations.entries
                |> List.filter targetPoint

        targetPoint ( id, transformation ) =
            case transformation of
                MirrorAt _ thosePoints ->
                    (thatPoint
                        |> withoutTransformation id
                        |> isMemberOf thosePoints
                    )
                        && (thatPoint
                                |> needsTransformation id
                           )

                _ ->
                    Debug.todo ""

        withoutTransformation id (That data) =
            That
                { data
                    | changes =
                        List.filter
                            (\{ transformationId } ->
                                transformationId /= id
                            )
                            data.changes
                }

        needsTransformation id (That { changes }) =
            List.any (\{ transformationId } -> transformationId == id) changes

        applyTransformations point =
            List.foldl applyTransformation point transformations

        applyTransformation ( _, transformation ) point =
            case transformation of
                MirrorAt thatLine targets ->
                    case axis2d p thatLine of
                        Just axis ->
                            point
                                |> Point2d.mirrorAcross axis

                        Nothing ->
                            point

                _ ->
                    Debug.todo ""
    in
    Maybe.map applyTransformations <|
        case
            thatPoint
                |> getObjectId
                |> get pattern.points
                |> Maybe.map .value
        of
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
axis2d ((Pattern pattern) as p) thatLine =
    case
        thatLine
            |> getObjectId
            |> get pattern.lines
            |> Maybe.map .value
    of
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
polygon2d ((Pattern pattern) as p) thatDetail =
    case
        thatDetail
            |> getObjectId
            |> get pattern.details
            |> Maybe.map .value
    of
        Just (CounterClockwise targets) ->
            targets
                |> List.map getObjectId
                |> List.map (thatPointFromId p)
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


type That object
    = That ThatData


type alias ThatData =
    { changes : List Change
    , objectId : Int
    }


getObjectId : That object -> Int
getObjectId (That that) =
    that.objectId


type alias Change =
    { transformationId : Int
    , branch : Maybe Int
    }


type Those a
    = Those (List (That a))


none : Those a
none =
    Those []


thoseFromList : List (That a) -> Those a
thoseFromList =
    -- FIXME make unique
    Those


insertIntoThose : That a -> Those a -> Those a
insertIntoThose that (Those those) =
    -- FIXME make unique
    Those (that :: those)


removeFromThose : That a -> Those a -> Those a
removeFromThose that those =
    -- FIXME actually implement
    those


memberOfThose : That a -> Those a -> Bool
memberOfThose that those =
    isMemberOf those that


isMemberOf : Those a -> That a -> Bool
isMemberOf (Those those) (That that) =
    let
        matches (That nextThat) =
            (that.objectId == nextThat.objectId)
                && sameChanges that.changes nextThat.changes

        sameChanges changesA changesB =
            case ( changesA, changesB ) of
                ( [], [] ) ->
                    True

                ( changeA :: restA, changeB :: restB ) ->
                    (changeA == changeB)
                        && sameChanges restA restB

                _ ->
                    False
    in
    List.any matches those



-- TRANSFORMATIONS


type Transformation
    = MirrorAt (That Line) (Those Point)
    | RotateAround (That Point) Angle (Those Point)
    | CutAlong (That Line) Detail
    | CutLineSegment (That LineSegment) Detail
    | CutCurve (That Curve) Detail



-- OBJECTS


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
    = CenteredThat (That Point) Length


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


points : Pattern -> List ( That Point, Entry Point )
points ((Pattern pattern) as p) =
    pattern.points
        |> toList
        |> List.map (Tuple.mapFirst (thatPointFromId p))


thatPointFromId : Pattern -> Int -> That Point
thatPointFromId (Pattern pattern) id =
    let
        applyTransformation ( transformationId, transformation ) changes =
            case transformation of
                MirrorAt thatLine thosePoints ->
                    if
                        That { changes = changes, objectId = id }
                            |> isMemberOf thosePoints
                    then
                        { transformationId = transformationId
                        , branch = Nothing
                        }
                            :: changes
                    else
                        changes

                _ ->
                    changes
    in
    That
        { changes =
            pattern.transformations.entries
                |> List.foldl applyTransformation []
        , objectId = id
        }


getPoint : Pattern -> That Point -> Maybe (Entry Point)
getPoint (Pattern pattern) (That { objectId }) =
    pattern.points.entries
        |> Dict.get objectId


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
lines ((Pattern pattern) as p) =
    pattern.lines
        |> toList
        |> List.map (Tuple.mapFirst (thatLineFromId p))


thatLineFromId : Pattern -> Int -> That Line
thatLineFromId (Pattern pattern) id =
    That
        { changes = []
        , objectId = id
        }


getLine : Pattern -> That Line -> Maybe Line
getLine (Pattern pattern) thatLine =
    Debug.todo ""


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


thatDetailFromId : Pattern -> Int -> That Detail
thatDetailFromId (Pattern pattern) id =
    That
        { changes = []
        , objectId = id
        }


insertDetail : Detail -> Pattern -> Pattern
insertDetail detail (Pattern pattern) =
    Pattern
        { pattern | details = insert Nothing detail pattern.details }


getDetail : Pattern -> That Detail -> Maybe Detail
getDetail (Pattern pattern) thatDetail =
    Debug.todo ""



---- ENCODER


encode : Pattern -> Value
encode (Pattern pattern) =
    Encode.object
        [ ( "points", encodeStore encodePoint pattern.points )
        , ( "lines", encodeStore encodeLine pattern.lines )
        , ( "details", encodeStore encodeDetail pattern.details )
        , ( "transformations", encodeTransformations pattern.transformations )
        ]


encodeStore : (a -> Value) -> Store a -> Value
encodeStore encodeA { entries, nextId } =
    let
        encodeEntry ( id, { name, value } ) =
            Encode.object
                [ ( "id", Encode.int id )
                , ( "name"
                  , case name of
                        Nothing ->
                            Encode.null

                        Just actualName ->
                            Encode.string actualName
                  )
                , ( "value", encodeA value )
                ]
    in
    Encode.object
        [ ( "entries"
          , entries
                |> Dict.toList
                |> Encode.list encodeEntry
          )
        , ( "nextId", Encode.int nextId )
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


encodePoint : Point -> Value
encodePoint point =
    case point of
        Origin ->
            withType "origin" []

        LeftOf anchor length ->
            withType "leftOf"
                [ ( "anchor", encodeThat anchor )
                , ( "distance", encodeLength length )
                ]

        RightOf anchor length ->
            withType "rightOf"
                [ ( "anchor", encodeThat anchor )
                , ( "distance", encodeLength length )
                ]

        Above anchor length ->
            withType "above"
                [ ( "anchor", encodeThat anchor )
                , ( "distance", encodeLength length )
                ]

        Below anchor length ->
            withType "below"
                [ ( "anchor", encodeThat anchor )
                , ( "distance", encodeLength length )
                ]

        AtAngle anchor angle length ->
            withType "atAngle"
                [ ( "anchor", encodeThat anchor )
                , ( "angle", encodeAngle angle )
                , ( "distance", encodeLength length )
                ]

        _ ->
            Encode.null


encodeLine : Line -> Value
encodeLine line =
    case line of
        ThroughTwoPoints anchorA anchorB ->
            withType "throughTwoPoints"
                [ ( "anchorA", encodeThat anchorA )
                , ( "anchorB", encodeThat anchorB )
                ]

        _ ->
            Encode.null


encodeDetail : Detail -> Value
encodeDetail detail =
    case detail of
        CounterClockwise thatPoints ->
            withType "counterClockwise"
                [ ( "points", Encode.list encodeThat thatPoints ) ]


encodeTransformation : Transformation -> Value
encodeTransformation transformation =
    case transformation of
        MirrorAt line targets ->
            withType "mirrorAt"
                [ ( "line", encodeThat line )
                , ( "targets", encodeThose targets )
                ]

        _ ->
            Encode.null


encodeThat : That a -> Value
encodeThat (That { changes, objectId }) =
    Encode.object
        [ ( "changes"
          , Encode.list encodeChange changes
          )
        , ( "objectId", Encode.int objectId )
        ]


encodeThose : Those a -> Value
encodeThose (Those thats) =
    Encode.list encodeThat thats


encodeChange : Change -> Value
encodeChange { transformationId, branch } =
    Encode.object
        [ ( "transformationId", Encode.int transformationId )
        , ( "branch"
          , case branch of
                Nothing ->
                    Encode.null

                Just actualBranch ->
                    Encode.int actualBranch
          )
        ]


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
        |> Decode.required "points" (storeDecoder pointDecoder)
        |> Decode.hardcoded emptyStore
        |> Decode.required "lines" (storeDecoder lineDecoder)
        |> Decode.required "details" (storeDecoder detailDecoder)
        |> Decode.hardcoded Dict.empty
        |> Decode.required "transformations" transformationsDecoder
        |> Decode.map Pattern


storeDecoder : Decoder a -> Decoder (Store a)
storeDecoder aDecoder =
    let
        entryDecoder =
            Decode.map2 Tuple.pair
                (Decode.field "id" Decode.int)
                (Decode.succeed Entry
                    |> Decode.required "name" (Decode.nullable Decode.string)
                    |> Decode.required "value" aDecoder
                )
    in
    Decode.succeed Store
        |> Decode.required "entries" (Decode.map Dict.fromList (Decode.list entryDecoder))
        |> Decode.required "nextId" Decode.int


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


pointDecoder : Decoder Point
pointDecoder =
    Decode.oneOf
        [ typeDecoder "origin" (Decode.succeed Origin)
        , typeDecoder "leftOf" <|
            Decode.map2 LeftOf
                (Decode.field "anchor" thatDecoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "rightOf" <|
            Decode.map2 RightOf
                (Decode.field "anchor" thatDecoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "above" <|
            Decode.map2 Above
                (Decode.field "anchor" thatDecoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "below" <|
            Decode.map2 Below
                (Decode.field "anchor" thatDecoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "atAngle" <|
            Decode.map3 AtAngle
                (Decode.field "anchor" thatDecoder)
                (Decode.field "angle" angleDecoder)
                (Decode.field "distance" lengthDecoder)
        ]


lineDecoder : Decoder Line
lineDecoder =
    Decode.oneOf
        [ typeDecoder "throughTwoPoints" <|
            Decode.map2 ThroughTwoPoints
                (Decode.field "anchorA" thatDecoder)
                (Decode.field "anchorB" thatDecoder)
        ]


detailDecoder : Decoder Detail
detailDecoder =
    Decode.oneOf
        [ typeDecoder "counterClockwise" <|
            Decode.map CounterClockwise
                (Decode.field "points" (Decode.list thatDecoder))
        ]


transformationDecoder : Decoder Transformation
transformationDecoder =
    Decode.oneOf
        [ typeDecoder "mirrorAt" <|
            Decode.map2 MirrorAt
                (Decode.field "line" thatDecoder)
                (Decode.field "targets" thoseDecoder)
        ]


thatDecoder : Decoder (That a)
thatDecoder =
    Decode.succeed ThatData
        |> Decode.required "changes" (Decode.list changeDecoder)
        |> Decode.required "objectId" Decode.int
        |> Decode.map That


thoseDecoder : Decoder (Those a)
thoseDecoder =
    Decode.map Those (Decode.list thatDecoder)


changeDecoder : Decoder Change
changeDecoder =
    Decode.succeed Change
        |> Decode.required "transformationId" Decode.int
        |> Decode.required "branch" (Decode.nullable Decode.int)


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
