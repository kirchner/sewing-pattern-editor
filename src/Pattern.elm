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
        , circles
        , computeLength
        , decoder
        , empty
        , encode
        , exprFromFloat
        , geometry
        , getCircle
        , getLine
        , getPoint
        , getPointGeometries
        , getPointGeometry
        , insertCircle
        , insertDetail
        , insertLine
        , insertPoint
        , insertTransformation
        , lastState
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
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
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
    , details : Store Detail
    , variables : Dict String Expr
    , transformations : Transformations
    }


empty : Pattern
empty =
    Pattern
        { points = Store.empty
        , circles = Store.empty
        , lines = Store.empty
        , details = Store.empty
        , variables = Dict.empty
        , transformations = noTransformations
        }



---- OBJECTS


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



---- TRANSFORMATIONS


type Transformation
    = MirrorAt (That Line) (Those Point)
    | RotateAround (That Point) Angle (Those Point)
    | CutAlong (That Line) Detail
    | CutLineSegment (That LineSegment) Detail
    | CutCurve (That Curve) Detail



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


computeLength : Pattern -> Length -> Maybe Float
computeLength (Pattern pattern) (Length expr) =
    compute pattern.variables expr
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
geometry pattern =
    let
        geometryPoint ( thatPoint, { name } ) =
            point2d pattern thatPoint
                |> Maybe.map (\p2d -> ( thatPoint, name, p2d ))

        geometryLine ( thatLine, { name } ) =
            axis2d pattern thatLine
                |> Maybe.map (\a2d -> ( thatLine, name, a2d ))

        geometryDetail ( thatDetail, { name } ) =
            polygon2d pattern thatDetail
                |> Maybe.map (\p2d -> ( thatDetail, name, p2d ))
    in
    ( { points =
            pattern
                |> points
                |> List.filterMap geometryPoint
      , circles = []
      , lines =
            pattern
                |> lines
                |> List.filterMap geometryLine
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
            List.filterMap
                (\{ transformationId } ->
                    getTransformation pattern.transformations.entries transformationId
                )
                (That.changes thatPoint)

        getTransformation entries id =
            entries
                |> List.filter (\( nextId, _ ) -> nextId == id)
                |> List.head

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


polygon2d : Pattern -> That Detail -> Maybe Polygon2d
polygon2d ((Pattern pattern) as p) thatDetail =
    case Maybe.map .value (getDetail p thatDetail) of
        Just (CounterClockwise targets) ->
            targets
                |> List.map That.objectId
                |> List.map (thatPointFromId p)
                |> List.filterMap (point2d p)
                |> Polygon2d.singleLoop
                |> Just

        _ ->
            Nothing



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
    let
        finalPoints =
            pattern.points
                |> Store.toList
                |> List.map (Tuple.mapFirst (thatPointFromId p))

        neededPoints =
            List.concat
                [ finalPoints
                    |> List.foldl collectNeededPoint []
                , pattern.lines
                    |> Store.toList
                    |> List.map (Tuple.mapFirst (thatLineFromId p))
                    |> List.foldl collectNeededPointByLine []
                ]

        collectNeededPoint ( _, { value } ) collectedPoints =
            case value of
                LeftOf thatAnchorPoint _ ->
                    addPoint thatAnchorPoint collectedPoints

                RightOf thatAnchorPoint _ ->
                    addPoint thatAnchorPoint collectedPoints

                Above thatAnchorPoint _ ->
                    addPoint thatAnchorPoint collectedPoints

                Below thatAnchorPoint _ ->
                    addPoint thatAnchorPoint collectedPoints

                _ ->
                    collectedPoints

        collectNeededPointByLine ( _, { value } ) collectedPoints =
            case value of
                ThroughTwoPoints thatAnchorA thatAnchorB ->
                    addPoint thatAnchorA (addPoint thatAnchorB collectedPoints)

                _ ->
                    collectedPoints

        addPoint thatPoint collectedPoints =
            case getPoint p thatPoint of
                Just entry ->
                    ( thatPoint, entry ) :: collectedPoints

                Nothing ->
                    collectedPoints
    in
    List.uniqueBy (Tuple.first >> That.toComparable)
        (finalPoints ++ neededPoints)


thatPointFromId : Pattern -> Int -> That Point
thatPointFromId (Pattern pattern) id =
    let
        applyTransformation ( transformationId, transformation ) previousThat =
            case transformation of
                MirrorAt thatLine thosePoints ->
                    if Those.member previousThat thosePoints then
                        that
                            ({ transformationId = transformationId
                             , branch = Nothing
                             }
                                :: That.changes previousThat
                            )
                            id
                    else
                        previousThat

                _ ->
                    previousThat
    in
    List.foldr applyTransformation (that [] id) pattern.transformations.entries


getPoint : Pattern -> That Point -> Maybe (Entry Point)
getPoint (Pattern pattern) =
    Store.get pattern.points << That.objectId


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
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatLineFromId p))


thatLineFromId : Pattern -> Int -> That Line
thatLineFromId (Pattern pattern) id =
    that [] id


getLine : Pattern -> That Line -> Maybe (Entry Line)
getLine (Pattern pattern) =
    Store.get pattern.lines << That.objectId


insertLine : Line -> Pattern -> Pattern
insertLine line (Pattern pattern) =
    Pattern
        { pattern | lines = Store.insert Nothing line pattern.lines }


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


details : Pattern -> List ( That Detail, Entry Detail )
details ((Pattern pattern) as p) =
    pattern.details
        |> Store.toList
        |> List.map (Tuple.mapFirst (thatDetailFromId p))


thatDetailFromId : Pattern -> Int -> That Detail
thatDetailFromId (Pattern pattern) id =
    that [] id


insertDetail : Detail -> Pattern -> Pattern
insertDetail detail (Pattern pattern) =
    Pattern
        { pattern | details = Store.insert Nothing detail pattern.details }


getDetail : Pattern -> That Detail -> Maybe (Entry Detail)
getDetail (Pattern pattern) =
    Store.get pattern.details << That.objectId



---- ENCODER


encode : Pattern -> Value
encode (Pattern pattern) =
    Encode.object
        [ ( "points", Store.encode encodePoint pattern.points )
        , ( "lines", Store.encode encodeLine pattern.lines )
        , ( "details", Store.encode encodeDetail pattern.details )
        , ( "transformations", encodeTransformations pattern.transformations )
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
                [ ( "anchor", That.encode anchor )
                , ( "distance", encodeLength length )
                ]

        RightOf anchor length ->
            withType "rightOf"
                [ ( "anchor", That.encode anchor )
                , ( "distance", encodeLength length )
                ]

        Above anchor length ->
            withType "above"
                [ ( "anchor", That.encode anchor )
                , ( "distance", encodeLength length )
                ]

        Below anchor length ->
            withType "below"
                [ ( "anchor", That.encode anchor )
                , ( "distance", encodeLength length )
                ]

        AtAngle anchor angle length ->
            withType "atAngle"
                [ ( "anchor", That.encode anchor )
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
                [ ( "anchorA", That.encode anchorA )
                , ( "anchorB", That.encode anchorB )
                ]

        _ ->
            Encode.null


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
        |> Decode.hardcoded Store.empty
        |> Decode.required "lines" (Store.decoder lineDecoder)
        |> Decode.required "details" (Store.decoder detailDecoder)
        |> Decode.hardcoded Dict.empty
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


pointDecoder : Decoder Point
pointDecoder =
    Decode.oneOf
        [ typeDecoder "origin" (Decode.succeed Origin)
        , typeDecoder "leftOf" <|
            Decode.map2 LeftOf
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "rightOf" <|
            Decode.map2 RightOf
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "above" <|
            Decode.map2 Above
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "below" <|
            Decode.map2 Below
                (Decode.field "anchor" That.decoder)
                (Decode.field "distance" lengthDecoder)
        , typeDecoder "atAngle" <|
            Decode.map3 AtAngle
                (Decode.field "anchor" That.decoder)
                (Decode.field "angle" angleDecoder)
                (Decode.field "distance" lengthDecoder)
        ]


lineDecoder : Decoder Line
lineDecoder =
    Decode.oneOf
        [ typeDecoder "throughTwoPoints" <|
            Decode.map2 ThroughTwoPoints
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
