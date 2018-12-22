module Seamly2D.V0_6_0 exposing
    ( Val
    , decode
    , replaceLineExprs
    , toPattern
    )

import Dict exposing (Dict)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Parser exposing ((|.), (|=), Parser)
import Pattern
import Set
import String.Extra as String
import That exposing (That)
import XmlParser as Xml


type alias Val =
    { patterns : Nonempty Pattern
    }


type alias Pattern =
    { version : String
    , unit : Unit
    , description : Maybe String
    , notes : Maybe String
    , patternName : Maybe String
    , patternNumber : Maybe String
    , company : Maybe String
    , customer : Maybe String
    , measurements : Maybe String
    , increments : Dict String Increment
    , draws : Nonempty Draw
    }


type Unit
    = Mm
    | Cm
    | Inch


type alias Increment =
    { description : String
    , formula : String
    }


type alias Draw =
    { name : String
    , calculations : Nonempty Calculation
    }


type alias Calculation =
    Dict Int Object


type Object
    = Point PointData
    | Line LineData
    | Spline SplineData


type alias PointData =
    { x : Maybe Float
    , y : Maybe Float
    , mx : Maybe Float
    , my : Maybe Float
    , type_ : Maybe String
    , name : Maybe String
    , firstPoint : Maybe Int
    , secondPoint : Maybe Int
    , thirdPoint : Maybe Int
    , basePoint : Maybe Int
    , pShoulder : Maybe Int
    , p1Line : Maybe Int
    , p2Line : Maybe Int
    , length : Maybe String
    , angle : Maybe String
    , typeLine : Maybe LinePenStyle
    , splinePath : Maybe Int
    , spline : Maybe Int
    , p1Line1 : Maybe Int
    , p1Line2 : Maybe Int
    , p2Line1 : Maybe Int
    , p2Line2 : Maybe Int
    , center : Maybe Int
    , radius : Maybe String
    , axisP1 : Maybe Int
    , axisP2 : Maybe Int
    , arc : Maybe Int
    , elArc : Maybe Int
    , curve : Maybe Int
    , curve1 : Maybe Int
    , curve2 : Maybe Int
    , lineColor : Maybe Color
    , color : Maybe Color
    , firstArc : Maybe Int
    , secondArc : Maybe Int
    , crossPoint : Maybe CrossType
    , vCrossPoint : Maybe CrossType
    , hCrossPoint : Maybe CrossType
    , c1Center : Maybe Int
    , c2Center : Maybe Int
    , c1Radius : Maybe String
    , c2Radius : Maybe String
    , cRadius : Maybe String
    , tangent : Maybe Int
    , cCenter : Maybe Int
    , name1 : Maybe String
    , mx1 : Maybe Float
    , my1 : Maybe Float
    , name2 : Maybe String
    , mx2 : Maybe Float
    , my2 : Maybe Float
    , point1 : Maybe Int
    , point2 : Maybe Int
    , dartP1 : Maybe Int
    , dartP2 : Maybe Int
    , dartP3 : Maybe Int
    , baseLineP1 : Maybe Int
    , baseLineP2 : Maybe Int
    }


type alias LineData =
    { firstPoint : Maybe Int
    , secondPoint : Maybe Int
    , typeLine : Maybe LinePenStyle
    , lineColor : Maybe Color
    }


type LinePenStyle
    = None
    | Hair
    | DashLine
    | DotLine
    | DashDotLine
    | DashDotDotLine


type CrossType
    = Cross1
    | Cross2


type alias SplineData =
    { pathPoints : List PathPoint
    , kCurve : Maybe Float
    , type_ : Maybe String
    , kAsm1 : Maybe Float
    , kAsm2 : Maybe Float
    , angle1 : Maybe String
    , angle2 : Maybe String
    , length1 : Maybe String
    , length2 : Maybe String
    , point1 : Maybe Int
    , point2 : Maybe Int
    , point3 : Maybe Int
    , point4 : Maybe Int
    , color : Maybe Color
    , penStyle : Maybe CurvePenStyle
    , duplicate : Maybe Int
    }


type alias PathPoint =
    { kAsm2 : Maybe String
    , pSpline : Maybe Int
    , angle : Maybe String
    , angle1 : Maybe String
    , angle2 : Maybe String
    , length1 : Maybe String
    , length2 : Maybe String
    , kAsm1 : Maybe String
    }


type CurvePenStyle
    = CurvePenStyleHair
    | CurvePenStyleDashLine
    | CurvePenStyleDotLine
    | CurvePenStyleDashDotLine
    | CurvePenStyleDashDotDotLine


type Color
    = Black
    | Green
    | Blue
    | DarkRed
    | DarkGreen
    | DarkBlue
    | Yellow
    | Lightsalmon
    | Goldenrod
    | Orange
    | Deeppink
    | Violet
    | Darkviolet
    | Mediumseagreen
    | Lime
    | Deepskyblue
    | Cornflowerblue



---- TO PATTERN


toPattern : Pattern -> Pattern.Pattern
toPattern { increments, draws } =
    let
        insertVariables pattern =
            increments
                |> Dict.toList
                |> List.foldl
                    (\( name, { formula } ) nextPattern ->
                        Pattern.insertVariable name formula nextPattern
                    )
                    pattern

        insertObjects pattern =
            draws
                |> Nonempty.head
                |> .calculations
                |> Nonempty.head
                |> Dict.toList
                |> List.foldl insertObject ( pattern, noObjects )
                |> Tuple.first

        insertObject ( id, object ) ( previousPattern, previousObjects ) =
            case object of
                Point data ->
                    insertPoint previousPattern previousObjects data id

                Line data ->
                    insertLine previousPattern previousObjects data id

                Spline data ->
                    insertSpline previousPattern previousObjects data id
    in
    Pattern.empty
        |> insertVariables
        |> insertObjects
        |> Pattern.computeCache


type alias Objects =
    { points : Dict Int (That Pattern.Point)
    , lines : Dict Int (That Pattern.Line)
    , curves : Dict Int (List (That Pattern.Curve))
    }


noObjects : Objects
noObjects =
    { points = Dict.empty
    , lines = Dict.empty
    , curves = Dict.empty
    }



---- INSERT POINT


insertPoint :
    Pattern.Pattern
    -> Objects
    -> PointData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertPoint pattern objects data id =
    let
        insert =
            Maybe.map
                (\point ->
                    let
                        ( nextPattern, thatPoint ) =
                            Pattern.insertPoint data.name
                                point
                                pattern
                    in
                    ( nextPattern
                    , { objects | points = Dict.insert id thatPoint objects.points }
                    )
                )
                >> Maybe.withDefault ( pattern, objects )
    in
    case data.type_ of
        Just "single" ->
            insert (pointSingle data)

        Just "endLine" ->
            insert (pointEndLine pattern objects data)

        Just "alongLine" ->
            insert (pointAlongLine pattern objects data)

        Just "normal" ->
            insert (pointNormal pattern objects data)

        Just "lineIntersectAxis" ->
            insertPointLineIntersectAxis pattern objects data id

        Just "pointOfIntersection" ->
            insertPointOfIntersection pattern objects data id

        _ ->
            ( pattern, objects )


pointSingle : PointData -> Maybe Pattern.Point
pointSingle data =
    Maybe.map2
        (\x y ->
            Pattern.origin { x = x, y = y }
        )
        data.x
        data.y


pointEndLine : Pattern.Pattern -> Objects -> PointData -> Maybe Pattern.Point
pointEndLine pattern objects data =
    Maybe.map3 (Pattern.atAngle pattern)
        (Maybe.andThen (lookUp objects.points) data.basePoint)
        (Maybe.map (replaceLineExprs >> fixRotation) data.angle)
        (Maybe.map replaceLineExprs data.length)
        |> Maybe.withDefault Nothing


pointAlongLine : Pattern.Pattern -> Objects -> PointData -> Maybe Pattern.Point
pointAlongLine pattern objects data =
    Maybe.map3
        (Pattern.betweenLength pattern)
        (Maybe.andThen (lookUp objects.points) data.firstPoint)
        (Maybe.andThen (lookUp objects.points) data.secondPoint)
        (Maybe.map3
            (\length firstName secondName ->
                String.replace
                    "CurrentLength"
                    (String.concat
                        [ "distance(\n  "
                        , firstName
                        , ",\n  "
                        , secondName
                        , ")"
                        ]
                    )
                    length
            )
            (Maybe.map replaceLineExprs data.length)
            (Maybe.andThen (pointName pattern objects) data.firstPoint)
            (Maybe.andThen (pointName pattern objects) data.secondPoint)
        )
        |> Maybe.withDefault Nothing


pointNormal : Pattern.Pattern -> Objects -> PointData -> Maybe Pattern.Point
pointNormal pattern objects data =
    Maybe.map3 (Pattern.atAngle pattern)
        (Maybe.andThen (lookUp objects.points) data.firstPoint)
        (Maybe.map3
            (\angle firstName secondName ->
                String.concat
                    [ "angleOfLine(\n  "
                    , firstName
                    , ",\n  "
                    , secondName
                    , "\n) - 90 +\n"
                    , fixRotation angle
                    ]
            )
            (Maybe.map replaceLineExprs data.angle)
            (Maybe.andThen (pointName pattern objects) data.firstPoint)
            (Maybe.andThen (pointName pattern objects) data.secondPoint)
        )
        (Maybe.map replaceLineExprs data.length)
        |> Maybe.withDefault Nothing


insertPointLineIntersectAxis :
    Pattern.Pattern
    -> Objects
    -> PointData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertPointLineIntersectAxis pattern objects data id =
    Maybe.map4
        (\basePoint angle p1Line p2Line ->
            Maybe.map2
                (\lineA lineB ->
                    let
                        ( nextPatternA, thatLineA ) =
                            Pattern.insertLine Nothing lineA pattern

                        ( nextPatternB, thatLineB ) =
                            Pattern.insertLine Nothing lineB nextPatternA
                    in
                    case Pattern.lineLine nextPatternB thatLineA thatLineB of
                        Nothing ->
                            Nothing

                        Just point ->
                            let
                                ( finalPattern, thatPoint ) =
                                    Pattern.insertPoint data.name
                                        point
                                        nextPatternB
                            in
                            Just
                                ( finalPattern
                                , { objects
                                    | points = Dict.insert id thatPoint objects.points
                                  }
                                )
                )
                (Pattern.throughOnePoint pattern basePoint angle)
                (Pattern.throughTwoPoints pattern p1Line p2Line)
                |> Maybe.withDefault Nothing
        )
        (Maybe.andThen (lookUp objects.points) data.basePoint)
        (Maybe.map (replaceLineExprs >> fixRotation) data.angle)
        (Maybe.andThen (lookUp objects.points) data.p1Line)
        (Maybe.andThen (lookUp objects.points) data.p2Line)
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ( pattern, objects )


insertPointOfIntersection :
    Pattern.Pattern
    -> Objects
    -> PointData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertPointOfIntersection pattern objects data id =
    Maybe.map2
        (\firstPoint secondPoint ->
            Maybe.map2
                (\lineA lineB ->
                    let
                        ( nextPatternA, thatLineA ) =
                            Pattern.insertLine Nothing lineA pattern

                        ( nextPatternB, thatLineB ) =
                            Pattern.insertLine Nothing lineB nextPatternA
                    in
                    case Pattern.lineLine nextPatternB thatLineA thatLineB of
                        Nothing ->
                            Nothing

                        Just point ->
                            let
                                ( finalPattern, thatPoint ) =
                                    Pattern.insertPoint data.name
                                        point
                                        nextPatternB
                            in
                            Just
                                ( finalPattern
                                , { objects
                                    | points = Dict.insert id thatPoint objects.points
                                  }
                                )
                )
                (Pattern.throughOnePoint pattern firstPoint "90")
                (Pattern.throughOnePoint pattern secondPoint "0")
                |> Maybe.withDefault Nothing
        )
        (Maybe.andThen (lookUp objects.points) data.firstPoint)
        (Maybe.andThen (lookUp objects.points) data.secondPoint)
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ( pattern, objects )



---- INSERT LINE


insertLine :
    Pattern.Pattern
    -> Objects
    -> LineData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertLine pattern objects data id =
    let
        insert =
            Maybe.map
                (\line ->
                    let
                        ( nextPattern, thatLine ) =
                            Pattern.insertLine Nothing line pattern
                    in
                    ( nextPattern
                    , { objects
                        | lines = Dict.insert id thatLine objects.lines
                      }
                    )
                )
                >> Maybe.withDefault ( pattern, objects )
    in
    insert
        (Maybe.map2 (Pattern.throughTwoPoints pattern)
            (Maybe.andThen (lookUp objects.points) data.firstPoint)
            (Maybe.andThen (lookUp objects.points) data.secondPoint)
            |> Maybe.withDefault Nothing
        )



---- INSERT SPLINE


insertSpline :
    Pattern.Pattern
    -> Objects
    -> SplineData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertSpline pattern objects data id =
    case data.type_ of
        Just "simpleInteractive" ->
            insertSplineSimpleInteractive pattern objects data id

        Just "pathInteractive" ->
            insertSplinePathInteractive pattern objects data id

        _ ->
            ( pattern, objects )


insertSplineSimpleInteractive :
    Pattern.Pattern
    -> Objects
    -> SplineData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertSplineSimpleInteractive pattern objects data id =
    Maybe.map2
        (\point1 point4 ->
            Maybe.map2
                (\controlPoint1 controlPoint4 ->
                    let
                        ( nextPatternA, thatControlPoint1 ) =
                            Pattern.insertPoint Nothing controlPoint1 pattern

                        ( nextPatternB, thatControlPoint4 ) =
                            Pattern.insertPoint Nothing controlPoint4 nextPatternA
                    in
                    case
                        Pattern.cubic nextPatternB
                            point1
                            thatControlPoint1
                            thatControlPoint4
                            point4
                    of
                        Nothing ->
                            Nothing

                        Just curve ->
                            let
                                ( finalPattern, thatCurve ) =
                                    Pattern.insertCurve Nothing curve nextPatternB
                            in
                            Just
                                ( finalPattern
                                , { objects
                                    | curves = Dict.insert id [ thatCurve ] objects.curves
                                  }
                                )
                )
                (Maybe.map2 (Pattern.atAngle pattern point1)
                    (Maybe.map fixRotation data.angle1)
                    (Maybe.map replaceLineExprs data.length1)
                    |> Maybe.withDefault Nothing
                )
                (Maybe.map2 (Pattern.atAngle pattern point4)
                    (Maybe.map fixRotation data.angle2)
                    (Maybe.map replaceLineExprs data.length2)
                    |> Maybe.withDefault Nothing
                )
                |> Maybe.withDefault Nothing
        )
        (Maybe.andThen (lookUp objects.points) data.point1)
        (Maybe.andThen (lookUp objects.points) data.point4)
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ( pattern, objects )


insertSplinePathInteractive :
    Pattern.Pattern
    -> Objects
    -> SplineData
    -> Int
    -> ( Pattern.Pattern, Objects )
insertSplinePathInteractive pattern objects data id =
    case data.pathPoints of
        [] ->
            ( pattern, objects )

        pathPoint :: otherPathPoints ->
            insertSplinePathInteractiveHelp pattern objects id pathPoint otherPathPoints


insertSplinePathInteractiveHelp :
    Pattern.Pattern
    -> Objects
    -> Int
    -> PathPoint
    -> List PathPoint
    -> ( Pattern.Pattern, Objects )
insertSplinePathInteractiveHelp pattern objects id pathPoint otherPathPoints =
    case otherPathPoints of
        [] ->
            ( pattern, objects )

        nextPathPoint :: remainingPathPoints ->
            Maybe.map2
                (\startPoint endPoint ->
                    Maybe.map2
                        (\startControlPoint endControlPoint ->
                            let
                                ( nextPatternA, thatStartControlPoint ) =
                                    Pattern.insertPoint Nothing
                                        startControlPoint
                                        pattern

                                ( nextPatternB, thatEndControlPoint ) =
                                    Pattern.insertPoint Nothing
                                        endControlPoint
                                        nextPatternA
                            in
                            case
                                Pattern.cubic nextPatternB
                                    startPoint
                                    thatStartControlPoint
                                    thatEndControlPoint
                                    endPoint
                            of
                                Nothing ->
                                    Nothing

                                Just curve ->
                                    let
                                        ( finalPattern, thatCurve ) =
                                            Pattern.insertCurve Nothing curve nextPatternB
                                    in
                                    Just
                                        ( finalPattern
                                        , { objects
                                            | curves =
                                                Dict.update id
                                                    (\maybeCurves ->
                                                        case maybeCurves of
                                                            Nothing ->
                                                                Just [ thatCurve ]

                                                            Just curves ->
                                                                Just (thatCurve :: curves)
                                                    )
                                                    objects.curves
                                          }
                                        )
                        )
                        (Maybe.map2 (Pattern.atAngle pattern startPoint)
                            (Maybe.map fixRotation pathPoint.angle2)
                            (Maybe.map replaceLineExprs pathPoint.length2)
                            |> Maybe.withDefault Nothing
                        )
                        (Maybe.map2 (Pattern.atAngle pattern endPoint)
                            (Maybe.map fixRotation nextPathPoint.angle1)
                            (Maybe.map replaceLineExprs nextPathPoint.length1)
                            |> Maybe.withDefault Nothing
                        )
                        |> Maybe.withDefault Nothing
                )
                (Maybe.andThen (lookUp objects.points) pathPoint.pSpline)
                (Maybe.andThen (lookUp objects.points) nextPathPoint.pSpline)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault ( pattern, objects )
                |> (\( nextPattern, nextObjects ) ->
                        insertSplinePathInteractiveHelp
                            nextPattern
                            nextObjects
                            id
                            nextPathPoint
                            remainingPathPoints
                   )



-- HELPER


lookUp : Dict Int (That object) -> Int -> Maybe (That object)
lookUp objects id =
    Dict.get id objects


pointName : Pattern.Pattern -> Objects -> Int -> Maybe String
pointName pattern objects id =
    Dict.get id objects.points
        |> Maybe.andThen (Pattern.getPoint pattern)
        |> Maybe.andThen .name



---- HELPER


fixRotation : String -> String
fixRotation angle =
    "-1 * (\n  " ++ angle ++ "\n)"


replaceLineExprs : String -> String
replaceLineExprs string =
    let
        replace { offsetStart, offsetEnd, pointA, pointB } previousString =
            String.replaceSlice
                (String.concat
                    [ "distance(\n  "
                    , pointA
                    , ",\n  "
                    , pointB
                    , "\n)"
                    ]
                )
                offsetStart
                offsetEnd
                previousString
    in
    case Parser.run lengthParser string of
        Err _ ->
            string

        Ok lineExprs ->
            List.foldl replace string lineExprs



---- PARSER


type alias LineExpr =
    { offsetStart : Int
    , pointA : String
    , pointB : String
    , offsetEnd : Int
    }


lengthParser : Parser (List LineExpr)
lengthParser =
    Parser.succeed identity
        |. Parser.chompWhile (\c -> c /= 'L')
        |= Parser.oneOf
            [ lineExprParser
                |> Parser.andThen
                    (\lineExpr ->
                        lengthParserHelp [ lineExpr ]
                    )
            , Parser.chompIf (\c -> c == 'L')
                |> Parser.andThen
                    (\_ ->
                        lengthParserHelp []
                    )
            , Parser.succeed []
                |. Parser.end
            ]


lengthParserHelp : List LineExpr -> Parser (List LineExpr)
lengthParserHelp lineExprs =
    Parser.succeed identity
        |. Parser.chompWhile (\c -> c /= 'L')
        |= Parser.oneOf
            [ lineExprParser
                |> Parser.andThen
                    (\lineExpr ->
                        lengthParserHelp (lineExpr :: lineExprs)
                    )
            , Parser.chompIf (\c -> c == 'L')
                |> Parser.andThen
                    (\_ ->
                        lengthParserHelp lineExprs
                    )
            , Parser.succeed lineExprs
                |. Parser.end
            ]


lineExprParser : Parser LineExpr
lineExprParser =
    let
        var =
            Parser.variable
                { start = isVarChar
                , inner = isVarChar
                , reserved = Set.empty
                }
    in
    Parser.succeed LineExpr
        |= Parser.getOffset
        |. Parser.token "Line_"
        |= var
        |. Parser.token "_"
        |= var
        |= Parser.getOffset


isVarChar : Char -> Bool
isVarChar char =
    [ ' '
    , '\t'
    , '\n'
    , ','
    , '+'
    , '-'
    , '*'
    , '/'
    , '('
    , ')'
    , '&'
    , '|'
    , '='
    , '>'
    , '<'
    , '_'
    ]
        |> List.member char
        |> not



---- DECODER


decode : String -> Result String Val
decode content =
    case Xml.parse content of
        Ok xml ->
            case xml.root of
                Xml.Element "pattern" _ nodes ->
                    getPatterns nodes

                _ ->
                    Err "There is an error in the XML structure."

        Err error ->
            Err "This is not a valid XML file."


getPatterns : List Xml.Node -> Result String { patterns : Nonempty Pattern }
getPatterns nodes =
    let
        required contentAt nodeName maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    contentAt nodes nodeName
                        |> Maybe.map toA

        possible contentAt nodeName maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    Just (toA (contentAt nodes nodeName))

        -- INCREMENTS
        increments maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    nodes
                        |> List.filterMap getIncrementsElement
                        |> List.map (List.filterMap getIncrement)
                        |> List.concat
                        |> Dict.fromList
                        |> toA
                        |> Just

        getIncrementsElement node =
            case node of
                Xml.Element "increments" [] subNodes ->
                    Just subNodes

                _ ->
                    Nothing

        getIncrement node =
            case node of
                Xml.Element "increment" attributes [] ->
                    Maybe.map3
                        (\name description formula ->
                            ( name
                            , Increment description formula
                            )
                        )
                        (stringAttributeAt attributes "name")
                        (stringAttributeAt attributes "description")
                        (stringAttributeAt attributes "formula")

                _ ->
                    Nothing

        -- DRAWS
        draws maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    nodes
                        |> List.filterMap getDrawElement
                        |> List.filterMap getDraw
                        |> Nonempty.fromList
                        |> Maybe.map toA

        getDrawElement node =
            case node of
                Xml.Element "draw" (attribute :: []) subNodes ->
                    if attribute.name == "name" then
                        Just
                            { name = attribute.value
                            , nodes = subNodes
                            }

                    else
                        Nothing

                _ ->
                    Nothing

        getDraw drawElement =
            drawElement.nodes
                |> calculations
                |> Nonempty.fromList
                |> Maybe.map (Draw drawElement.name)
    in
    case
        Just Pattern
            |> required stringAt "version"
            |> required unitAt "unit"
            |> possible stringAt "description"
            |> possible stringAt "notes"
            |> possible stringAt "patternName"
            |> possible stringAt "patternNumber"
            |> possible stringAt "company"
            |> possible stringAt "customer"
            |> possible stringAt "measurements"
            |> increments
            |> draws
    of
        Nothing ->
            Err "There is an error in the XML structure."

        Just pattern ->
            Ok { patterns = Nonempty.fromElement pattern }


calculations : List Xml.Node -> List Calculation
calculations nodes =
    let
        getCalculationElement node =
            case node of
                Xml.Element "calculation" [] subNodes ->
                    Just subNodes

                _ ->
                    Nothing
    in
    nodes
        |> List.filterMap getCalculationElement
        |> List.map (getObjects >> Dict.fromList)


getObjects : List Xml.Node -> List ( Int, Object )
getObjects =
    List.filterMap <|
        \node ->
            case node of
                Xml.Element "point" attributes [] ->
                    case intAttributeAt attributes "id" of
                        Nothing ->
                            Nothing

                        Just id ->
                            Maybe.map (Point >> Tuple.pair id)
                                (getPointData attributes)

                Xml.Element "line" attributes [] ->
                    case intAttributeAt attributes "id" of
                        Nothing ->
                            Nothing

                        Just id ->
                            Maybe.map (Line >> Tuple.pair id)
                                (getLineData attributes)

                Xml.Element "spline" attributes nodes ->
                    case intAttributeAt attributes "id" of
                        Nothing ->
                            Nothing

                        Just id ->
                            Maybe.map (Spline >> Tuple.pair id)
                                (getSplineData attributes nodes)

                _ ->
                    Nothing


getPointData : List Xml.Attribute -> Maybe PointData
getPointData attributes =
    let
        possible attributeAt attributeName maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    attributeAt attributes attributeName
                        |> toA
                        |> Just
    in
    Just PointData
        |> possible floatAttributeAt "x"
        |> possible floatAttributeAt "y"
        |> possible floatAttributeAt "mx"
        |> possible floatAttributeAt "my"
        |> possible stringAttributeAt "type"
        |> possible stringAttributeAt "name"
        |> possible intAttributeAt "firstPoint"
        |> possible intAttributeAt "secondPoint"
        |> possible intAttributeAt "thirdPoint"
        |> possible intAttributeAt "basePoint"
        |> possible intAttributeAt "pShoulder"
        |> possible intAttributeAt "p1Line"
        |> possible intAttributeAt "p2Line"
        |> possible stringAttributeAt "length"
        |> possible stringAttributeAt "angle"
        |> possible linePenStyleAttributeAt "typeLine"
        |> possible intAttributeAt "splinePath"
        |> possible intAttributeAt "spline"
        |> possible intAttributeAt "p1Line1"
        |> possible intAttributeAt "p1Line2"
        |> possible intAttributeAt "p2Line1"
        |> possible intAttributeAt "p2Line2"
        |> possible intAttributeAt "center"
        |> possible stringAttributeAt "radius"
        |> possible intAttributeAt "axisP1"
        |> possible intAttributeAt "axisP2"
        |> possible intAttributeAt "arc"
        |> possible intAttributeAt "elArc"
        |> possible intAttributeAt "curve"
        |> possible intAttributeAt "curve1"
        |> possible intAttributeAt "curve2"
        |> possible colorAttributeAt "lineColor"
        |> possible colorAttributeAt "color"
        |> possible intAttributeAt "firstArc"
        |> possible intAttributeAt "secondArc"
        |> possible crossTypeAttributeAt "crossPoint"
        |> possible crossTypeAttributeAt "vCrossPoint"
        |> possible crossTypeAttributeAt "hCrossPoint"
        |> possible intAttributeAt "c1Center"
        |> possible intAttributeAt "c2Center"
        |> possible stringAttributeAt "c1Radius"
        |> possible stringAttributeAt "c2Radius"
        |> possible stringAttributeAt "cRadius"
        |> possible intAttributeAt "tangent"
        |> possible intAttributeAt "cCenter"
        |> possible stringAttributeAt "name1"
        |> possible floatAttributeAt "mx1"
        |> possible floatAttributeAt "my1"
        |> possible stringAttributeAt "name2"
        |> possible floatAttributeAt "mx2"
        |> possible floatAttributeAt "my2"
        |> possible intAttributeAt "point1"
        |> possible intAttributeAt "point2"
        |> possible intAttributeAt "dartP1"
        |> possible intAttributeAt "dartP2"
        |> possible intAttributeAt "dartP3"
        |> possible intAttributeAt "baseLineP1"
        |> possible intAttributeAt "baseLineP2"


getLineData : List Xml.Attribute -> Maybe LineData
getLineData attributes =
    let
        possible attributeAt attributeName maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    attributeAt attributes attributeName
                        |> toA
                        |> Just
    in
    Just LineData
        |> possible intAttributeAt "firstPoint"
        |> possible intAttributeAt "secondPoint"
        |> possible linePenStyleAttributeAt "typeLine"
        |> possible colorAttributeAt "lineColor"


getSplineData : List Xml.Attribute -> List Xml.Node -> Maybe SplineData
getSplineData attributes nodes =
    let
        possible attributeAt attributeName maybeToA =
            case maybeToA of
                Nothing ->
                    Nothing

                Just toA ->
                    attributeAt attributes attributeName
                        |> toA
                        |> Just

        pathPoints =
            List.filterMap getPathPoint
                nodes
    in
    Just (SplineData pathPoints)
        |> possible floatAttributeAt "kCurve"
        |> possible stringAttributeAt "type"
        |> possible floatAttributeAt "kAsm1"
        |> possible floatAttributeAt "kAsm2"
        |> possible stringAttributeAt "angle1"
        |> possible stringAttributeAt "angle2"
        |> possible stringAttributeAt "length1"
        |> possible stringAttributeAt "length2"
        |> possible intAttributeAt "point1"
        |> possible intAttributeAt "point2"
        |> possible intAttributeAt "point3"
        |> possible intAttributeAt "point4"
        |> possible colorAttributeAt "color"
        |> possible curvePenStyleAttributeAt "penStyle"
        |> possible intAttributeAt "duplicate"


getPathPoint : Xml.Node -> Maybe PathPoint
getPathPoint node =
    case node of
        Xml.Element "pathPoint" attributes [] ->
            let
                possible attributeAt attributeName maybeToA =
                    case maybeToA of
                        Nothing ->
                            Nothing

                        Just toA ->
                            attributeAt attributes attributeName
                                |> toA
                                |> Just
            in
            Just PathPoint
                |> possible stringAttributeAt "kAsm2"
                |> possible intAttributeAt "pSpline"
                |> possible stringAttributeAt "angle"
                |> possible stringAttributeAt "angle1"
                |> possible stringAttributeAt "angle2"
                |> possible stringAttributeAt "length1"
                |> possible stringAttributeAt "length2"
                |> possible stringAttributeAt "kAsm1"

        _ ->
            Nothing



---- HELPER


stringAt : List Xml.Node -> String -> Maybe String
stringAt nodes nodeName =
    let
        hasName node =
            case node of
                Xml.Element name _ _ ->
                    name == nodeName

                Xml.Text _ ->
                    False

        getText node =
            case node of
                Xml.Element _ _ ((Xml.Text string) :: []) ->
                    Just string

                _ ->
                    Nothing
    in
    nodes
        |> List.find hasName
        |> Maybe.andThen getText


unitAt : List Xml.Node -> String -> Maybe Unit
unitAt nodes nodeName =
    let
        hasName node =
            case node of
                Xml.Element name _ _ ->
                    name == nodeName

                Xml.Text _ ->
                    False

        getUnit node =
            case node of
                Xml.Element _ _ ((Xml.Text string) :: []) ->
                    case string of
                        "mm" ->
                            Just Mm

                        "cm" ->
                            Just Cm

                        "inch" ->
                            Just Inch

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    nodes
        |> List.find hasName
        |> Maybe.andThen getUnit


intAttributeAt : List Xml.Attribute -> String -> Maybe Int
intAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName

        getInt attribute =
            String.toInt attribute.value
    in
    attributes
        |> List.find hasName
        |> Maybe.andThen getInt


floatAttributeAt : List Xml.Attribute -> String -> Maybe Float
floatAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName

        getFloat attribute =
            String.toFloat attribute.value
    in
    attributes
        |> List.find hasName
        |> Maybe.andThen getFloat


stringAttributeAt : List Xml.Attribute -> String -> Maybe String
stringAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName
    in
    attributes
        |> List.find hasName
        |> Maybe.map .value


linePenStyleAttributeAt : List Xml.Attribute -> String -> Maybe LinePenStyle
linePenStyleAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName

        getLinePenStyle attribute =
            case attribute.value of
                "none" ->
                    Just None

                "hair" ->
                    Just Hair

                "dashLine" ->
                    Just DashLine

                "dotLine" ->
                    Just DotLine

                "dashDotLine" ->
                    Just DashDotLine

                "dashDotDotLine" ->
                    Just DashDotDotLine

                _ ->
                    Nothing
    in
    attributes
        |> List.find hasName
        |> Maybe.andThen getLinePenStyle


curvePenStyleAttributeAt : List Xml.Attribute -> String -> Maybe CurvePenStyle
curvePenStyleAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName

        getCurvePenStyle attribute =
            case attribute.value of
                "hair" ->
                    Just CurvePenStyleHair

                "dashLine" ->
                    Just CurvePenStyleDashLine

                "dotLine" ->
                    Just CurvePenStyleDotLine

                "dashDotLine" ->
                    Just CurvePenStyleDashDotLine

                "dashDotDotLine" ->
                    Just CurvePenStyleDashDotDotLine

                _ ->
                    Nothing
    in
    attributes
        |> List.find hasName
        |> Maybe.andThen getCurvePenStyle


colorAttributeAt : List Xml.Attribute -> String -> Maybe Color
colorAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName

        getColor attribute =
            case attribute.value of
                "black" ->
                    Just Black

                "green" ->
                    Just Green

                "blue" ->
                    Just Blue

                "darkRed" ->
                    Just DarkRed

                "darkGreen" ->
                    Just DarkGreen

                "darkBlue" ->
                    Just DarkBlue

                "yellow" ->
                    Just Yellow

                "lightsalmon" ->
                    Just Lightsalmon

                "goldenrod" ->
                    Just Goldenrod

                "orange" ->
                    Just Orange

                "deeppink" ->
                    Just Deeppink

                "violet" ->
                    Just Violet

                "darkviolet" ->
                    Just Darkviolet

                "mediumseagreen" ->
                    Just Mediumseagreen

                "lime" ->
                    Just Lime

                "deepskyblue" ->
                    Just Deepskyblue

                "cornflowerblue" ->
                    Just Cornflowerblue

                _ ->
                    Nothing
    in
    attributes
        |> List.find hasName
        |> Maybe.andThen getColor


crossTypeAttributeAt : List Xml.Attribute -> String -> Maybe CrossType
crossTypeAttributeAt attributes attributeName =
    let
        hasName attribute =
            attribute.name == attributeName

        getCrossType attribute =
            case attribute.value of
                "1" ->
                    Just Cross1

                "2" ->
                    Just Cross2

                _ ->
                    Nothing
    in
    attributes
        |> List.find hasName
        |> Maybe.andThen getCrossType
