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


type LinePenStyle
    = None
    | Hair
    | DashLine
    | DotLine
    | DashDotLine
    | DashDotDotLine


type CurvePenStyle
    = CurvePenStyleHair
    | CurvePenStyleDashLine
    | CurvePenStyleDotLine
    | CurvePenStyleDashDotLine
    | CurvePenStyleDashDotDotLine


type CrossType
    = Cross1
    | Cross2


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



---- IMPORT


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
            Nonempty.head draws
                |> .calculations
                |> Nonempty.head
                |> Dict.toList
                |> List.foldl
                    (\( id, object ) ( previousPattern, previousObjects ) ->
                        let
                            lookUpPoint maybeId =
                                case maybeId of
                                    Nothing ->
                                        Nothing

                                    Just id_ ->
                                        Dict.get id_ previousObjects.points

                            lookUpLine maybeId =
                                case maybeId of
                                    Nothing ->
                                        Nothing

                                    Just id_ ->
                                        Dict.get id_ previousObjects.lines

                            nameOfPoint maybeId =
                                case maybeId of
                                    Nothing ->
                                        Nothing

                                    Just id_ ->
                                        Dict.get id_ previousObjects.points
                                            |> Maybe.andThen (Pattern.getPoint previousPattern)
                                            |> Maybe.andThen .name
                        in
                        case object of
                            Point data ->
                                let
                                    insertPoint point =
                                        let
                                            ( nextPattern, thatPoint ) =
                                                Pattern.insertPoint data.name
                                                    point
                                                    previousPattern
                                        in
                                        ( nextPattern
                                        , { previousObjects
                                            | points =
                                                Dict.insert id
                                                    thatPoint
                                                    previousObjects.points
                                          }
                                        )

                                    insertLine line =
                                        let
                                            ( nextPattern, thatLine ) =
                                                Pattern.insertLine Nothing
                                                    line
                                                    previousPattern
                                        in
                                        ( nextPattern
                                        , { previousObjects
                                            | lines =
                                                Dict.insert id
                                                    thatLine
                                                    previousObjects.lines
                                          }
                                        )
                                in
                                case data.type_ of
                                    Just "single" ->
                                        Maybe.map2 (\x y -> Pattern.origin { x = x, y = y })
                                            data.x
                                            data.y
                                            |> Maybe.map insertPoint
                                            |> Maybe.withDefault
                                                ( previousPattern, previousObjects )

                                    Just "endLine" ->
                                        Maybe.map3 (Pattern.atAngle previousPattern)
                                            (lookUpPoint data.basePoint)
                                            (Maybe.map replaceLineExprs data.angle
                                                |> Maybe.map
                                                    (\angle ->
                                                        "-1 * (" ++ angle ++ ")"
                                                    )
                                            )
                                            (Maybe.map replaceLineExprs data.length)
                                            |> Maybe.withDefault Nothing
                                            |> Maybe.map insertPoint
                                            |> Maybe.withDefault
                                                ( previousPattern, previousObjects )

                                    Just "alongLine" ->
                                        Maybe.map3
                                            (Pattern.betweenLength previousPattern)
                                            (lookUpPoint data.firstPoint)
                                            (lookUpPoint data.secondPoint)
                                            (Maybe.map3
                                                (\length firstName secondName ->
                                                    String.replace
                                                        "CurrentLength"
                                                        ("distance(\n  "
                                                            ++ firstName
                                                            ++ ",\n  "
                                                            ++ secondName
                                                            ++ ")"
                                                        )
                                                        length
                                                )
                                                (Maybe.map replaceLineExprs data.length)
                                                (nameOfPoint data.firstPoint)
                                                (nameOfPoint data.secondPoint)
                                            )
                                            |> Maybe.withDefault Nothing
                                            |> Maybe.map insertPoint
                                            |> Maybe.withDefault
                                                ( previousPattern, previousObjects )

                                    Just "normal" ->
                                        Maybe.map3 (Pattern.atAngle previousPattern)
                                            (lookUpPoint data.firstPoint)
                                            (Maybe.map3
                                                (\angle firstName secondName ->
                                                    "angleOfLine(\n  "
                                                        ++ firstName
                                                        ++ ",\n  "
                                                        ++ secondName
                                                        ++ "\n) - 90 + -1 * ("
                                                        ++ angle
                                                        ++ ")"
                                                )
                                                (Maybe.map replaceLineExprs data.angle)
                                                (nameOfPoint data.firstPoint)
                                                (nameOfPoint data.secondPoint)
                                            )
                                            (Maybe.map replaceLineExprs data.length)
                                            |> Maybe.withDefault Nothing
                                            |> Maybe.map insertPoint
                                            |> Maybe.withDefault
                                                ( previousPattern, previousObjects )

                                    Just "lineIntersectAxis" ->
                                        Maybe.map4
                                            (\basePoint angle p1Line p2Line ->
                                                Maybe.map2
                                                    (\lineA lineB ->
                                                        let
                                                            ( nextPatternA, thatLineA ) =
                                                                Pattern.insertLine Nothing lineA previousPattern

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
                                                                    , { previousObjects
                                                                        | points = Dict.insert id thatPoint previousObjects.points
                                                                      }
                                                                    )
                                                    )
                                                    (Pattern.throughOnePoint previousPattern basePoint angle)
                                                    (Pattern.throughTwoPoints previousPattern p1Line p2Line)
                                                    |> Maybe.withDefault Nothing
                                            )
                                            (lookUpPoint data.basePoint)
                                            (Maybe.map replaceLineExprs data.angle
                                                |> Maybe.map
                                                    (\angle ->
                                                        "-1 * (" ++ angle ++ ")"
                                                    )
                                            )
                                            (lookUpPoint data.p1Line)
                                            (lookUpPoint data.p2Line)
                                            |> Maybe.withDefault Nothing
                                            |> Maybe.withDefault ( previousPattern, previousObjects )

                                    Just "pointOfIntersection" ->
                                        Maybe.map2
                                            (\firstPoint secondPoint ->
                                                Maybe.map2
                                                    (\lineA lineB ->
                                                        let
                                                            ( nextPatternA, thatLineA ) =
                                                                Pattern.insertLine Nothing lineA previousPattern

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
                                                                    , { previousObjects
                                                                        | points = Dict.insert id thatPoint previousObjects.points
                                                                      }
                                                                    )
                                                    )
                                                    (Pattern.throughOnePoint previousPattern firstPoint "90")
                                                    (Pattern.throughOnePoint previousPattern secondPoint "0")
                                                    |> Maybe.withDefault Nothing
                                            )
                                            (lookUpPoint data.firstPoint)
                                            (lookUpPoint data.secondPoint)
                                            |> Maybe.withDefault Nothing
                                            |> Maybe.withDefault ( previousPattern, previousObjects )

                                    _ ->
                                        ( previousPattern, previousObjects )

                            Line data ->
                                let
                                    insertLine line =
                                        let
                                            ( nextPattern, thatLine ) =
                                                Pattern.insertLine Nothing
                                                    line
                                                    previousPattern
                                        in
                                        ( nextPattern
                                        , { previousObjects
                                            | lines =
                                                Dict.insert id
                                                    thatLine
                                                    previousObjects.lines
                                          }
                                        )
                                in
                                Maybe.map2 (Pattern.throughTwoPoints previousPattern)
                                    (lookUpPoint data.firstPoint)
                                    (lookUpPoint data.secondPoint)
                                    |> Maybe.withDefault Nothing
                                    |> Maybe.map insertLine
                                    |> Maybe.withDefault
                                        ( previousPattern, previousObjects )

                            Spline data ->
                                case data.type_ of
                                    Just "simpleInteractive" ->
                                        Maybe.map2
                                            (\point1 point4 ->
                                                Maybe.map2
                                                    (\controlPoint1 controlPoint4 ->
                                                        let
                                                            ( nextPatternA, thatControlPoint1 ) =
                                                                Pattern.insertPoint Nothing controlPoint1 previousPattern

                                                            ( nextPatternB, thatControlPoint4 ) =
                                                                Pattern.insertPoint Nothing controlPoint4 nextPatternA
                                                        in
                                                        case Pattern.cubic nextPatternB point1 thatControlPoint1 thatControlPoint4 point4 of
                                                            Nothing ->
                                                                Nothing

                                                            Just curve ->
                                                                let
                                                                    ( finalPattern, thatCurve ) =
                                                                        Pattern.insertCurve Nothing curve nextPatternB
                                                                in
                                                                Just
                                                                    ( finalPattern
                                                                    , { previousObjects
                                                                        | curves = Dict.insert id thatCurve previousObjects.curves
                                                                      }
                                                                    )
                                                    )
                                                    (Maybe.map2 (Pattern.atAngle previousPattern point1)
                                                        (data.angle1
                                                            |> Maybe.map
                                                                (\angle ->
                                                                    "-1 * (" ++ angle ++ ")"
                                                                )
                                                        )
                                                        (Maybe.map replaceLineExprs data.length1)
                                                        |> Maybe.withDefault Nothing
                                                    )
                                                    (Maybe.map2 (Pattern.atAngle previousPattern point4)
                                                        (data.angle2
                                                            |> Maybe.map
                                                                (\angle ->
                                                                    "-1 * (" ++ angle ++ ")"
                                                                )
                                                        )
                                                        (Maybe.map replaceLineExprs data.length2)
                                                        |> Maybe.withDefault Nothing
                                                    )
                                                    |> Maybe.withDefault Nothing
                                            )
                                            (lookUpPoint data.point1)
                                            (lookUpPoint data.point4)
                                            |> Maybe.withDefault Nothing
                                            |> Maybe.withDefault ( previousPattern, previousObjects )

                                    _ ->
                                        ( previousPattern, previousObjects )
                    )
                    ( pattern
                    , { points = Dict.empty
                      , lines = Dict.empty
                      , curves = Dict.empty
                      }
                    )
                |> Tuple.first
    in
    Pattern.empty
        |> insertVariables
        |> insertObjects
        |> Pattern.computeCache


replaceLineExprs : String -> String
replaceLineExprs string =
    case Parser.run lengthParser string of
        Err _ ->
            string

        Ok lineExprs ->
            List.foldl
                (\{ offsetStart, offsetEnd, pointA, pointB } previousString ->
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
                )
                string
                lineExprs


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

                _ ->
                    Err "There is an error in the XML structure."

        Err error ->
            Err "This is not a valid XML file."


calculations : List Xml.Node -> List Calculation
calculations nodes =
    let
        getCalculationElement node =
            case node of
                Xml.Element "calculation" [] subNodes ->
                    Just subNodes

                _ ->
                    Nothing

        getCalculation =
            List.filterMap <|
                \node ->
                    case node of
                        Xml.Element "point" attributes [] ->
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
                            case intAttributeAt attributes "id" of
                                Nothing ->
                                    Nothing

                                Just id ->
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
                                        |> Maybe.map (Point >> Tuple.pair id)

                        Xml.Element "line" attributes [] ->
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
                            case intAttributeAt attributes "id" of
                                Nothing ->
                                    Nothing

                                Just id ->
                                    Just LineData
                                        |> possible intAttributeAt "firstPoint"
                                        |> possible intAttributeAt "secondPoint"
                                        |> possible linePenStyleAttributeAt "typeLine"
                                        |> possible colorAttributeAt "lineColor"
                                        |> Maybe.map (Line >> Tuple.pair id)

                        Xml.Element "spline" attributes elements ->
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
                                    List.filterMap
                                        (\element ->
                                            case element of
                                                Xml.Element "pathPoint" pathPointAttributes [] ->
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
                                        )
                                        elements
                            in
                            case intAttributeAt attributes "id" of
                                Nothing ->
                                    Nothing

                                Just id ->
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
                                        |> Maybe.map (Spline >> Tuple.pair id)

                        _ ->
                            Nothing
    in
    nodes
        |> List.filterMap getCalculationElement
        |> List.map (getCalculation >> Dict.fromList)


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
