port module Main exposing (main)

import Axis2d exposing (Axis2d)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Pattern exposing (Detail, Entry, Line, Pattern, Point, That, Those)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Url exposing (Url)


main : Program {} Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


port safePattern : Value -> Cmd msg


port requestPattern : () -> Cmd msg


port patternReceived : (Value -> msg) -> Sub msg



---- MODEL


type alias Model =
    { pattern : Pattern
    , tool : Maybe Tool
    }


type Tool
    = -- POINTS
      LeftOf (Maybe (That Point)) String
    | RightOf (Maybe (That Point)) String
    | Above (Maybe (That Point)) String
    | Below (Maybe (That Point)) String
    | AtAngle
      -- LINES
    | ThroughTwoPoints (Maybe (That Point)) (Maybe (That Point))
      -- TRANSFORMATIONS
    | MirrorAt (Maybe (That Line)) (Those Point)
      -- DETAILS
    | CounterClockwise (List (That Point))



-- TR


init : {} -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { pattern =
            Pattern.empty
                |> Pattern.insertPoint Pattern.Origin
      , tool = Nothing
      }
    , requestPattern ()
    )



---- VIEW


view : Model -> Document Msg
view model =
    { title = "Sewing Pattern Editor"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (viewEditor model)
        ]
    }


viewEditor : Model -> Element Msg
viewEditor model =
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (Element.html <|
                Svg.svg
                    [ Attributes.viewBox "-320 -320 640 640" ]
                    (viewPattern model.pattern)
            )
        , Element.column
            [ Element.height Element.fill
            , Border.color (Element.rgb 0.3 0.3 0.3)
            , Border.width 1
            ]
            [ Element.row
                [ Element.padding 10
                , Element.spacing 5
                ]
                [ button "left of" LeftOfClicked
                , button "right of" RightOfClicked
                , button "above" AboveClicked
                , button "below" BelowClicked
                , button "at angle" AtAngleClicked
                ]
            , Element.row
                [ Element.padding 10
                , Element.spacing 5
                ]
                [ button "through two points" ThroughTwoPointsClicked
                ]
            , Element.row
                [ Element.padding 10
                , Element.spacing 5
                ]
                [ button "mirror at" MirrorAtClicked
                ]
            , Element.row
                [ Element.padding 10
                , Element.spacing 5
                ]
                [ button "counter clockwise" CounterClockwiseClicked
                ]
            , horizontalLine
            , model.tool
                |> Maybe.map
                    (viewTool
                        model.pattern
                        (Pattern.points model.pattern)
                        (Pattern.lines model.pattern)
                    )
                |> Maybe.withDefault Element.none
            ]
        ]


viewTool :
    Pattern
    -> List ( That Point, Entry Point )
    -> List ( That Line, Entry Line )
    -> Tool
    -> Element Msg
viewTool pattern points lines tool =
    let
        pointOption ( thatPoint, { name } ) =
            Input.option thatPoint (Element.text (Maybe.withDefault "<unnamed>" name))

        lineOption ( thatLine, { name } ) =
            Input.option thatLine (Element.text (Maybe.withDefault "<unnamed>" name))

        simpleDistanceTool anchor distance =
            Element.column
                [ Element.padding 10
                , Element.spacing 10
                ]
                [ anchorSelection anchor "anchor" AnchorChanged
                , Input.text []
                    { onChange = Just DistanceChanged
                    , text = distance
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (Element.text "distance")
                    }
                , button "create" CreateClicked
                ]

        anchorSelection anchor label msg =
            Input.radio []
                { onChange = Just msg
                , selected = anchor
                , label = Input.labelAbove [] (Element.text label)
                , options = List.map pointOption points
                }
    in
    case tool of
        LeftOf anchor distance ->
            simpleDistanceTool anchor distance

        RightOf anchor distance ->
            simpleDistanceTool anchor distance

        Above anchor distance ->
            simpleDistanceTool anchor distance

        Below anchor distance ->
            simpleDistanceTool anchor distance

        AtAngle ->
            Debug.todo ""

        ThroughTwoPoints anchorA anchorB ->
            Element.column
                [ Element.padding 10
                , Element.spacing 10
                ]
                [ anchorSelection anchorA "anchor a" AnchorAChanged
                , anchorSelection anchorB "anchor b" AnchorBChanged
                , button "create" CreateClicked
                ]

        MirrorAt line targets ->
            let
                pointCheckbox ( thatPoint, { name } ) =
                    Input.checkbox []
                        { onChange = Just (PointChecked thatPoint)
                        , icon = Nothing
                        , checked = Pattern.memberOfThose thatPoint targets
                        , label =
                            Input.labelRight [] <|
                                Element.text (Maybe.withDefault "<unnamed>" name)
                        }
            in
            Element.column
                [ Element.padding 10
                , Element.spacing 10
                ]
                [ Input.radio []
                    { onChange = Just LineChanged
                    , selected = line
                    , label = Input.labelAbove [] (Element.text "line")
                    , options = List.map lineOption lines
                    }
                , Element.text "targets"
                , Element.column [] <|
                    List.map pointCheckbox points
                , button "create" CreateClicked
                ]

        CounterClockwise targets ->
            let
                pointButton ( thatPoint, { name } ) =
                    button (Maybe.withDefault "<unnamed>" name) (PointAdded thatPoint)
            in
            Element.column
                [ Element.padding 10
                , Element.spacing 10
                ]
                [ Element.text
                    (targets
                        |> List.filterMap (Pattern.getPoint pattern)
                        |> List.map (.name >> Maybe.withDefault "<unnamed>")
                        |> String.join ", "
                    )
                , Element.column [] <|
                    List.map pointButton points
                , button "create" CreateClicked
                ]


button : String -> msg -> Element msg
button label msg =
    Input.button
        [ Background.color (Element.rgb 0.9 0.9 0.9)
        , Element.padding 10
        ]
        { onPress = Just msg
        , label = Element.text label
        }


horizontalLine : Element msg
horizontalLine =
    Element.el
        [ Element.height (Element.px 1)
        , Element.width Element.fill
        , Background.color (Element.rgb 0.3 0.3 0.3)
        ]
        Element.none



---- SVG


viewPattern : Pattern -> List (Svg msg)
viewPattern pattern =
    let
        ( geometry, problems ) =
            Pattern.geometry pattern
    in
    List.concat
        [ List.map viewLine geometry.lines
        , List.map viewDetail geometry.details
        , List.map viewPoint geometry.points
        ]


viewPoint : ( That Point, Maybe String, Point2d ) -> Svg msg
viewPoint ( thatPoint, maybeName, point2d ) =
    let
        ( x, y ) =
            Point2d.coordinates point2d
    in
    Svg.circle
        [ Attributes.cx (String.fromFloat x)
        , Attributes.cy (String.fromFloat y)
        , Attributes.r "2"
        ]
        []


viewLine : ( That Line, Maybe String, Axis2d ) -> Svg msg
viewLine ( thatLine, maybeName, axis2d ) =
    let
        ( x1, y1 ) =
            -1000
                |> Point2d.along axis2d
                |> Point2d.coordinates

        ( x2, y2 ) =
            1000
                |> Point2d.along axis2d
                |> Point2d.coordinates
    in
    Svg.line
        [ Attributes.x1 (String.fromFloat x1)
        , Attributes.y1 (String.fromFloat y1)
        , Attributes.x2 (String.fromFloat x2)
        , Attributes.y2 (String.fromFloat y2)
        , Attributes.stroke "black"
        ]
        []


viewDetail : ( That Detail, Maybe String, Polygon2d ) -> Svg msg
viewDetail ( thatDetail, maybeName, polygon2d ) =
    let
        printCoordinates ( x, y ) =
            String.concat
                [ String.fromFloat x
                , ","
                , String.fromFloat y
                ]
    in
    Svg.polygon
        [ Attributes.points
            (polygon2d
                |> Polygon2d.outerLoop
                |> List.map (Point2d.coordinates >> printCoordinates)
                |> String.join " "
            )
        , Attributes.fill "lightGrey"
        ]
        []



---- UPDATE


type Msg
    = NoOp
      -- POINTS
    | LeftOfClicked
    | RightOfClicked
    | AboveClicked
    | BelowClicked
    | AtAngleClicked
      -- LINES
    | ThroughTwoPointsClicked
      -- TRANSFORMATIONS
    | MirrorAtClicked
      -- DETAILS
    | CounterClockwiseClicked
      --
    | AnchorChanged (That Point)
    | AnchorAChanged (That Point)
    | AnchorBChanged (That Point)
    | DistanceChanged String
    | LineChanged (That Line)
    | PointChecked (That Point) Bool
    | PointAdded (That Point)
      --
    | CreateClicked
      -- STORAGE
    | PatternReceived Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- POINTS
        LeftOfClicked ->
            ( { model | tool = Just (LeftOf Nothing "") }
            , Cmd.none
            )

        RightOfClicked ->
            ( { model | tool = Just (RightOf Nothing "") }
            , Cmd.none
            )

        AboveClicked ->
            ( { model | tool = Just (Above Nothing "") }
            , Cmd.none
            )

        BelowClicked ->
            ( { model | tool = Just (Below Nothing "") }
            , Cmd.none
            )

        AtAngleClicked ->
            ( { model | tool = Just AtAngle }
            , Cmd.none
            )

        -- LINES
        ThroughTwoPointsClicked ->
            ( { model | tool = Just (ThroughTwoPoints Nothing Nothing) }
            , Cmd.none
            )

        -- TRANSFORMATIONS
        MirrorAtClicked ->
            ( { model | tool = Just (MirrorAt Nothing Pattern.none) }
            , Cmd.none
            )

        -- DETAILS
        CounterClockwiseClicked ->
            ( { model | tool = Just (CounterClockwise []) }
            , Cmd.none
            )

        -- TOOL PARAMETERS
        AnchorChanged newAnchor ->
            case model.tool of
                Just (LeftOf anchor distance) ->
                    ( { model | tool = Just (LeftOf (Just newAnchor) distance) }
                    , Cmd.none
                    )

                Just (RightOf anchor distance) ->
                    ( { model | tool = Just (RightOf (Just newAnchor) distance) }
                    , Cmd.none
                    )

                Just (Above anchor distance) ->
                    ( { model | tool = Just (Above (Just newAnchor) distance) }
                    , Cmd.none
                    )

                Just (Below anchor distance) ->
                    ( { model | tool = Just (Below (Just newAnchor) distance) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AnchorAChanged newAnchorA ->
            case model.tool of
                Just (ThroughTwoPoints anchorA anchorB) ->
                    ( { model | tool = Just (ThroughTwoPoints (Just newAnchorA) anchorB) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AnchorBChanged newAnchorB ->
            case model.tool of
                Just (ThroughTwoPoints anchorA anchorB) ->
                    ( { model | tool = Just (ThroughTwoPoints anchorA (Just newAnchorB)) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DistanceChanged newDistance ->
            case model.tool of
                Just (LeftOf anchor distance) ->
                    ( { model | tool = Just (LeftOf anchor newDistance) }
                    , Cmd.none
                    )

                Just (RightOf anchor distance) ->
                    ( { model | tool = Just (RightOf anchor newDistance) }
                    , Cmd.none
                    )

                Just (Above anchor distance) ->
                    ( { model | tool = Just (Above anchor newDistance) }
                    , Cmd.none
                    )

                Just (Below anchor distance) ->
                    ( { model | tool = Just (Below anchor newDistance) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        LineChanged newLine ->
            case model.tool of
                Just (MirrorAt line targets) ->
                    ( { model | tool = Just (MirrorAt (Just newLine) targets) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PointChecked thatPoint checked ->
            case model.tool of
                Just (MirrorAt line targets) ->
                    ( { model
                        | tool =
                            Just <|
                                MirrorAt line <|
                                    if checked then
                                        Pattern.insertIntoThose thatPoint targets
                                    else
                                        Pattern.removeFromThose thatPoint targets
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PointAdded thatPoint ->
            case model.tool of
                Just (CounterClockwise targets) ->
                    ( { model | tool = Just (CounterClockwise (thatPoint :: targets)) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        --
        CreateClicked ->
            let
                insertSimpleDistance constructor anchor distance =
                    case ( anchor, String.toFloat distance ) of
                        ( Just thatPoint, Just by ) ->
                            let
                                newPoint =
                                    constructor thatPoint
                                        (Pattern.Length (Pattern.exprFromFloat by))

                                newPattern =
                                    Pattern.insertPoint newPoint model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , tool = Nothing
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                lastState =
                    Pattern.lastState model.pattern
            in
            case model.tool of
                Just (LeftOf anchor distance) ->
                    insertSimpleDistance Pattern.LeftOf anchor distance

                Just (RightOf anchor distance) ->
                    insertSimpleDistance Pattern.RightOf anchor distance

                Just (Above anchor distance) ->
                    insertSimpleDistance Pattern.Above anchor distance

                Just (Below anchor distance) ->
                    insertSimpleDistance Pattern.Below anchor distance

                Just (ThroughTwoPoints anchorA anchorB) ->
                    case ( anchorA, anchorB ) of
                        ( Just thatPointA, Just thatPointB ) ->
                            let
                                newPoint =
                                    Pattern.ThroughTwoPoints
                                        thatPointA
                                        thatPointB

                                newPattern =
                                    Pattern.insertLine newPoint model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , tool = Nothing
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Just (MirrorAt line targets) ->
                    case line of
                        Just thatLine ->
                            let
                                newTransformation =
                                    Pattern.MirrorAt thatLine targets

                                newPattern =
                                    Pattern.insertTransformation newTransformation model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , tool = Nothing
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Just (CounterClockwise targets) ->
                    let
                        newDetail =
                            targets
                                |> Pattern.CounterClockwise

                        newPattern =
                            Pattern.insertDetail newDetail model.pattern
                    in
                    ( { model
                        | pattern = newPattern
                        , tool = Nothing
                      }
                    , safePattern (Pattern.encode newPattern)
                    )

                _ ->
                    ( model, Cmd.none )

        -- STORAGE
        PatternReceived value ->
            case Decode.decodeValue Pattern.decoder value of
                Err error ->
                    Debug.todo (Decode.errorToString error)

                Ok newPattern ->
                    ( { model | pattern = newPattern }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    patternReceived PatternReceived


onUrlRequest urlRequest =
    NoOp


onUrlChange url =
    NoOp
