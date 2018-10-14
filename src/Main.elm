port module Main exposing (main)

import Axis2d exposing (Axis2d)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Circle2d
import Color
import Direction2d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg as Svg
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode exposing (Value)
import LineSegment2d exposing (LineSegment2d)
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Pattern exposing (Detail, Line, LineSegment, Pattern, Point)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import QuadraticSpline2d
import Store exposing (Entry)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Events
import That exposing (That)
import Those exposing (Those)
import Url exposing (Url)
import Vector2d


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
    , hoveredPoint : Maybe (That Point)
    }


type Tool
    = -- POINTS
      LeftOf
        { name : String
        , dropdown : Dropdown
        , thatAnchor : Maybe (That Point)
        , distance : String
        }
    | RightOf
        { name : String
        , dropdown : Dropdown
        , thatAnchor : Maybe (That Point)
        , distance : String
        }
    | Above
        { name : String
        , dropdown : Dropdown
        , thatAnchor : Maybe (That Point)
        , distance : String
        }
    | Below
        { name : String
        , dropdown : Dropdown
        , thatAnchor : Maybe (That Point)
        , distance : String
        }
    | AtAngle
      -- LINES
    | ThroughTwoPoints
        { dropdownA : Dropdown
        , thatAnchorA : Maybe (That Point)
        , dropdownB : Dropdown
        , thatAnchorB : Maybe (That Point)
        }
      -- LINE SEGMENTS
    | FromTo
        { dropdownA : Dropdown
        , thatAnchorA : Maybe (That Point)
        , dropdownB : Dropdown
        , thatAnchorB : Maybe (That Point)
        }
      -- TRANSFORMATIONS
    | MirrorAt (Maybe (That Line)) (Those Point)
    | CutAlongLineSegment (Maybe (That LineSegment)) (Maybe (That Detail))
      -- DETAILS
    | CounterClockwise (List (That Point))


isLeftOf maybeTool =
    case maybeTool of
        Just (LeftOf _) ->
            True

        _ ->
            False


isRightOf maybeTool =
    case maybeTool of
        Just (RightOf _) ->
            True

        _ ->
            False


isAbove maybeTool =
    case maybeTool of
        Just (Above _) ->
            True

        _ ->
            False


isBelow maybeTool =
    case maybeTool of
        Just (Below _) ->
            True

        _ ->
            False


isAtAngle maybeTool =
    case maybeTool of
        Just AtAngle ->
            True

        _ ->
            False


isThroughTwoPoints maybeTool =
    case maybeTool of
        Just (ThroughTwoPoints _) ->
            True

        _ ->
            False


isFromTo maybeTool =
    case maybeTool of
        Just (FromTo _) ->
            True

        _ ->
            False


isMirrorAt maybeTool =
    case maybeTool of
        Just (MirrorAt _ _) ->
            True

        _ ->
            False


isCutAlongLineSegment maybeTool =
    case maybeTool of
        Just (CutAlongLineSegment _ _) ->
            True

        _ ->
            False


isCounterClockwise maybeTool =
    case maybeTool of
        Just (CounterClockwise _) ->
            True

        _ ->
            False


selectedPointsFromTool : Tool -> Those Point
selectedPointsFromTool tool =
    let
        empty =
            Those.fromList []
    in
    case tool of
        LeftOf { thatAnchor } ->
            thatAnchor
                |> maybeToList
                |> Those.fromList

        RightOf { thatAnchor } ->
            thatAnchor
                |> maybeToList
                |> Those.fromList

        Above { thatAnchor } ->
            thatAnchor
                |> maybeToList
                |> Those.fromList

        Below { thatAnchor } ->
            thatAnchor
                |> maybeToList
                |> Those.fromList

        AtAngle ->
            empty

        ThroughTwoPoints { thatAnchorA, thatAnchorB } ->
            [ thatAnchorA, thatAnchorB ]
                |> List.filterMap identity
                |> Those.fromList

        FromTo { thatAnchorA, thatAnchorB } ->
            [ thatAnchorA, thatAnchorB ]
                |> List.filterMap identity
                |> Those.fromList

        MirrorAt _ targets ->
            targets

        CutAlongLineSegment _ _ ->
            empty

        CounterClockwise targets ->
            Those.fromList targets


selectedLinesFromTool : Tool -> Those Line
selectedLinesFromTool tool =
    let
        empty =
            Those.fromList []
    in
    case tool of
        LeftOf _ ->
            empty

        RightOf _ ->
            empty

        Above _ ->
            empty

        Below _ ->
            empty

        AtAngle ->
            empty

        ThroughTwoPoints _ ->
            empty

        FromTo _ ->
            empty

        MirrorAt line _ ->
            line
                |> maybeToList
                |> Those.fromList

        CutAlongLineSegment _ _ ->
            empty

        CounterClockwise _ ->
            empty


selectedLineSegmentsFromTool : Tool -> Those LineSegment
selectedLineSegmentsFromTool tool =
    let
        empty =
            Those.fromList []
    in
    case tool of
        LeftOf _ ->
            empty

        RightOf _ ->
            empty

        Above _ ->
            empty

        Below _ ->
            empty

        AtAngle ->
            empty

        ThroughTwoPoints _ ->
            empty

        FromTo _ ->
            empty

        MirrorAt _ _ ->
            empty

        CutAlongLineSegment lineSegment _ ->
            lineSegment
                |> maybeToList
                |> Those.fromList

        CounterClockwise _ ->
            empty


selectedDetailsFromTool : Tool -> Those Detail
selectedDetailsFromTool tool =
    let
        empty =
            Those.fromList []
    in
    case tool of
        LeftOf _ ->
            empty

        RightOf _ ->
            empty

        Above _ ->
            empty

        Below _ ->
            empty

        AtAngle ->
            empty

        ThroughTwoPoints _ ->
            empty

        FromTo _ ->
            empty

        MirrorAt _ _ ->
            empty

        CutAlongLineSegment _ detail ->
            detail
                |> maybeToList
                |> Those.fromList

        CounterClockwise _ ->
            empty


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]


init : {} -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { pattern =
            Pattern.empty
                |> Pattern.insertPoint (Just "origin") Pattern.Origin
      , tool = Nothing
      , hoveredPoint = Nothing
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
            , Font.family
                [ Font.external
                    { name = "Dosis"
                    , url = "https://fonts.googleapis.com/css?family=Dosis"
                    }
                , Font.sansSerif
                ]
            ]
            (viewEditor model)
        ]
    }


viewEditor : Model -> Element Msg
viewEditor model =
    let
        selectedPoints =
            model.tool
                |> Maybe.map selectedPointsFromTool
                |> Maybe.withDefault (Those.fromList [])

        selectedLines =
            model.tool
                |> Maybe.map selectedLinesFromTool
                |> Maybe.withDefault (Those.fromList [])

        selectedLineSegments =
            model.tool
                |> Maybe.map selectedLineSegmentsFromTool
                |> Maybe.withDefault (Those.fromList [])

        selectedDetails =
            model.tool
                |> Maybe.map selectedDetailsFromTool
                |> Maybe.withDefault (Those.fromList [])
    in
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.inFront <|
            Element.column
                [ Element.alignRight
                , Element.moveLeft 10
                , Element.moveDown 10
                , Background.color (color Color.lightGray)
                , Border.rounded 4
                , Border.width 2
                , Border.color (color Color.lightGray)
                , Element.onLeft <|
                    case model.tool of
                        Nothing ->
                            Element.none

                        Just tool ->
                            Element.el
                                [ Element.alignTop
                                , Element.width (Element.px 400)
                                , Element.moveLeft 10
                                , Element.moveUp 2
                                , Background.color (color Color.lightGray)
                                , Border.rounded 4
                                , Border.width 2
                                , Border.color (color Color.lightGray)
                                ]
                                (viewTool
                                    model.pattern
                                    (Pattern.points model.pattern)
                                    (Pattern.lines model.pattern)
                                    (Pattern.lineSegments model.pattern)
                                    (Pattern.details model.pattern)
                                    tool
                                )
                ]
                [ viewToolSelector model.tool
                , horizontalLine
                , Element.row
                    [ Element.padding 5
                    , Element.spacing 5
                    , Element.width Element.fill
                    ]
                    [ buttonDanger "Clear pattern" ClearPatternClicked
                    ]
                ]
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (Element.html <|
                Svg.svg
                    [ Attributes.viewBox "-320 -320 640 640" ]
                    (drawPattern
                        model.hoveredPoint
                        selectedPoints
                        selectedLines
                        selectedLineSegments
                        selectedDetails
                        model.pattern
                    )
            )
        ]


viewTool :
    Pattern
    -> List ( That Point, Entry Point )
    -> List ( That Line, Entry Line )
    -> List ( That LineSegment, Entry LineSegment )
    -> List ( That Detail, Entry Detail )
    -> Tool
    -> Element Msg
viewTool pattern points lines lineSegments details tool =
    let
        simpleDistanceTool toolId newName dropdown anchor distance =
            Element.column
                [ Element.width Element.fill ]
                [ labeledInputText True NameChanged "Name:" newName
                , labeledDropdown
                    (Pattern.getPoint pattern
                        >> Maybe.andThen .name
                        >> Maybe.withDefault "<no name>"
                    )
                    DropdownMsg
                    (toolId ++ "-anchor")
                    "Anchor:"
                    points
                    dropdown
                    anchor
                , labeledInputText False DistanceChanged "Distance:" distance
                ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 5
        , Element.spacing 10
        ]
        [ case tool of
            LeftOf { name, dropdown, thatAnchor, distance } ->
                simpleDistanceTool "leftof" name dropdown thatAnchor distance

            RightOf { name, dropdown, thatAnchor, distance } ->
                simpleDistanceTool "rightof" name dropdown thatAnchor distance

            Above { name, dropdown, thatAnchor, distance } ->
                simpleDistanceTool "above" name dropdown thatAnchor distance

            Below { name, dropdown, thatAnchor, distance } ->
                simpleDistanceTool "below" name dropdown thatAnchor distance

            AtAngle ->
                Element.none

            ThroughTwoPoints { dropdownA, thatAnchorA, dropdownB, thatAnchorB } ->
                Element.column
                    [ Element.width Element.fill ]
                    [ labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        DropdownAMsg
                        "through-two-points-anchor-a"
                        "1st anchor:"
                        points
                        dropdownA
                        thatAnchorA
                    , labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        DropdownBMsg
                        "through-two-points-anchor-b"
                        "2st anchor:"
                        points
                        dropdownB
                        thatAnchorB
                    ]

            FromTo { dropdownA, thatAnchorA, dropdownB, thatAnchorB } ->
                Element.column
                    [ Element.width Element.fill ]
                    [ labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        DropdownAMsg
                        "from-to-anchor-a"
                        "1st anchor:"
                        points
                        dropdownA
                        thatAnchorA
                    , labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        DropdownBMsg
                        "from-to-anchor-b"
                        "2st anchor:"
                        points
                        dropdownB
                        thatAnchorB
                    ]

            MirrorAt line targets ->
                let
                    pointCheckbox ( thatPoint, { name } ) =
                        Input.checkbox
                            [ Element.width Element.fill ]
                            { onChange = PointChecked thatPoint
                            , icon = Input.defaultCheckbox
                            , checked = Those.member thatPoint targets
                            , label =
                                Input.labelRight []
                                    (Element.text (Maybe.withDefault "<unnamed>" name))
                            }
                in
                Element.column
                    [ Element.width Element.fill ]
                    [ labeledInputRadio LineChanged "Line:" line lines
                    , Element.column
                        [ Element.paddingXY 5 0 ]
                        [ Element.text "Targets:"
                        , Element.column
                            [ Element.padding 5
                            , Element.spacing 4
                            ]
                            (List.map pointCheckbox points)
                        ]
                    ]

            CutAlongLineSegment lineSegment detail ->
                Element.column
                    [ Element.width Element.fill ]
                    [ labeledInputRadio LineSegmentChanged "Line segment:" lineSegment lineSegments
                    , labeledInputRadio DetailChanged "Detail:" detail details
                    ]

            CounterClockwise targets ->
                let
                    pointButton ( thatPoint, { name } ) =
                        button "" (Maybe.withDefault "<unnamed>" name) (PointAdded thatPoint) False
                in
                Element.column
                    [ Element.width Element.fill ]
                    [ Element.text
                        (targets
                            |> List.filterMap (Pattern.getPoint pattern)
                            |> List.map (.name >> Maybe.withDefault "<unnamed>")
                            |> String.join ", "
                        )
                    , Element.column [] <|
                        List.map pointButton points
                    ]
        , Element.row
            [ Element.alignRight
            , Element.spacing 5
            ]
            [ buttonCreate "Create" CreateClicked
            , buttonDismiss "Cancel" CancelClicked
            ]
        ]


viewToolSelector maybeTool =
    Element.column
        [ Element.padding 5
        , Element.spacing 5
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ Element.el [ Font.size 16 ] (Element.text "Points:")
            , Element.column
                [ Element.spacing 5 ]
                [ Element.row
                    [ Element.spacing 5
                    , Element.width Element.fill
                    ]
                    [ button "left_of" "Left of" LeftOfClicked (isLeftOf maybeTool)
                    , button "right_of" "Right of" RightOfClicked (isRightOf maybeTool)
                    , button "above" "Above" AboveClicked (isAbove maybeTool)
                    , button "below" "Below" BelowClicked (isBelow maybeTool)
                    ]
                , Element.row
                    [ Element.spacing 5
                    , Element.width Element.fill
                    ]
                    [ button "at_angle" "At angle" AtAngleClicked (isAtAngle maybeTool)
                    ]
                ]
            ]
        , Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ Element.el [ Font.size 16 ] (Element.text "Lines:")
            , Element.column
                [ Element.spacing 5
                , Element.width Element.fill
                ]
                [ button "through_two_points" "Through two points" ThroughTwoPointsClicked (isThroughTwoPoints maybeTool)
                ]
            ]
        , Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ Element.el [ Font.size 16 ] (Element.text "Line segments:")
            , Element.column
                [ Element.spacing 5
                , Element.width Element.fill
                ]
                [ button "from_to" "From to" FromToClicked (isFromTo maybeTool)
                ]
            ]
        , Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ Element.el [ Font.size 16 ] (Element.text "Transformations:")
            , Element.row
                [ Element.spacing 5
                , Element.width Element.fill
                ]
                [ button "mirror_at" "Mirror at" MirrorAtClicked (isMirrorAt maybeTool)
                , button "cut_along_line_segment" "Cut along line segment" CutAlongLineSegmentClicked (isCutAlongLineSegment maybeTool)
                ]
            ]
        , Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ Element.el [ Font.size 16 ] (Element.text "Details:")
            , Element.column
                [ Element.spacing 5
                , Element.width Element.fill
                ]
                [ button "counter_clockwise" "Counter clockwise" CounterClockwiseClicked (isCounterClockwise maybeTool)
                ]
            ]
        ]



---- REUSABLE ELEMENTS


button : String -> String -> msg -> Bool -> Element msg
button iconSrc label msg selected =
    Input.button
        [ Element.padding 5
        , Background.color <|
            if selected then
                color Color.lightCharcoal

            else
                color Color.gray
        , Border.color (color Color.black)
        , Border.width 1
        , Border.rounded 3
        , Element.mouseOver
            [ Background.color <|
                if selected then
                    color Color.lightCharcoal

                else
                    color Color.darkGray
            ]
        ]
        { onPress = Just msg
        , label =
            Element.image
                [ Element.width (Element.px 48)
                , Element.height (Element.px 48)
                ]
                { src = "icons/" ++ iconSrc ++ ".svg"
                , description = label
                }
        }


buttonDismiss : String -> msg -> Element msg
buttonDismiss label msg =
    Input.button
        [ Element.paddingXY 8 7
        , Element.width Element.fill
        , Background.color (color Color.gray)
        , Border.color (color Color.black)
        , Border.width 1
        , Border.rounded 3
        , Element.mouseOver
            [ Background.color (color Color.darkGray) ]
        ]
        { onPress = Just msg
        , label = Element.text label
        }


buttonDanger : String -> msg -> Element msg
buttonDanger label msg =
    Input.button
        [ Element.paddingXY 8 7
        , Element.width Element.fill
        , Background.color (color Color.lightRed)
        , Font.color (color Color.darkCharcoal)
        , Border.color (color Color.black)
        , Border.width 1
        , Border.rounded 3
        , Element.mouseOver
            [ Background.color (color Color.red)
            , Font.color (color Color.black)
            ]
        ]
        { onPress = Just msg
        , label = Element.text label
        }


buttonCreate : String -> msg -> Element msg
buttonCreate label msg =
    Input.button
        [ Element.paddingXY 8 7
        , Background.color (color Color.lightGreen)
        , Font.color (color Color.darkCharcoal)
        , Border.color (color Color.black)
        , Border.width 1
        , Border.rounded 3
        , Element.mouseOver
            [ Background.color (color Color.green)
            , Font.color (color Color.black)
            ]
        ]
        { onPress = Just msg
        , label = Element.text label
        }


labeledInputText : Bool -> (String -> msg) -> String -> String -> Element msg
labeledInputText focusedOnLoad onChange label name =
    let
        defaultAttrs =
            [ Element.paddingXY 8 7
            , Element.width Element.fill
            ]
    in
    Input.text
        (if focusedOnLoad then
            Input.focusedOnLoad :: defaultAttrs

         else
            defaultAttrs
        )
        { onChange = onChange
        , text = name
        , placeholder = Nothing
        , label =
            Input.labelLeft
                [ Element.centerY
                , Element.paddingXY 5 0
                , Element.width (Element.px 100)
                ]
                (Element.text label)
        }


labeledDropdown printOption msg id label options dropdown selected =
    Element.row
        [ Element.paddingXY 5 7
        , Element.width Element.fill
        ]
        [ Element.el
            [ Element.width (Element.px 100)
            , Element.htmlAttribute <|
                Html.Attributes.id (id ++ "-label")
            ]
            (Element.text label)
        , Element.html <|
            Html.map msg <|
                Dropdown.view (dropdownViewConfig printOption)
                    { id = id
                    , labelledBy = id ++ "-label"
                    }
                    (List.map (Tuple.first >> Listbox.option) options)
                    dropdown
                    selected
        ]


labeledInputRadio :
    (That a -> msg)
    -> String
    -> Maybe (That a)
    -> List ( That a, Entry a )
    -> Element msg
labeledInputRadio msg label selected options =
    let
        option ( that, { name } ) =
            Input.option that <|
                Element.el
                    [ Element.padding 3 ]
                    (Element.text (Maybe.withDefault "<unnamed>" name))
    in
    Input.radio
        [ Element.paddingXY 8 7
        , Element.width Element.fill
        ]
        { onChange = msg
        , selected = selected
        , label =
            Input.labelLeft
                [ Element.centerY
                , Element.paddingXY 5 0
                , Element.width (Element.px 100)
                ]
                (Element.text label)
        , options = List.map option options
        }


horizontalLine : Element msg
horizontalLine =
    Element.el
        [ Element.height (Element.px 1)
        , Element.width Element.fill
        , Background.color (color Color.white)
        ]
        Element.none


color =
    Element.fromRgb << Color.toRgba



---- DROPDOWN CONFIG


dropdownUpdateConfig =
    Dropdown.updateConfig That.hash
        { jumpAtEnds = True
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , closeAfterMouseSelection = True
        , typeAhead = Listbox.noTypeAhead
        , minimalGap = 0
        , initialGap = 0
        }


dropdownViewConfig printOption =
    Dropdown.viewConfig That.hash
        { container =
            [ Html.Attributes.style "flex-grow" "10000"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "position" "relative"
            ]
        , button =
            \{ maybeSelection } ->
                { attributes =
                    [ Html.Attributes.style "margin" "0"
                    , Html.Attributes.style "padding-bottom" "7px"
                    , Html.Attributes.style "padding-left" "8px"
                    , Html.Attributes.style "padding-right" "8px"
                    , Html.Attributes.style "padding-top" "7px"
                    , Html.Attributes.style "flex-grow" "10000"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "border-color" "rgb(0, 0, 0)"
                    , Html.Attributes.style "border-left-radius" "3px"
                    , Html.Attributes.style "border-right-radius" "3px"
                    , Html.Attributes.style "border-style" "solid"
                    , Html.Attributes.style "border-width" "0.666667px"
                    , Html.Attributes.style "border-radius" "3px"
                    , Html.Attributes.style "box-sizing" "border-box"
                    , Html.Attributes.style "font-family" "\"Dosis\", sans-serif"
                    , Html.Attributes.style "font-size" "20px"
                    , Html.Attributes.style "font-style" "normal"
                    , Html.Attributes.style "font-weight" "400"
                    , Html.Attributes.style "line-height" "20px"
                    ]
                , children =
                    [ Html.text <|
                        case maybeSelection of
                            Nothing ->
                                "Select a point.."

                            Just thatPoint ->
                                printOption thatPoint
                    ]
                }
        , ul =
            [ Html.Attributes.style "z-index" "2000"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "max-height" "20rem"
            , Html.Attributes.style "overflow-y" "auto"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "background" "#fff"
            , Html.Attributes.style "border" "1px solid #ccc"
            , Html.Attributes.style "box-sizing" "border-box"
            , Html.Attributes.style "top" "100%"
            ]
        , liOption =
            \{ focused, hovered } thatPoint ->
                let
                    defaultAttrs =
                        [ Html.Attributes.style "display" "block"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "line-height" "1rem"
                        , Html.Attributes.style "padding" "10px"
                        ]
                in
                { attributes =
                    if focused then
                        [ Html.Attributes.style "background-color" "#f5fafd"
                        , Html.Attributes.style "color" "#495c68"
                        ]
                            ++ defaultAttrs

                    else if hovered then
                        [ Html.Attributes.style "background-color" "rgb(250, 250, 250)"
                        , Html.Attributes.style "color" "#495c68"
                        ]
                            ++ defaultAttrs

                    else
                        defaultAttrs
                , children =
                    [ Html.text (printOption thatPoint) ]
                }
        , liDivider = Listbox.noDivider
        }



---- SVG


drawPattern :
    Maybe (That Point)
    -> Those Point
    -> Those Line
    -> Those LineSegment
    -> Those Detail
    -> Pattern
    -> List (Svg Msg)
drawPattern hoveredPoint selectedPoints selectedLines selectedLineSegments selectedDetails pattern =
    let
        ( geometry, problems ) =
            Pattern.geometry pattern
    in
    List.concat
        [ [ Svg.defs []
                [ Svg.marker
                    [ Attributes.id "arrow"
                    , Attributes.viewBox "0 0 10 10"
                    , Attributes.refX "5"
                    , Attributes.refY "5"
                    , Attributes.markerWidth "6"
                    , Attributes.markerHeight "6"
                    , Attributes.orient "auto-start-reverse"
                    , Attributes.fill "blue"
                    ]
                    [ Svg.path
                        [ Attributes.d "M 0 0 L 10 5 L 0 10 z" ]
                        []
                    ]
                ]
          ]
        , List.map (drawDetail selectedDetails) geometry.details
        , List.map (drawLine selectedLines) geometry.lines
        , List.map (drawLineSegment selectedLineSegments) geometry.lineSegments
        , List.map (drawPoint pattern hoveredPoint selectedPoints) geometry.points
        ]


drawPoint : Pattern -> Maybe (That Point) -> Those Point -> ( That Point, Maybe String, Point2d ) -> Svg Msg
drawPoint pattern hoveredPoint selectedPoints ( thatPoint, maybeName, point2d ) =
    let
        ( x, y ) =
            Point2d.coordinates point2d

        hovered =
            hoveredPoint
                |> Maybe.map (That.areEqual thatPoint)
                |> Maybe.withDefault False

        selected =
            Those.member thatPoint selectedPoints

        helper =
            if hovered then
                Svg.g []
                    [ case
                        hoveredPoint
                            |> Maybe.andThen (Pattern.getPoint pattern)
                            |> Maybe.map .value
                      of
                        Just (Pattern.LeftOf thatAnchorPoint distance) ->
                            drawAnchorLine thatAnchorPoint distance <|
                                \float -> Vector2d.fromComponents ( -1 * float, 0 )

                        Just (Pattern.RightOf thatAnchorPoint distance) ->
                            drawAnchorLine thatAnchorPoint distance <|
                                \float -> Vector2d.fromComponents ( float, 0 )

                        Just (Pattern.Above thatAnchorPoint distance) ->
                            drawAnchorLine thatAnchorPoint distance <|
                                \float -> Vector2d.fromComponents ( 0, -1 * float )

                        Just (Pattern.Below thatAnchorPoint distance) ->
                            drawAnchorLine thatAnchorPoint distance <|
                                \float -> Vector2d.fromComponents ( 0, float )

                        _ ->
                            Svg.text ""
                    , case
                        hoveredPoint
                            |> Maybe.map (Pattern.getPointGeometries pattern)
                      of
                        Nothing ->
                            Svg.text ""

                        Just point2ds ->
                            drawPointChain point2ds
                    ]

            else
                Svg.text ""

        drawAnchorLine thatAnchorPoint distance toDirection =
            Maybe.map2 (drawDashedLine toDirection)
                (Pattern.getPointGeometry pattern thatAnchorPoint)
                (Pattern.computeLength pattern distance)
                |> Maybe.withDefault (Svg.text "")

        drawDashedLine toDirection p2d float =
            let
                otherPoint =
                    Point2d.translateBy (toDirection float) p2d
            in
            Svg.g
                []
                [ Svg.lineSegment2d
                    [ Attributes.stroke "blue"
                    , Attributes.strokeDasharray "4"
                    , Attributes.strokeWidth "1"
                    ]
                    (LineSegment2d.fromEndpoints
                        ( p2d, otherPoint )
                    )
                , Svg.circle2d
                    [ Attributes.fill "blue" ]
                    (Circle2d.withRadius 2 p2d)
                ]

        drawPointChain points =
            List.foldl drawLink ( Nothing, [] ) points
                |> Tuple.second
                |> Svg.g []

        drawLink point ( maybePreviousPoint, links ) =
            ( Just point
            , (case maybePreviousPoint of
                Nothing ->
                    Svg.g []
                        [ Svg.circle2d [ Attributes.fill "blue" ]
                            (Circle2d.withRadius 2 point)
                        ]

                Just previousPoint ->
                    let
                        startPoint =
                            previousPoint

                        midpoint =
                            LineSegment2d.from previousPoint point
                                |> LineSegment2d.midpoint

                        controlPoint =
                            case Direction2d.from previousPoint point of
                                Nothing ->
                                    midpoint

                                Just direction ->
                                    Point2d.along
                                        (Axis2d.through midpoint (Direction2d.perpendicularTo direction))
                                        (Point2d.squaredDistanceFrom startPoint endPoint / 1500)

                        endPoint =
                            point

                        spline =
                            QuadraticSpline2d.with
                                { startPoint = startPoint
                                , controlPoint = controlPoint
                                , endPoint = endPoint
                                }

                        id =
                            midpoint
                                |> Point2d.coordinates
                                |> (\( s, t ) ->
                                        String.join "-"
                                            [ String.fromFloat s
                                            , String.fromFloat t
                                            ]
                                   )
                    in
                    Svg.g []
                        [ Svg.mask
                            [ Attributes.id ("circleMask-" ++ id) ]
                            [ Svg.boundingBox2d
                                [ Attributes.fill "white" ]
                                (QuadraticSpline2d.boundingBox spline)
                            , Svg.circle2d
                                [ Attributes.fill "black" ]
                                (Circle2d.withRadius 15 startPoint)
                            , Svg.circle2d
                                [ Attributes.fill "black" ]
                                (Circle2d.withRadius 15 endPoint)
                            ]
                        , Svg.quadraticSpline2d
                            [ Attributes.stroke "blue"
                            , Attributes.strokeDasharray "4"
                            , Attributes.fill "none"
                            , Attributes.markerEnd "url(#arrow)"
                            , Attributes.mask ("url(#circleMask-" ++ id ++ ")")
                            ]
                            spline
                        , Svg.circle2d [ Attributes.fill "blue" ]
                            (Circle2d.withRadius 2 point)
                        ]
              )
                :: links
            )
    in
    Svg.g []
        [ Svg.circle2d
            [ Attributes.fill "black" ]
            (Circle2d.withRadius 2 point2d)
        , if selected then
            Svg.circle2d
                [ Attributes.stroke "blue"
                , Attributes.fill "none"
                ]
                (Circle2d.withRadius 5 point2d)

          else
            Svg.g [] []
        , helper
        , maybeName
            |> Maybe.map
                (\name ->
                    Svg.text_
                        [ Attributes.x (String.fromFloat x)
                        , Attributes.y (String.fromFloat y)
                        , Attributes.dy "-5"
                        , Attributes.style "font: 10px sans-serif;"
                        , Attributes.textAnchor "middle"
                        ]
                        [ Svg.text name ]
                )
            |> Maybe.withDefault (Svg.text "")
        , Svg.circle2d
            [ Attributes.fill "transparent"
            , Svg.Events.onMouseOver (PointHovered (Just thatPoint))
            , Svg.Events.onMouseOut (PointHovered Nothing)
            ]
            (Circle2d.withRadius 5 point2d)
        ]


drawLine : Those Line -> ( That Line, Maybe String, Axis2d ) -> Svg msg
drawLine selectedLines ( thatLine, maybeName, axis2d ) =
    let
        selected =
            Those.member thatLine selectedLines
    in
    Svg.lineSegment2d
        [ Attributes.stroke <|
            if selected then
                "blue"

            else
                "grey"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.along axis2d -1000
            , Point2d.along axis2d 1000
            )
        )


drawLineSegment : Those LineSegment -> ( That LineSegment, Maybe String, LineSegment2d ) -> Svg msg
drawLineSegment selectedLineSegments ( thatLineSegment, maybeName, lineSegment2d ) =
    let
        selected =
            Those.member thatLineSegment selectedLineSegments
    in
    Svg.lineSegment2d
        [ Attributes.stroke <|
            if selected then
                "blue"

            else
                "grey"
        ]
        lineSegment2d


drawDetail : Those Detail -> ( That Detail, Maybe String, Polygon2d ) -> Svg msg
drawDetail selectedDetails ( thatDetail, maybeName, polygon2d ) =
    let
        selected =
            Those.member thatDetail selectedDetails
    in
    Svg.polygon2d
        [ Attributes.fill "lightGrey"
        , Attributes.stroke <|
            if selected then
                "blue"

            else
                "black"
        , Attributes.strokeWidth "1"
        ]
        polygon2d



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
      -- LINE SEGMENTS
    | FromToClicked
      -- TRANSFORMATIONS
    | MirrorAtClicked
    | CutAlongLineSegmentClicked
      -- DETAILS
    | CounterClockwiseClicked
      --
    | NameChanged String
    | DistanceChanged String
    | LineChanged (That Line)
    | LineSegmentChanged (That LineSegment)
    | DetailChanged (That Detail)
    | PointChecked (That Point) Bool
    | PointAdded (That Point)
    | DropdownMsg (Dropdown.Msg (That Point))
    | DropdownAMsg (Dropdown.Msg (That Point))
    | DropdownBMsg (Dropdown.Msg (That Point))
      --
    | CreateClicked
    | CancelClicked
      -- PATTERN
    | PointHovered (Maybe (That Point))
      -- STORAGE
    | ClearPatternClicked
    | PatternReceived Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- POINTS
        LeftOfClicked ->
            ( { model
                | tool =
                    Just <|
                        LeftOf
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Cmd.none
            )

        RightOfClicked ->
            ( { model
                | tool =
                    Just <|
                        RightOf
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Cmd.none
            )

        AboveClicked ->
            ( { model
                | tool =
                    Just <|
                        Above
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Cmd.none
            )

        BelowClicked ->
            ( { model
                | tool =
                    Just <|
                        Below
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Cmd.none
            )

        AtAngleClicked ->
            ( { model | tool = Just AtAngle }
            , Cmd.none
            )

        -- LINES
        ThroughTwoPointsClicked ->
            ( { model
                | tool =
                    Just <|
                        ThroughTwoPoints
                            { dropdownA = Dropdown.init
                            , thatAnchorA = Nothing
                            , dropdownB = Dropdown.init
                            , thatAnchorB = Nothing
                            }
              }
            , Cmd.none
            )

        -- LINE SEGMENTS
        FromToClicked ->
            ( { model
                | tool =
                    Just <|
                        FromTo
                            { dropdownA = Dropdown.init
                            , thatAnchorA = Nothing
                            , dropdownB = Dropdown.init
                            , thatAnchorB = Nothing
                            }
              }
            , Cmd.none
            )

        -- TRANSFORMATIONS
        MirrorAtClicked ->
            ( { model | tool = Just (MirrorAt Nothing Those.none) }
            , Cmd.none
            )

        CutAlongLineSegmentClicked ->
            ( { model | tool = Just (CutAlongLineSegment Nothing Nothing) }
            , Cmd.none
            )

        -- DETAILS
        CounterClockwiseClicked ->
            ( { model | tool = Just (CounterClockwise []) }
            , Cmd.none
            )

        -- TOOL PARAMETERS
        NameChanged newName ->
            case model.tool of
                Just (LeftOf data) ->
                    ( { model | tool = Just (LeftOf { data | name = newName }) }
                    , Cmd.none
                    )

                Just (RightOf data) ->
                    ( { model | tool = Just (RightOf { data | name = newName }) }
                    , Cmd.none
                    )

                Just (Above data) ->
                    ( { model | tool = Just (Above { data | name = newName }) }
                    , Cmd.none
                    )

                Just (Below data) ->
                    ( { model | tool = Just (Below { data | name = newName }) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DistanceChanged newDistance ->
            case model.tool of
                Just (LeftOf data) ->
                    ( { model | tool = Just (LeftOf { data | distance = newDistance }) }
                    , Cmd.none
                    )

                Just (RightOf data) ->
                    ( { model | tool = Just (RightOf { data | distance = newDistance }) }
                    , Cmd.none
                    )

                Just (Above data) ->
                    ( { model | tool = Just (Above { data | distance = newDistance }) }
                    , Cmd.none
                    )

                Just (Below data) ->
                    ( { model | tool = Just (Below { data | distance = newDistance }) }
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

        LineSegmentChanged newLineSegment ->
            case model.tool of
                Just (CutAlongLineSegment _ detail) ->
                    ( { model | tool = Just (CutAlongLineSegment (Just newLineSegment) detail) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DetailChanged newDetail ->
            case model.tool of
                Just (CutAlongLineSegment lineSegment _) ->
                    ( { model | tool = Just (CutAlongLineSegment lineSegment (Just newDetail)) }
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
                                        Those.insert thatPoint targets

                                    else
                                        Those.remove thatPoint targets
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

        DropdownMsg dropdownMsg ->
            case model.tool of
                Just (LeftOf data) ->
                    let
                        ( newDropdown, dropdownCmd, newAnchor ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdown
                                data.thatAnchor
                    in
                    ( { model
                        | tool =
                            Just <|
                                LeftOf
                                    { data
                                        | dropdown = newDropdown
                                        , thatAnchor = newAnchor
                                    }
                      }
                    , Cmd.map DropdownMsg dropdownCmd
                    )

                Just (RightOf data) ->
                    let
                        ( newDropdown, dropdownCmd, newAnchor ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdown
                                data.thatAnchor
                    in
                    ( { model
                        | tool =
                            Just <|
                                RightOf
                                    { data
                                        | dropdown = newDropdown
                                        , thatAnchor = newAnchor
                                    }
                      }
                    , Cmd.map DropdownMsg dropdownCmd
                    )

                Just (Above data) ->
                    let
                        ( newDropdown, dropdownCmd, newAnchor ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdown
                                data.thatAnchor
                    in
                    ( { model
                        | tool =
                            Just <|
                                Above
                                    { data
                                        | dropdown = newDropdown
                                        , thatAnchor = newAnchor
                                    }
                      }
                    , Cmd.map DropdownMsg dropdownCmd
                    )

                Just (Below data) ->
                    let
                        ( newDropdown, dropdownCmd, newAnchor ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdown
                                data.thatAnchor
                    in
                    ( { model
                        | tool =
                            Just <|
                                Below
                                    { data
                                        | dropdown = newDropdown
                                        , thatAnchor = newAnchor
                                    }
                      }
                    , Cmd.map DropdownMsg dropdownCmd
                    )

                _ ->
                    ( model, Cmd.none )

        DropdownAMsg dropdownMsg ->
            case model.tool of
                Just (ThroughTwoPoints data) ->
                    let
                        ( newDropdownA, dropdownCmd, newAnchorA ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownA
                                data.thatAnchorA
                    in
                    ( { model
                        | tool =
                            Just <|
                                ThroughTwoPoints
                                    { data
                                        | dropdownA = newDropdownA
                                        , thatAnchorA = newAnchorA
                                    }
                      }
                    , Cmd.map DropdownAMsg dropdownCmd
                    )

                Just (FromTo data) ->
                    let
                        ( newDropdownA, dropdownCmd, newAnchorA ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownA
                                data.thatAnchorA
                    in
                    ( { model
                        | tool =
                            Just <|
                                FromTo
                                    { data
                                        | dropdownA = newDropdownA
                                        , thatAnchorA = newAnchorA
                                    }
                      }
                    , Cmd.map DropdownAMsg dropdownCmd
                    )

                _ ->
                    ( model, Cmd.none )

        DropdownBMsg dropdownMsg ->
            case model.tool of
                Just (ThroughTwoPoints data) ->
                    let
                        ( newDropdownB, dropdownCmd, newAnchorB ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownB
                                data.thatAnchorB
                    in
                    ( { model
                        | tool =
                            Just <|
                                ThroughTwoPoints
                                    { data
                                        | dropdownB = newDropdownB
                                        , thatAnchorB = newAnchorB
                                    }
                      }
                    , Cmd.map DropdownAMsg dropdownCmd
                    )

                Just (FromTo data) ->
                    let
                        ( newDropdownB, dropdownCmd, newAnchorB ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownB
                                data.thatAnchorB
                    in
                    ( { model
                        | tool =
                            Just <|
                                FromTo
                                    { data
                                        | dropdownB = newDropdownB
                                        , thatAnchorB = newAnchorB
                                    }
                      }
                    , Cmd.map DropdownBMsg dropdownCmd
                    )

                _ ->
                    ( model, Cmd.none )

        --
        CancelClicked ->
            ( { model | tool = Nothing }
            , Cmd.none
            )

        CreateClicked ->
            let
                insertSimpleDistance constructor name anchor distance =
                    case ( anchor, String.toFloat distance ) of
                        ( Just thatPoint, Just by ) ->
                            let
                                newPoint =
                                    constructor thatPoint
                                        (Pattern.Length (Pattern.exprFromFloat by))

                                newPattern =
                                    Pattern.insertPoint
                                        (if name == "" then
                                            Nothing

                                         else
                                            Just name
                                        )
                                        newPoint
                                        model.pattern
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
                Just (LeftOf { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.LeftOf name thatAnchor distance

                Just (RightOf { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.RightOf name thatAnchor distance

                Just (Above { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.Above name thatAnchor distance

                Just (Below { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.Below name thatAnchor distance

                Just AtAngle ->
                    ( model, Cmd.none )

                Just (ThroughTwoPoints { thatAnchorA, thatAnchorB }) ->
                    case ( thatAnchorA, thatAnchorB ) of
                        ( Just thatPointA, Just thatPointB ) ->
                            let
                                newLine =
                                    Pattern.ThroughTwoPoints
                                        thatPointA
                                        thatPointB

                                newPattern =
                                    Pattern.insertLine newLine model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , tool = Nothing
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Just (FromTo { thatAnchorA, thatAnchorB }) ->
                    case ( thatAnchorA, thatAnchorB ) of
                        ( Just thatPointA, Just thatPointB ) ->
                            let
                                newLineSegment =
                                    Pattern.FromTo
                                        thatPointA
                                        thatPointB

                                newPattern =
                                    Pattern.insertLineSegment newLineSegment model.pattern
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

                Just (CutAlongLineSegment lineSegment detail) ->
                    case ( lineSegment, detail ) of
                        ( Just thatLineSegment, Just thatDetail ) ->
                            let
                                newTransformation =
                                    Pattern.CutAlongLineSegment thatLineSegment thatDetail

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

                Nothing ->
                    ( model, Cmd.none )

        -- PATTERN
        PointHovered newHoveredPoint ->
            ( { model | hoveredPoint = newHoveredPoint }
            , Cmd.none
            )

        -- STORAGE
        ClearPatternClicked ->
            let
                newPattern =
                    Pattern.empty
                        |> Pattern.insertPoint (Just "origin") Pattern.Origin
            in
            ( { model
                | pattern = newPattern
                , tool = Nothing
              }
            , safePattern (Pattern.encode newPattern)
            )

        PatternReceived value ->
            case Decode.decodeValue Pattern.decoder value of
                Err error ->
                    let
                        newPattern =
                            Pattern.empty
                                |> Pattern.insertPoint (Just "origin") Pattern.Origin
                    in
                    ( { model | pattern = newPattern }
                    , safePattern (Pattern.encode newPattern)
                    )

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
