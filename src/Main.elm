port module Main exposing (main)

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

import Accessibility.Widget as Widget
import Axis2d exposing (Axis2d)
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Circle2d
import Color
import Css
import Direction2d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg as Svg
import Html.Attributes
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Json.Encode exposing (Value)
import LineSegment2d exposing (LineSegment2d)
import Pattern exposing (Detail, Line, LineSegment, Pattern, Point)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import QuadraticSpline2d
import Store exposing (Entry)
import Styled.Listbox as Listbox exposing (Listbox)
import Styled.Listbox.Dropdown as Dropdown exposing (Dropdown)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task
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
    , hoveredPoint : Maybe (That Point)
    , dialog : Dialog

    -- RIGHT TOOLBAR
    , variablesVisible : Bool
    , pointsVisible : Bool
    }


type Dialog
    = NoDialog
    | Tool Tool
    | CreateVariable
        { name : String
        , value : String
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
        { name : String
        , dropdownA : Dropdown
        , thatAnchorA : Maybe (That Point)
        , dropdownB : Dropdown
        , thatAnchorB : Maybe (That Point)
        }
      -- LINE SEGMENTS
    | FromTo
        { name : String
        , dropdownA : Dropdown
        , thatAnchorA : Maybe (That Point)
        , dropdownB : Dropdown
        , thatAnchorB : Maybe (That Point)
        }
      -- TRANSFORMATIONS
    | MirrorAt
        { dropdown : Dropdown
        , thatLine : Maybe (That Line)
        , listbox : Listbox
        , thosePoints : Those Point
        }
    | CutAlongLineSegment
        { dropdownLineSegment : Dropdown
        , thatLineSegment : Maybe (That LineSegment)
        , dropdownDetail : Dropdown
        , thatDetail : Maybe (That Detail)
        }
      -- DETAILS
    | CounterClockwise (List (That Point))


isLeftOf maybeTool =
    case maybeTool of
        Just (LeftOf _) ->
            True

        _ ->
            False


toolDescription : Tool -> Element msg
toolDescription tool =
    let
        simpleDistanceHorizontal kind =
            Element.paragraph []
                [ Element.text "Create a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "point")
                , Element.text " to the "
                , Element.el
                    [ Font.bold ]
                    (Element.text kind)
                , Element.text " another point."
                ]

        simpleDistanceVertical kind =
            Element.paragraph []
                [ Element.text "Create a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "point")
                , Element.text " "
                , Element.el
                    [ Font.bold ]
                    (Element.text kind)
                , Element.text " another point."
                ]
    in
    case tool of
        LeftOf _ ->
            simpleDistanceHorizontal "left of"

        RightOf _ ->
            simpleDistanceHorizontal "right of"

        Above _ ->
            simpleDistanceVertical "above"

        Below _ ->
            simpleDistanceVertical "below"

        AtAngle ->
            Element.paragraph []
                [ Element.text "Create a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "point")
                , Element.text " relative to another point by providing an "
                , Element.el
                    [ Font.bold ]
                    (Element.text "angle")
                , Element.text " and a "
                , Element.el
                    [ Font.bold ]
                    (Element.text "distance")
                , Element.text "."
                ]

        ThroughTwoPoints _ ->
            Element.paragraph []
                [ Element.text "Create a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "line")
                , Element.text " through "
                , Element.el
                    [ Font.bold ]
                    (Element.text "two points")
                , Element.text "."
                ]

        FromTo _ ->
            Element.paragraph []
                [ Element.text "Connect "
                , Element.el
                    [ Font.bold ]
                    (Element.text "two points")
                , Element.text " with a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "line segment")
                , Element.text "."
                ]

        MirrorAt _ ->
            Element.paragraph []
                [ Element.text "Mirror a "
                , Element.el
                    [ Font.bold ]
                    (Element.text "set of points")
                , Element.text " along a "
                , Element.el
                    [ Font.bold ]
                    (Element.text "line")
                , Element.text "."
                ]

        CutAlongLineSegment _ ->
            Element.paragraph []
                [ Element.text "Cut a "
                , Element.el
                    [ Font.bold ]
                    (Element.text "detail")
                , Element.text " into two along a "
                , Element.el
                    [ Font.bold ]
                    (Element.text "line segment")
                , Element.text "."
                ]

        CounterClockwise _ ->
            Element.paragraph []
                [ Element.text "Create a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "detail")
                , Element.text " by connecting "
                , Element.el
                    [ Font.bold ]
                    (Element.text "points")
                , Element.text " counterclockwise."
                ]


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
        Just (MirrorAt _) ->
            True

        _ ->
            False


isCutAlongLineSegment maybeTool =
    case maybeTool of
        Just (CutAlongLineSegment _) ->
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

        MirrorAt { thosePoints } ->
            thosePoints

        CutAlongLineSegment _ ->
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

        MirrorAt { thatLine } ->
            thatLine
                |> maybeToList
                |> Those.fromList

        CutAlongLineSegment _ ->
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

        MirrorAt _ ->
            empty

        CutAlongLineSegment { thatLineSegment } ->
            thatLineSegment
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

        MirrorAt _ ->
            empty

        CutAlongLineSegment { thatDetail } ->
            thatDetail
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
      , hoveredPoint = Nothing
      , dialog = NoDialog
      , variablesVisible = True
      , pointsVisible = False
      }
    , requestPattern ()
    )



---- VIEW


view : Model -> Document Msg
view model =
    { title = "Sewing Pattern Editor"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Just (color (Color.rgb255 229 223 197))
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width Element.fill
            , Element.height Element.fill
            , Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]
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
        [ viewLeftToolbar model
        , viewWorkspace model
        , viewRightToolbar model
        ]


viewLeftToolbar model =
    Element.column
        [ Element.height Element.fill
        , Background.color gray900
        ]
        [ viewToolSelector <|
            case model.dialog of
                Tool tool ->
                    Just tool

                _ ->
                    Nothing
        , Element.el [ Element.height Element.fill ] Element.none
        , Element.row
            [ Element.padding 5
            , Element.spacing 5
            , Element.width Element.fill
            ]
            [ buttonDanger "Clear pattern" ClearPatternClicked
            ]
        ]


viewWorkspace model =
    let
        maybeTool =
            case model.dialog of
                Tool tool ->
                    Just tool

                _ ->
                    Nothing

        selectedPoints =
            maybeTool
                |> Maybe.map selectedPointsFromTool
                |> Maybe.withDefault (Those.fromList [])

        selectedLines =
            maybeTool
                |> Maybe.map selectedLinesFromTool
                |> Maybe.withDefault (Those.fromList [])

        selectedLineSegments =
            maybeTool
                |> Maybe.map selectedLineSegmentsFromTool
                |> Maybe.withDefault (Those.fromList [])

        selectedDetails =
            maybeTool
                |> Maybe.map selectedDetailsFromTool
                |> Maybe.withDefault (Those.fromList [])
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront (viewDialog model.pattern model.dialog)
        ]
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-320 -320 640 640" ]
                (drawPattern
                    model.hoveredPoint
                    selectedPoints
                    selectedLines
                    selectedLineSegments
                    selectedDetails
                    model.pattern
                )
        )


viewRightToolbar model =
    Element.column
        [ Element.width (Element.px 400)
        , Element.height Element.fill
        , Background.color gray900
        ]
        [ viewVariables model
        , viewPoints model
        ]



-- DIALOG


viewDialog pattern dialog =
    case dialog of
        NoDialog ->
            Element.none

        Tool tool ->
            Element.el
                [ Element.alignRight
                , Element.moveLeft 40
                , Element.width (Element.px 300)
                , Background.color gray900
                ]
                (viewTool pattern
                    (Pattern.points pattern)
                    (Pattern.lines pattern)
                    (Pattern.lineSegments pattern)
                    (Pattern.details pattern)
                    tool
                )

        CreateVariable { name, value } ->
            Element.el
                [ Element.alignRight
                , Element.moveLeft 40
                , Element.width (Element.px 500)
                , Background.color gray900
                ]
                (Element.column
                    [ Element.width Element.fill
                    , Element.padding 15
                    , Element.spacing 15
                    ]
                    [ Element.paragraph
                        [ Font.size 12
                        , Font.color white
                        ]
                        [ Element.text "Create a new "
                        , Element.el
                            [ Font.bold ]
                            (Element.text "variable")
                        ]
                    , Element.column
                        [ Element.width Element.fill
                        , Element.spacing 10
                        ]
                        [ labeledInputText VariableNameChanged "name" name
                        , labeledInputText VariableValueChanged "value" value
                        ]
                    , Element.row
                        [ Element.alignRight
                        , Element.spacing 5
                        ]
                        [ buttonCreate "Create" CreateClicked
                        , buttonDismiss "Cancel" CancelClicked
                        ]
                    ]
                )



-- TOOL


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
        simpleDistanceTool toolId name dropdown anchor distance =
            Element.column
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ labeledInputText NameChanged "name" name
                , labeledDropdown
                    (Pattern.getPoint pattern
                        >> Maybe.andThen .name
                        >> Maybe.withDefault "<no name>"
                    )
                    "Select a point.."
                    DropdownMsg
                    (toolId ++ "-anchor")
                    "anchor"
                    points
                    dropdown
                    anchor
                , labeledInputText DistanceChanged "distance" distance
                ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 15
        , Element.spacing 30
        ]
        [ Element.el
            [ Font.size 12
            , Font.color white
            ]
            (toolDescription tool)
        , case tool of
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

            ThroughTwoPoints { name, dropdownA, thatAnchorA, dropdownB, thatAnchorB } ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ labeledInputText NameChanged "name" name
                    , labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a point.."
                        DropdownAMsg
                        "through-two-points-anchor-a"
                        "1st anchor"
                        points
                        dropdownA
                        thatAnchorA
                    , labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a point.."
                        DropdownBMsg
                        "through-two-points-anchor-b"
                        "2st anchor"
                        points
                        dropdownB
                        thatAnchorB
                    ]

            FromTo { name, dropdownA, thatAnchorA, dropdownB, thatAnchorB } ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ labeledInputText NameChanged "name" name
                    , labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a point.."
                        DropdownAMsg
                        "from-to-anchor-a"
                        "1st anchor"
                        points
                        dropdownA
                        thatAnchorA
                    , labeledDropdown
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a point.."
                        DropdownBMsg
                        "from-to-anchor-b"
                        "2st anchor"
                        points
                        dropdownB
                        thatAnchorB
                    ]

            MirrorAt { dropdown, thatLine, listbox, thosePoints } ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ labeledDropdown
                        (Pattern.getLine pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a line.."
                        DropdownLineMsg
                        "mirror-at-line"
                        "line"
                        lines
                        dropdown
                        thatLine
                    , labeledListbox
                        (Pattern.getPoint pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        ListboxPointsMsg
                        "mirror-at-points"
                        "targets"
                        points
                        listbox
                        thosePoints
                    ]

            CutAlongLineSegment { dropdownLineSegment, thatLineSegment, dropdownDetail, thatDetail } ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ labeledDropdown
                        (Pattern.getLineSegment pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a line segment.."
                        DropdownLineSegmentMsg
                        "cut-along-line-segment--line-segment"
                        "line segment"
                        lineSegments
                        dropdownLineSegment
                        thatLineSegment
                    , labeledDropdown
                        (Pattern.getDetail pattern
                            >> Maybe.andThen .name
                            >> Maybe.withDefault "<no name>"
                        )
                        "Select a detail.."
                        DropdownDetailMsg
                        "cut-along-line-segment--detail"
                        "detail"
                        details
                        dropdownDetail
                        thatDetail
                    ]

            CounterClockwise targets ->
                let
                    pointButton ( thatPoint, { name } ) =
                        button "" (Maybe.withDefault "<unnamed>" name) (PointAdded thatPoint) False
                in
                Element.column
                    [ Element.width Element.fill
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
        [ Element.padding 10
        , Element.spacing 30
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text "points")
            , Element.column
                [ Element.spacing 5 ]
                [ Element.row
                    [ Element.spacing 5
                    , Element.width Element.fill
                    ]
                    [ button "left_of" "Left of" LeftOfClicked (isLeftOf maybeTool)
                    , button "right_of" "Right of" RightOfClicked (isRightOf maybeTool)
                    ]
                , Element.row
                    [ Element.spacing 5
                    , Element.width Element.fill
                    ]
                    [ button "above" "Above" AboveClicked (isAbove maybeTool)
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
            [ Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text "lines")
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
            [ Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text "line segments")
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
            [ Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text "transformations")
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
            [ Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text "details")
            , Element.column
                [ Element.spacing 5
                , Element.width Element.fill
                ]
                [ button "counter_clockwise" "Counter clockwise" CounterClockwiseClicked (isCounterClockwise maybeTool)
                ]
            ]
        ]



-- VARIABLES


viewVariables model =
    let
        viewFloatValue value =
            Element.el
                [ Font.size 14
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.el [ Element.alignRight ]
                    (Element.text (String.fromFloat value))
                )
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 10
        ]
        [ accordionToggle VariablesRulerClicked "variables" model.variablesVisible
        , if model.variablesVisible then
            Element.column
                [ Element.width Element.fill
                , Element.spacing 15
                , Element.padding 5
                ]
                [ Element.table
                    [ Element.spacing 7 ]
                    { data = List.sortBy .name (Pattern.variables model.pattern)
                    , columns =
                        [ { header =
                                Element.el
                                    [ Font.size 12
                                    , Font.variant Font.smallCaps
                                    , Font.color (color (Color.rgb255 229 223 197))
                                    ]
                                    (Element.text "name")
                          , width = Element.fill
                          , view =
                                \{ name } ->
                                    Element.el
                                        [ Font.size 14
                                        , Font.color (color (Color.rgb255 229 223 197))
                                        ]
                                        (Element.text name)
                          }
                        , { header =
                                Element.el
                                    [ Font.size 12
                                    , Font.variant Font.smallCaps
                                    , Font.color (color (Color.rgb255 229 223 197))
                                    ]
                                    (Element.text "computed")
                          , width = Element.shrink
                          , view =
                                \{ computed } ->
                                    viewFloatValue computed
                          }
                        , { header = Element.none
                          , width = Element.shrink
                          , view =
                                \_ ->
                                    Element.row
                                        [ Element.paddingEach
                                            { left = 5
                                            , right = 0
                                            , top = 0
                                            , bottom = 0
                                            }
                                        , Element.spacing 10
                                        ]
                                        [ Input.button
                                            [ Font.color white
                                            , Border.width 1
                                            , Border.color gray900
                                            , Element.mouseOver
                                                [ Font.color gray700 ]
                                            ]
                                            { onPress = Nothing
                                            , label =
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    ]
                                                    (icon "edit")
                                            }
                                        , Input.button
                                            [ Font.color white
                                            , Border.width 1
                                            , Border.color gray900
                                            , Element.mouseOver
                                                [ Font.color gray700 ]
                                            ]
                                            { onPress = Nothing
                                            , label =
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    ]
                                                    (icon "trash")
                                            }
                                        ]
                          }
                        ]
                    }
                , Element.row
                    [ Element.width Element.fill ]
                    [ Input.button
                        [ Element.alignRight
                        , Element.paddingXY 8 7
                        , Font.size 14
                        , Background.color gray800
                        , Border.color gray800
                        , Border.width 1
                        , Font.color white
                        , Element.mouseOver
                            [ Background.color gray700 ]
                        ]
                        { onPress = Just VariableCreateClicked
                        , label = Element.text "Create variable"
                        }
                    ]
                ]

          else
            Element.none
        ]


viewPoints model =
    let
        viewHeader name =
            Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text name)

        viewValue value =
            Element.el
                [ Font.size 14
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text value)

        viewFloatHeader name =
            Element.el
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.el [ Element.alignRight ]
                    (Element.text name)
                )

        viewFloatValue value =
            Element.el
                [ Font.size 14
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.el [ Element.alignRight ]
                    (Element.text (String.fromFloat value))
                )
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 10
        ]
        [ accordionToggle PointsRulerClicked "points" model.pointsVisible
        , if model.pointsVisible then
            Element.column
                [ Element.width Element.fill
                , Element.spacing 15
                , Element.padding 5
                ]
                [ Element.table
                    [ Element.spacing 7 ]
                    { data =
                        List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
                            (Pattern.points model.pattern)
                    , columns =
                        [ { header = viewHeader "name"
                          , width = Element.fill
                          , view =
                                \( _, { name } ) ->
                                    viewValue (Maybe.withDefault "<no name>" name)
                          }
                        , { header = viewFloatHeader "x"
                          , width = Element.px 35
                          , view =
                                \( thatPoint, _ ) ->
                                    thatPoint
                                        |> Pattern.getPointGeometry model.pattern
                                        |> Maybe.map
                                            (Point2d.xCoordinate >> viewFloatValue)
                                        |> Maybe.withDefault Element.none
                          }
                        , { header = viewFloatHeader "y"
                          , width = Element.px 35
                          , view =
                                \( thatPoint, _ ) ->
                                    thatPoint
                                        |> Pattern.getPointGeometry model.pattern
                                        |> Maybe.map
                                            (Point2d.yCoordinate >> viewFloatValue)
                                        |> Maybe.withDefault Element.none
                          }
                        , { header = Element.none
                          , width = Element.shrink
                          , view =
                                \_ ->
                                    Element.row
                                        [ Element.paddingEach
                                            { left = 5
                                            , right = 0
                                            , top = 0
                                            , bottom = 0
                                            }
                                        , Element.spacing 10
                                        ]
                                        [ Input.button
                                            [ Font.color white
                                            , Border.width 1
                                            , Border.color gray900
                                            , Element.mouseOver
                                                [ Font.color gray700 ]
                                            ]
                                            { onPress = Nothing
                                            , label =
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    ]
                                                    (icon "edit")
                                            }
                                        , Input.button
                                            [ Font.color white
                                            , Border.width 1
                                            , Border.color gray900
                                            , Element.mouseOver
                                                [ Font.color gray700 ]
                                            ]
                                            { onPress = Nothing
                                            , label =
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    ]
                                                    (icon "trash")
                                            }
                                        ]
                          }
                        ]
                    }
                ]

          else
            Element.none
        ]



---- REUSABLE ELEMENTS


icon name =
    Element.html <|
        Html.toUnstyled <|
            Html.i
                [ Attributes.class "fas"
                , Attributes.class ("fa-" ++ name)
                , Attributes.css
                    [ Css.fontSize (Css.px 12)
                    , Css.color Css.inherit
                    ]
                ]
                []


accordionToggle msg name visible =
    Input.button
        [ Element.width Element.fill
        , Element.padding 5
        , Border.color gray900
        , Border.width 1
        , Element.mouseOver
            [ Background.color gray800 ]
        ]
        { onPress = Just msg
        , label =
            Element.row
                [ Element.width Element.fill
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Font.size 16
                    , Font.variant Font.smallCaps
                    , Font.color (color (Color.rgb255 229 223 197))
                    ]
                    (Element.text name)
                , Element.html <|
                    Html.toUnstyled <|
                        Html.i
                            [ Attributes.class "fas"
                            , Attributes.class <|
                                if visible then
                                    "fa-chevron-up"

                                else
                                    "fa-chevron-down"
                            , Attributes.css
                                [ Css.fontSize (Css.px 12)
                                , Css.paddingRight (Css.px 5)
                                , Css.color (Css.rgb 229 223 197)
                                ]
                            ]
                            []
                ]
        }


button : String -> String -> msg -> Bool -> Element msg
button iconSrc label msg selected =
    Input.button
        [ Element.padding 5
        , Background.color <|
            if selected then
                gray700

            else
                gray800
        , Border.color gray900
        , Border.width 1
        , Element.mouseOver
            [ Background.color <|
                if selected then
                    gray700

                else
                    gray700
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
        , Font.size 14
        , Background.color gray800
        , Border.color gray800
        , Border.width 1
        , Font.color white
        , Element.mouseOver
            [ Background.color gray700 ]
        ]
        { onPress = Just msg
        , label = Element.text label
        }


buttonDanger : String -> msg -> Element msg
buttonDanger label msg =
    Input.button
        [ Element.paddingXY 8 7
        , Element.alignRight
        , Font.size 14
        , Background.color gray800
        , Border.color gray800
        , Border.width 1
        , Font.color white
        , Element.mouseOver
            [ Background.color gray700 ]
        ]
        { onPress = Just msg
        , label = Element.text label
        }


buttonCreate : String -> msg -> Element msg
buttonCreate label msg =
    Input.button
        [ Element.paddingXY 8 7
        , Background.color gray800
        , Border.color gray800
        , Border.width 1
        , Font.color white
        , Font.size 14
        , Element.mouseOver
            [ Background.color gray700 ]
        ]
        { onPress = Just msg
        , label = Element.text label
        }


labeledInputText : (String -> msg) -> String -> String -> Element msg
labeledInputText onChange label name =
    Input.text
        [ Element.width Element.fill
        , Element.padding 5
        , Font.size 16
        , Font.color white
        , Background.color gray700
        , Border.width 1
        , Border.color gray700
        , Element.htmlAttribute <|
            Html.Attributes.id (label ++ "-input")
        ]
        { onChange = onChange
        , text = name
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ Font.size 12
                , Font.variant Font.smallCaps
                , Font.color (color (Color.rgb255 229 223 197))
                ]
                (Element.text label)
        }


labeledDropdown :
    (That object -> String)
    -> String
    -> (Dropdown.Msg (That object) -> Msg)
    -> String
    -> String
    -> List ( That object, Entry object )
    -> Dropdown
    -> Maybe (That object)
    -> Element Msg
labeledDropdown printOption placeholder msg id label options dropdown selected =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 3
        ]
        [ Element.el
            [ Font.size 12
            , Font.color (color (Color.rgb255 229 223 197))
            , Font.variant Font.smallCaps
            , Element.htmlAttribute <|
                Html.Attributes.id (id ++ "-label")
            ]
            (Element.text label)
        , Element.el [ Element.width Element.fill ] <|
            Element.html <|
                Html.toUnstyled <|
                    Html.map msg <|
                        Dropdown.view (dropdownViewConfig printOption placeholder)
                            { id = id
                            , labelledBy = id ++ "-label"
                            }
                            (List.map (Tuple.first >> Listbox.option) options)
                            dropdown
                            selected
        ]


labeledListbox :
    (That object -> String)
    -> (Listbox.Msg (That object) -> Msg)
    -> String
    -> String
    -> List ( That object, Entry object )
    -> Listbox
    -> Those object
    -> Element Msg
labeledListbox printOption msg id label options listbox selection =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 3
        ]
        [ Element.el
            [ Font.size 12
            , Font.color (color (Color.rgb255 229 223 197))
            , Font.variant Font.smallCaps
            , Element.htmlAttribute <|
                Html.Attributes.id (id ++ "-label")
            ]
            (Element.text label)
        , Element.el [ Element.width Element.fill ] <|
            Element.html <|
                Html.toUnstyled <|
                    Listbox.view (listboxViewConfig printOption)
                        { id = id
                        , labelledBy = id ++ "-label"
                        , lift = msg
                        }
                        (List.map (Tuple.first >> Listbox.option) options)
                        listbox
                        (Those.toList selection)
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
                , Element.width (Element.px 120)
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


white =
    color (Color.rgb255 229 223 197)


green900 =
    color (Color.rgb255 27 94 32)


gray700 =
    color (Color.rgb255 97 97 97)


gray750 =
    color (Color.rgb255 82 82 82)


gray800 =
    color (Color.rgb255 66 66 66)


gray900 =
    color (Color.rgb255 33 33 33)



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


dropdownViewConfig printOption placeholder =
    Dropdown.viewConfig That.hash
        { container =
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexGrow (Css.num 10000)
                , Css.position Css.relative
                ]
            ]
        , button =
            \{ maybeSelection } ->
                { attributes =
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.flexGrow (Css.num 10000)
                        , Css.margin Css.zero
                        , Css.paddingLeft (Css.px 5)
                        , Css.paddingRight (Css.px 5)
                        , Css.paddingBottom (Css.px 5)
                        , Css.paddingTop (Css.px 5)
                        , Css.borderColor (Css.rgb 97 97 97)
                        , Css.borderStyle Css.solid
                        , Css.borderWidth (Css.px 1)
                        , Css.borderRadius (Css.px 3)
                        , Css.boxSizing Css.borderBox
                        , Css.fontFamilies [ "Roboto", "sans-serif" ]
                        , Css.fontSize (Css.px 16)
                        , Css.fontStyle Css.normal
                        , Css.fontWeight (Css.int 400)
                        , Css.lineHeight (Css.px 20)
                        , Css.backgroundColor (Css.rgb 97 97 97)
                        , Css.color (Css.rgb 229 223 197)
                        , Css.focus
                            [ Css.borderColor (Css.rgb 229 223 197) ]
                        ]
                    ]
                , children =
                    [ Html.text <|
                        case maybeSelection of
                            Nothing ->
                                placeholder

                            Just thatPoint ->
                                printOption thatPoint
                    ]
                }
        , ul =
            [ Attributes.css
                [ Css.zIndex (Css.int 2000)
                , Css.top (Css.pct 100)
                , Css.width (Css.pct 100)
                , Css.maxHeight (Css.rem 20)
                , Css.overflowY Css.auto
                , Css.padding Css.zero
                , Css.margin Css.zero
                , Css.color (Css.rgb 229 223 197)
                , Css.backgroundColor (Css.rgb 66 66 66)
                , Css.borderWidth (Css.px 1)
                , Css.borderStyle Css.solid
                , Css.borderColor (Css.rgb 66 66 66)
                , Css.borderRadius (Css.px 3)
                , Css.boxSizing Css.borderBox
                , Css.focus
                    [ Css.borderColor (Css.rgb 229 223 197) ]
                ]
            ]
        , liOption =
            \{ focused, hovered } thatPoint ->
                let
                    defaultAttrs =
                        [ Attributes.css
                            [ Css.display Css.block
                            , Css.cursor Css.pointer
                            , Css.lineHeight (Css.rem 1)
                            , Css.padding (Css.px 10)
                            , Css.fontSize (Css.px 16)
                            ]
                        ]
                in
                { attributes =
                    if focused then
                        [ Attributes.css
                            [ Css.backgroundColor (Css.rgb 97 97 97) ]
                        ]
                            ++ defaultAttrs

                    else if hovered then
                        [ Attributes.css
                            [ Css.backgroundColor (Css.rgb 82 82 82) ]
                        ]
                            ++ defaultAttrs

                    else
                        defaultAttrs
                , children =
                    [ Html.text (printOption thatPoint) ]
                }
        , liDivider = Listbox.noDivider
        }



---- LISTBOX CONFIG


listboxUpdateConfig =
    Listbox.updateConfig That.hash
        { jumpAtEnds = True
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = Listbox.noTypeAhead
        , minimalGap = 0
        , initialGap = 0
        }


listboxViewConfig printOption =
    Listbox.viewConfig That.hash
        { ul =
            [ Attributes.css
                [ Css.top (Css.pct 100)
                , Css.width (Css.pct 100)
                , Css.maxHeight (Css.rem 10)
                , Css.overflowY Css.auto
                , Css.padding Css.zero
                , Css.margin Css.zero
                , Css.backgroundColor (Css.rgb 66 66 66)
                , Css.boxSizing Css.borderBox
                , Css.borderWidth (Css.px 1)
                , Css.borderStyle Css.solid
                , Css.borderColor (Css.rgb 66 66 66)
                , Css.borderRadius (Css.px 3)
                , Css.color (Css.rgb 229 223 197)
                , Css.focus
                    [ Css.borderColor (Css.rgb 229 223 197) ]
                ]
            ]
        , liOption =
            \{ selected, focused, hovered } thatPoint ->
                let
                    defaultAttrs =
                        [ Attributes.css
                            [ Css.display Css.block
                            , Css.cursor Css.pointer
                            , Css.lineHeight (Css.rem 1)
                            , Css.padding (Css.px 10)
                            , Css.fontSize (Css.px 16)
                            ]
                        ]
                in
                { attributes =
                    if focused then
                        [ Attributes.css
                            [ Css.backgroundColor (Css.rgb 97 97 97) ]
                        ]
                            ++ defaultAttrs

                    else if hovered then
                        [ Attributes.css
                            [ Css.backgroundColor (Css.rgb 82 82 82) ]
                        ]
                            ++ defaultAttrs

                    else
                        defaultAttrs
                , children =
                    [ Html.i
                        [ Attributes.class "fas"
                        , Attributes.class "fa-check"
                        , Attributes.css
                            [ Css.fontSize (Css.px 12)
                            , Css.paddingRight (Css.px 5)
                            , if selected then
                                Css.color Css.inherit

                              else
                                Css.color Css.transparent
                            ]
                        ]
                        []
                    , Html.text (printOption thatPoint)
                    ]
                }
        , liDivider = Listbox.noDivider
        , empty = Html.text ""
        , focusable = True
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
                    [ Svg.Attributes.id "arrow"
                    , Svg.Attributes.viewBox "0 0 10 10"
                    , Svg.Attributes.refX "5"
                    , Svg.Attributes.refY "5"
                    , Svg.Attributes.markerWidth "6"
                    , Svg.Attributes.markerHeight "6"
                    , Svg.Attributes.orient "auto-start-reverse"
                    , Svg.Attributes.fill "blue"
                    ]
                    [ Svg.path
                        [ Svg.Attributes.d "M 0 0 L 10 5 L 0 10 z" ]
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
                    [ Svg.Attributes.stroke "blue"
                    , Svg.Attributes.strokeDasharray "4"
                    , Svg.Attributes.strokeWidth "1"
                    ]
                    (LineSegment2d.fromEndpoints
                        ( p2d, otherPoint )
                    )
                , Svg.circle2d
                    [ Svg.Attributes.fill "blue" ]
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
                        [ Svg.circle2d [ Svg.Attributes.fill "blue" ]
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
                            [ Svg.Attributes.id ("circleMask-" ++ id) ]
                            [ Svg.boundingBox2d
                                [ Svg.Attributes.fill "white" ]
                                (QuadraticSpline2d.boundingBox spline)
                            , Svg.circle2d
                                [ Svg.Attributes.fill "black" ]
                                (Circle2d.withRadius 15 startPoint)
                            , Svg.circle2d
                                [ Svg.Attributes.fill "black" ]
                                (Circle2d.withRadius 15 endPoint)
                            ]
                        , Svg.quadraticSpline2d
                            [ Svg.Attributes.stroke "blue"
                            , Svg.Attributes.strokeDasharray "4"
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.markerEnd "url(#arrow)"
                            , Svg.Attributes.mask ("url(#circleMask-" ++ id ++ ")")
                            ]
                            spline
                        , Svg.circle2d [ Svg.Attributes.fill "blue" ]
                            (Circle2d.withRadius 2 point)
                        ]
              )
                :: links
            )
    in
    Svg.g []
        [ Svg.circle2d
            [ Svg.Attributes.fill "black" ]
            (Circle2d.withRadius 2 point2d)
        , if selected then
            Svg.circle2d
                [ Svg.Attributes.stroke "blue"
                , Svg.Attributes.fill "none"
                ]
                (Circle2d.withRadius 5 point2d)

          else
            Svg.g [] []
        , helper
        , maybeName
            |> Maybe.map
                (\name ->
                    Svg.text_
                        [ Svg.Attributes.x (String.fromFloat x)
                        , Svg.Attributes.y (String.fromFloat y)
                        , Svg.Attributes.dy "-5"
                        , Svg.Attributes.style "font: 10px sans-serif;"
                        , Svg.Attributes.textAnchor "middle"
                        ]
                        [ Svg.text name ]
                )
            |> Maybe.withDefault (Svg.text "")
        , Svg.circle2d
            [ Svg.Attributes.fill "transparent"
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
        [ Svg.Attributes.stroke <|
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
        [ Svg.Attributes.stroke <|
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
        [ Svg.Attributes.fill "lightGrey"
        , Svg.Attributes.stroke <|
            if selected then
                "blue"

            else
                "black"
        , Svg.Attributes.strokeWidth "1"
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
    | PointAdded (That Point)
    | DropdownMsg (Dropdown.Msg (That Point))
    | DropdownAMsg (Dropdown.Msg (That Point))
    | DropdownBMsg (Dropdown.Msg (That Point))
    | DropdownLineMsg (Dropdown.Msg (That Line))
    | ListboxPointsMsg (Listbox.Msg (That Point))
    | DropdownLineSegmentMsg (Dropdown.Msg (That LineSegment))
    | DropdownDetailMsg (Dropdown.Msg (That Detail))
      --
    | CreateClicked
    | CancelClicked
      -- PATTERN
    | PointHovered (Maybe (That Point))
      -- STORAGE
    | ClearPatternClicked
    | PatternReceived Value
      -- RIGHT TOOLBAR
    | VariablesRulerClicked
    | VariableCreateClicked
    | VariableNameChanged String
    | VariableValueChanged String
    | PointsRulerClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- POINTS
        LeftOfClicked ->
            ( { model
                | dialog =
                    Tool <|
                        LeftOf
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        RightOfClicked ->
            ( { model
                | dialog =
                    Tool <|
                        RightOf
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        AboveClicked ->
            ( { model
                | dialog =
                    Tool <|
                        Above
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        BelowClicked ->
            ( { model
                | dialog =
                    Tool <|
                        Below
                            { name = ""
                            , dropdown = Dropdown.init
                            , thatAnchor = Nothing
                            , distance = ""
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        AtAngleClicked ->
            ( { model | dialog = Tool AtAngle }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        -- LINES
        ThroughTwoPointsClicked ->
            ( { model
                | dialog =
                    Tool <|
                        ThroughTwoPoints
                            { name = ""
                            , dropdownA = Dropdown.init
                            , thatAnchorA = Nothing
                            , dropdownB = Dropdown.init
                            , thatAnchorB = Nothing
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        -- LINE SEGMENTS
        FromToClicked ->
            ( { model
                | dialog =
                    Tool <|
                        FromTo
                            { name = ""
                            , dropdownA = Dropdown.init
                            , thatAnchorA = Nothing
                            , dropdownB = Dropdown.init
                            , thatAnchorB = Nothing
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        -- TRANSFORMATIONS
        MirrorAtClicked ->
            ( { model
                | dialog =
                    Tool <|
                        MirrorAt
                            { dropdown = Dropdown.init
                            , thatLine = Nothing
                            , listbox = Listbox.init
                            , thosePoints = Those.none
                            }
              }
            , Dropdown.focus "mirror-at-line"
                |> Task.attempt (\_ -> NoOp)
            )

        CutAlongLineSegmentClicked ->
            ( { model
                | dialog =
                    Tool <|
                        CutAlongLineSegment
                            { dropdownLineSegment = Dropdown.init
                            , thatLineSegment = Nothing
                            , dropdownDetail = Dropdown.init
                            , thatDetail = Nothing
                            }
              }
            , Dropdown.focus "cut-along-line-segment--line-segment"
                |> Task.attempt (\_ -> NoOp)
            )

        -- DETAILS
        CounterClockwiseClicked ->
            ( { model | dialog = Tool (CounterClockwise []) }
            , Cmd.none
            )

        -- TOOL PARAMETERS
        NameChanged newName ->
            let
                updateName toTool data =
                    ( { model | dialog = Tool (toTool { data | name = newName }) }
                    , Cmd.none
                    )
            in
            case model.dialog of
                Tool (LeftOf data) ->
                    updateName LeftOf data

                Tool (RightOf data) ->
                    updateName RightOf data

                Tool (Above data) ->
                    updateName Above data

                Tool (Below data) ->
                    updateName Below data

                Tool (ThroughTwoPoints data) ->
                    updateName ThroughTwoPoints data

                Tool (FromTo data) ->
                    updateName FromTo data

                _ ->
                    ( model, Cmd.none )

        DistanceChanged newDistance ->
            let
                updateDistance toTool data =
                    ( { model | dialog = Tool (toTool { data | distance = newDistance }) }
                    , Cmd.none
                    )
            in
            case model.dialog of
                Tool (LeftOf data) ->
                    updateDistance LeftOf data

                Tool (RightOf data) ->
                    updateDistance RightOf data

                Tool (Above data) ->
                    updateDistance Above data

                Tool (Below data) ->
                    updateDistance Below data

                _ ->
                    ( model, Cmd.none )

        PointAdded thatPoint ->
            case model.dialog of
                Tool (CounterClockwise targets) ->
                    ( { model | dialog = Tool (CounterClockwise (thatPoint :: targets)) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DropdownMsg dropdownMsg ->
            let
                updateDropdown toTool data =
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
                        | dialog =
                            Tool <|
                                toTool
                                    { data
                                        | dropdown = newDropdown
                                        , thatAnchor = newAnchor
                                    }
                      }
                    , Cmd.map DropdownMsg dropdownCmd
                    )
            in
            case model.dialog of
                Tool (LeftOf data) ->
                    updateDropdown LeftOf data

                Tool (RightOf data) ->
                    updateDropdown RightOf data

                Tool (Above data) ->
                    updateDropdown Above data

                Tool (Below data) ->
                    updateDropdown Below data

                _ ->
                    ( model, Cmd.none )

        DropdownAMsg dropdownMsg ->
            let
                updateDropdown toTool data =
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
                        | dialog =
                            Tool <|
                                toTool
                                    { data
                                        | dropdownA = newDropdownA
                                        , thatAnchorA = newAnchorA
                                    }
                      }
                    , Cmd.map DropdownAMsg dropdownCmd
                    )
            in
            case model.dialog of
                Tool (ThroughTwoPoints data) ->
                    updateDropdown ThroughTwoPoints data

                Tool (FromTo data) ->
                    updateDropdown FromTo data

                _ ->
                    ( model, Cmd.none )

        DropdownBMsg dropdownMsg ->
            let
                updateDropdown toTool data =
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
                        | dialog =
                            Tool <|
                                toTool
                                    { data
                                        | dropdownB = newDropdownB
                                        , thatAnchorB = newAnchorB
                                    }
                      }
                    , Cmd.map DropdownBMsg dropdownCmd
                    )
            in
            case model.dialog of
                Tool (ThroughTwoPoints data) ->
                    updateDropdown ThroughTwoPoints data

                Tool (FromTo data) ->
                    updateDropdown FromTo data

                _ ->
                    ( model, Cmd.none )

        DropdownLineMsg dropdownMsg ->
            case model.dialog of
                Tool (MirrorAt data) ->
                    let
                        ( newDropdown, dropdownCmd, newLine ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.lines model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdown
                                data.thatLine
                    in
                    ( { model
                        | dialog =
                            Tool <|
                                MirrorAt
                                    { data
                                        | dropdown = newDropdown
                                        , thatLine = newLine
                                    }
                      }
                    , Cmd.map DropdownLineMsg dropdownCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ListboxPointsMsg listboxMsg ->
            case model.dialog of
                Tool (MirrorAt data) ->
                    let
                        ( newListbox, listboxCmd, newPoints ) =
                            Listbox.update listboxUpdateConfig
                                (Pattern.points model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                listboxMsg
                                data.listbox
                                (Those.toList data.thosePoints)
                    in
                    ( { model
                        | dialog =
                            Tool <|
                                MirrorAt
                                    { data
                                        | listbox = newListbox
                                        , thosePoints = Those.fromList newPoints
                                    }
                      }
                    , Cmd.map ListboxPointsMsg listboxCmd
                    )

                _ ->
                    ( model, Cmd.none )

        DropdownLineSegmentMsg dropdownMsg ->
            case model.dialog of
                Tool (CutAlongLineSegment data) ->
                    let
                        ( newDropdown, dropdownCmd, newLineSegment ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.lineSegments model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownLineSegment
                                data.thatLineSegment
                    in
                    ( { model
                        | dialog =
                            Tool <|
                                CutAlongLineSegment
                                    { data
                                        | dropdownLineSegment = newDropdown
                                        , thatLineSegment = newLineSegment
                                    }
                      }
                    , Cmd.map DropdownLineSegmentMsg dropdownCmd
                    )

                _ ->
                    ( model, Cmd.none )

        DropdownDetailMsg dropdownMsg ->
            case model.dialog of
                Tool (CutAlongLineSegment data) ->
                    let
                        ( newDropdown, dropdownCmd, newDetail ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.details model.pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownDetail
                                data.thatDetail
                    in
                    ( { model
                        | dialog =
                            Tool <|
                                CutAlongLineSegment
                                    { data
                                        | dropdownDetail = newDropdown
                                        , thatDetail = newDetail
                                    }
                      }
                    , Cmd.map DropdownDetailMsg dropdownCmd
                    )

                _ ->
                    ( model, Cmd.none )

        --
        CancelClicked ->
            ( { model | dialog = NoDialog }
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
                                , dialog = NoDialog
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                lastState =
                    Pattern.lastState model.pattern
            in
            case model.dialog of
                Tool (LeftOf { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.LeftOf name thatAnchor distance

                Tool (RightOf { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.RightOf name thatAnchor distance

                Tool (Above { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.Above name thatAnchor distance

                Tool (Below { name, thatAnchor, distance }) ->
                    insertSimpleDistance Pattern.Below name thatAnchor distance

                Tool AtAngle ->
                    ( model, Cmd.none )

                Tool (ThroughTwoPoints { name, thatAnchorA, thatAnchorB }) ->
                    case ( thatAnchorA, thatAnchorB ) of
                        ( Just thatPointA, Just thatPointB ) ->
                            let
                                newLine =
                                    Pattern.ThroughTwoPoints
                                        thatPointA
                                        thatPointB

                                newPattern =
                                    Pattern.insertLine
                                        (if name == "" then
                                            Nothing

                                         else
                                            Just name
                                        )
                                        newLine
                                        model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , dialog = NoDialog
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Tool (FromTo { name, thatAnchorA, thatAnchorB }) ->
                    case ( thatAnchorA, thatAnchorB ) of
                        ( Just thatPointA, Just thatPointB ) ->
                            let
                                newLineSegment =
                                    Pattern.FromTo
                                        thatPointA
                                        thatPointB

                                newPattern =
                                    Pattern.insertLineSegment
                                        (if name == "" then
                                            Nothing

                                         else
                                            Just name
                                        )
                                        newLineSegment
                                        model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , dialog = NoDialog
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Tool (MirrorAt { thatLine, thosePoints }) ->
                    case thatLine of
                        Just line ->
                            let
                                newTransformation =
                                    Pattern.MirrorAt line thosePoints

                                newPattern =
                                    Pattern.insertTransformation newTransformation model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , dialog = NoDialog
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Tool (CutAlongLineSegment { thatLineSegment, thatDetail }) ->
                    case ( thatLineSegment, thatDetail ) of
                        ( Just lineSegment, Just detail ) ->
                            let
                                newTransformation =
                                    Pattern.CutAlongLineSegment lineSegment detail

                                newPattern =
                                    Pattern.insertTransformation newTransformation model.pattern
                            in
                            ( { model
                                | pattern = newPattern
                                , dialog = NoDialog
                              }
                            , safePattern (Pattern.encode newPattern)
                            )

                        _ ->
                            ( model, Cmd.none )

                Tool (CounterClockwise targets) ->
                    let
                        newDetail =
                            targets
                                |> Pattern.CounterClockwise

                        newPattern =
                            Pattern.insertDetail newDetail model.pattern
                    in
                    ( { model
                        | pattern = newPattern
                        , dialog = NoDialog
                      }
                    , safePattern (Pattern.encode newPattern)
                    )

                CreateVariable { name, value } ->
                    let
                        newPattern =
                            Pattern.insertVariable name value model.pattern
                    in
                    ( { model
                        | pattern = newPattern
                        , dialog = NoDialog
                      }
                    , safePattern (Pattern.encode newPattern)
                    )

                NoDialog ->
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
                , dialog = NoDialog
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

        VariablesRulerClicked ->
            ( { model | variablesVisible = not model.variablesVisible }
            , Cmd.none
            )

        VariableCreateClicked ->
            ( { model | dialog = CreateVariable { name = "", value = "" } }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        VariableNameChanged newName ->
            case model.dialog of
                CreateVariable data ->
                    ( { model | dialog = CreateVariable { data | name = newName } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        VariableValueChanged newValue ->
            case model.dialog of
                CreateVariable data ->
                    ( { model | dialog = CreateVariable { data | value = newValue } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PointsRulerClicked ->
            ( { model | pointsVisible = not model.pointsVisible }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ patternReceived PatternReceived
        , case model.dialog of
            NoDialog ->
                Sub.none

            _ ->
                Browser.Events.onKeyDown
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\key ->
                                case key of
                                    "Escape" ->
                                        Decode.succeed CancelClicked

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                    )
        ]


onUrlRequest urlRequest =
    NoOp


onUrlChange url =
    NoOp
