module Page.Editor exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

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
import Array
import BoundingBox2d
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Circle2d
import Color
import Design exposing (Grey(..))
import Draw.Pattern as Pattern
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import LineSegment2d
import List.Extra as List
import Listbox exposing (Listbox)
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Pattern exposing (Circle, Detail, Line, LineSegment, Pattern, Point)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Process
import Store exposing (Entry)
import StoredPattern exposing (StoredPattern)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Task
import That exposing (That)
import Those exposing (Those)
import Url exposing (Url)
import Vector2d
import View.Design
import View.Icon
import View.Input
import View.Navigation
import View.Table
import VoronoiDiagram2d



---- MODEL


type alias Model =
    { maybeDrag : Maybe Drag
    , patternContainerDimensions : Maybe Dimensions
    , maybeModal : Maybe Modal

    -- PATTERN
    , hoveredPoint : Maybe (That Point)

    -- LEFT TOOLBAR
    , maybeTool : Maybe Tool
    , preventActionMenuClose : Bool

    -- RIGHT TOOLBAR
    , rightToolbarVisible : Bool
    , variablesVisible : Bool
    , maybeVariableDialog : Maybe VariableDialog
    , pointsVisible : Bool
    , circlesVisible : Bool
    , linesVisible : Bool
    , lineSegmentsVisible : Bool
    , detailsVisible : Bool
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Position =
    { x : Float
    , y : Float
    }


type Modal
    = DetailDeleteConfirm (That Detail)


type VariableDialog
    = VariableDialogCreate
        { name : String
        , value : String
        }


type ToolTag
    = LeftOfTag
    | RightOfTag
    | AboveTag
    | BelowTag
    | AtAngleTag
    | BetweenRatioTag
    | BetweenLengthTag
    | CircleCircleTag
    | LineLineTag
    | CircleLineTag
    | CenteredAtTag
    | ThroughTwoPointsTag
    | ThroughOnePointTag
    | FromToTag
    | MirrorAtTag
    | DetailTag


toolTagToId : ToolTag -> String
toolTagToId toolTag =
    case toolTag of
        LeftOfTag ->
            "left-of"

        RightOfTag ->
            "right-of"

        AboveTag ->
            "above"

        BelowTag ->
            "below"

        AtAngleTag ->
            "at-angle"

        BetweenRatioTag ->
            "between-ratio"

        BetweenLengthTag ->
            "between-length"

        CircleCircleTag ->
            "circle-circle"

        LineLineTag ->
            "line-line"

        CircleLineTag ->
            "circle-line"

        CenteredAtTag ->
            "centered-at"

        ThroughTwoPointsTag ->
            "through-two-points"

        ThroughOnePointTag ->
            "through-one-point"

        FromToTag ->
            "from-to"

        MirrorAtTag ->
            "mirror-at"

        DetailTag ->
            "detail"


toolToTag : Tool -> ToolTag
toolToTag tool =
    let
        pointDataToTag pointData =
            case pointData of
                LeftOf _ ->
                    LeftOfTag

                RightOf _ ->
                    RightOfTag

                Above _ ->
                    AboveTag

                Below _ ->
                    BelowTag

                AtAngle _ ->
                    AtAngleTag

                BetweenRatio _ ->
                    BetweenRatioTag

                BetweenLength _ ->
                    BetweenLengthTag

                CircleCircle _ ->
                    CircleCircleTag

                LineLine _ ->
                    LineLineTag

                CircleLine _ ->
                    CircleLineTag

        circleDataToTag circleData =
            case circleData of
                CenteredAt _ ->
                    CenteredAtTag

        lineDataToTag lineData =
            case lineData of
                ThroughTwoPoints _ ->
                    ThroughTwoPointsTag

                ThroughOnePoint _ ->
                    ThroughOnePointTag

        detailDataToTag detailData =
            case detailData of
                DetailOnePoint _ ->
                    DetailTag

                DetailManyPoints _ ->
                    DetailTag
    in
    case tool of
        CreatePoint _ pointData ->
            pointDataToTag pointData

        EditPoint _ pointData ->
            pointDataToTag pointData

        CreateCircle _ circleData ->
            circleDataToTag circleData

        EditCircle _ circleData ->
            circleDataToTag circleData

        CreateLine _ lineData ->
            lineDataToTag lineData

        EditLine _ lineData ->
            lineDataToTag lineData

        FromTo _ ->
            FromToTag

        MirrorAt _ ->
            MirrorAtTag

        CreateDetail _ detailData ->
            detailDataToTag detailData

        EditDetail _ detailData ->
            detailDataToTag detailData


type PointData
    = LeftOf
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , distance : String
        }
    | RightOf
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , distance : String
        }
    | Above
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , distance : String
        }
    | Below
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , distance : String
        }
    | AtAngle
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , angle : String
        , distance : String
        }
    | BetweenRatio
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , dropdownAnchorB : Dropdown
        , maybeThatAnchorB : Maybe (That Point)
        , ratio : String
        }
    | BetweenLength
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , dropdownAnchorB : Dropdown
        , maybeThatAnchorB : Maybe (That Point)
        , length : String
        }
    | CircleCircle
        { dropdownCircleA : Dropdown
        , maybeThatCircleA : Maybe (That Circle)
        , dropdownCircleB : Dropdown
        , maybeThatCircleB : Maybe (That Circle)
        , first : Bool
        }
    | LineLine
        { dropdownLineA : Dropdown
        , maybeThatLineA : Maybe (That Line)
        , dropdownLineB : Dropdown
        , maybeThatLineB : Maybe (That Line)
        }
    | CircleLine
        { dropdownCircleA : Dropdown
        , maybeThatCircleA : Maybe (That Circle)
        , dropdownLineA : Dropdown
        , maybeThatLineA : Maybe (That Line)
        , first : Bool
        }


type CircleData
    = CenteredAt
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , radius : String
        }


type LineData
    = ThroughTwoPoints
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , dropdownAnchorB : Dropdown
        , maybeThatAnchorB : Maybe (That Point)
        }
    | ThroughOnePoint
        { dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , angle : String
        }


type DetailData
    = DetailOnePoint
        { firstPointDropdown : Dropdown
        , firstPointMaybeThat : Maybe (That Point)
        }
    | DetailManyPoints
        { firstPointDropdown : Dropdown
        , firstPointMaybeThat : Maybe (That Point)
        , firstPointActionMenu : ActionMenu
        , secondPointDropdown : Dropdown
        , secondPointMaybeThat : Maybe (That Point)
        , secondPointActionMenu : ActionMenu
        , connectionFirstSecond : Connection
        , otherPoints :
            List
                { dropdown : Dropdown
                , maybeThat : Maybe (That Point)
                , actionMenu : ActionMenu
                , connectionPrevious : Connection
                }
        , connectionLastFirst : Connection
        }


type Tool
    = CreatePoint (Validatable String) PointData
    | EditPoint (That Point) PointData
      -- CIRCLES
    | CreateCircle (Validatable String) CircleData
    | EditCircle (That Circle) CircleData
      -- LINES
    | CreateLine (Validatable String) LineData
    | EditLine (That Line) LineData
      -- LINE SEGMENTS
    | FromTo
        { name : String
        , dropdownAnchorA : Dropdown
        , maybeThatAnchorA : Maybe (That Point)
        , dropdownAnchorB : Dropdown
        , maybeThatAnchorB : Maybe (That Point)
        }
      -- TRANSFORMATIONS
    | MirrorAt
        { dropdownLineA : Dropdown
        , maybeThatLineA : Maybe (That Line)
        , listbox : Listbox
        , thosePoints : Those Point
        }
      -- DETAILS
    | CreateDetail (Validatable String) DetailData
    | EditDetail (That Detail) DetailData


type Validatable value
    = Valid value
    | Invalid value String


updateValue : value -> Validatable value -> Validatable value
updateValue newValue validatable =
    case validatable of
        Valid _ ->
            Valid newValue

        Invalid _ help ->
            Invalid newValue help


valueOf : Validatable value -> value
valueOf validatable =
    case validatable of
        Valid v ->
            v

        Invalid v _ ->
            v


type ActionMenu
    = Closed
    | MoveUp
    | MoveDown
    | InsertPointBefore
    | InsertPointAfter
    | Remove


type ConnectionTag
    = ConnectionStraightTag
    | ConnectionQuadraticTag


type Connection
    = ConnectionStraight
    | ConnectionQuadratic
        { dropdown : Dropdown
        , maybeThat : Maybe (That Point)
        }


toolDescription : ToolTag -> Element msg
toolDescription toolTag =
    let
        simpleDistanceHorizontal kind =
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " to the "
                , strong kind
                , s " another point."
                ]

        simpleDistanceVertical kind =
            Element.paragraph []
                [ s "Create a new "
                , Element.el
                    [ Font.bold ]
                    (Element.text "point")
                , s " "
                , Element.el
                    [ Font.bold ]
                    (Element.text kind)
                , s " another point."
                ]

        s =
            Element.text

        strong =
            Element.el [ Font.bold ] << Element.text
    in
    case toolTag of
        LeftOfTag ->
            simpleDistanceHorizontal "left of"

        RightOfTag ->
            simpleDistanceHorizontal "right of"

        AboveTag ->
            simpleDistanceVertical "above"

        BelowTag ->
            simpleDistanceVertical "below"

        AtAngleTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " relative to another point by providing an "
                , strong "angle"
                , s " and a "
                , strong "distance"
                , s "."
                ]

        BetweenRatioTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " between two other points at a certain "
                , strong "ratio"
                , s "."
                ]

        BetweenLengthTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " between two other points at a certain "
                , strong "length"
                , s "."
                ]

        CircleCircleTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " at the intersection of two "
                , strong "circles"
                , s "."
                ]

        LineLineTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " at the intersection of two "
                , strong "lines"
                , s "."
                ]

        CircleLineTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "point"
                , s " at the intersection of a "
                , strong "circle"
                , s " and a "
                , strong "line"
                , s "."
                ]

        CenteredAtTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "circle"
                , s " by providing its "
                , strong "center point"
                , s " and the "
                , strong "radius"
                , s "."
                ]

        ThroughTwoPointsTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "line"
                , s " through "
                , strong "two points"
                , s "."
                ]

        ThroughOnePointTag ->
            Element.paragraph []
                [ s "Create a new "
                , strong "line"
                , s " through "
                , strong "one point"
                , s " with a given "
                , strong "angle"
                , s "."
                ]

        FromToTag ->
            Element.paragraph []
                [ s "Connect "
                , strong "two points"
                , s " with a new "
                , strong "line segment"
                , s "."
                ]

        MirrorAtTag ->
            Element.paragraph []
                [ s "Mirror a "
                , strong "set of points"
                , s " along a "
                , strong "line"
                , s "."
                ]

        DetailTag ->
            Element.paragraph []
                [ s "Create a "
                , strong "detail"
                , s "."
                ]


toolDescriptionEdit : String -> ToolTag -> Element msg
toolDescriptionEdit name toolTag =
    let
        simpleDistanceHorizontal kind =
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " to the "
                , strong kind
                , s " another point."
                ]

        simpleDistanceVertical kind =
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , s "point"
                , s " "
                , strong kind
                , s " another point."
                ]

        s =
            Element.text

        strong =
            Element.el [ Font.bold ] << Element.text
    in
    case toolTag of
        LeftOfTag ->
            simpleDistanceHorizontal "left of"

        RightOfTag ->
            simpleDistanceHorizontal "right of"

        AboveTag ->
            simpleDistanceVertical "above"

        BelowTag ->
            simpleDistanceVertical "below"

        AtAngleTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " relative to another point by providing an "
                , strong "angle"
                , s " and a "
                , strong "distance"
                , s "."
                ]

        BetweenRatioTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " between two other points at a certain "
                , strong "ratio"
                , s "."
                ]

        BetweenLengthTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " between two other points at a certain "
                , strong "length"
                , s "."
                ]

        CircleCircleTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " at the intersection of two "
                , strong "circles"
                , s "."
                ]

        LineLineTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " at the intersection of two "
                , strong "lines"
                , s "."
                ]

        CircleLineTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "point"
                , s " at the intersection of a "
                , strong "circle"
                , s " and a "
                , strong "line"
                , s "."
                ]

        CenteredAtTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "circle"
                , s " constructed by providing its "
                , strong "center point"
                , s " and the "
                , strong "radius"
                , s "."
                ]

        ThroughTwoPointsTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "line"
                , s " through "
                , strong "two points"
                , s "."
                ]

        ThroughOnePointTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "line"
                , s " through "
                , strong "one point"
                , s " with a given "
                , strong "angle"
                , s "."
                ]

        FromToTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "line segment"
                , s " connecting "
                , strong "two points"
                , s "."
                ]

        MirrorAtTag ->
            Element.paragraph []
                [ s "Mirror a "
                , strong "set of points"
                , s " along a "
                , strong "line"
                , s "."
                ]

        DetailTag ->
            Element.paragraph []
                [ s "Edit «"
                , strong name
                , s "» which is a "
                , strong "detail"
                , s "."
                ]


selectedPointsFromTool : Tool -> Those Point
selectedPointsFromTool tool =
    let
        empty =
            Those.fromList []

        onlyAnchorA data =
            data.maybeThatAnchorA
                |> maybeToList
                |> Those.fromList

        anchorAandB data =
            [ data.maybeThatAnchorA, data.maybeThatAnchorB ]
                |> List.filterMap identity
                |> Those.fromList

        selectedPointsFromPointData pointData =
            case pointData of
                LeftOf data ->
                    onlyAnchorA data

                RightOf data ->
                    onlyAnchorA data

                Above data ->
                    onlyAnchorA data

                Below data ->
                    onlyAnchorA data

                AtAngle data ->
                    onlyAnchorA data

                BetweenRatio data ->
                    anchorAandB data

                BetweenLength data ->
                    anchorAandB data

                CircleCircle _ ->
                    empty

                LineLine _ ->
                    empty

                CircleLine _ ->
                    empty

        selectedPointsFromCircleData circleData =
            case circleData of
                CenteredAt data ->
                    onlyAnchorA data

        selectedPointsFromLineData lineData =
            case lineData of
                ThroughTwoPoints data ->
                    anchorAandB data

                ThroughOnePoint data ->
                    onlyAnchorA data

        selectedPointsFromDetailData detailData =
            case detailData of
                DetailOnePoint data ->
                    data.firstPointMaybeThat
                        |> maybeToList
                        |> Those.fromList

                DetailManyPoints data ->
                    Those.fromList <|
                        List.filterMap identity <|
                            List.concat
                                [ [ data.firstPointMaybeThat
                                  , data.secondPointMaybeThat
                                  ]
                                , List.concat <|
                                    List.map
                                        (\{ maybeThat, connectionPrevious } ->
                                            maybeThat
                                                :: (case connectionPrevious of
                                                        ConnectionStraight ->
                                                            []

                                                        ConnectionQuadratic quadraticData ->
                                                            [ quadraticData.maybeThat ]
                                                   )
                                        )
                                        data.otherPoints
                                ]
    in
    case tool of
        CreatePoint _ pointData ->
            selectedPointsFromPointData pointData

        EditPoint _ pointData ->
            selectedPointsFromPointData pointData

        CreateCircle _ circleData ->
            selectedPointsFromCircleData circleData

        EditCircle _ circleData ->
            selectedPointsFromCircleData circleData

        CreateLine _ lineData ->
            selectedPointsFromLineData lineData

        EditLine _ lineData ->
            selectedPointsFromLineData lineData

        FromTo data ->
            anchorAandB data

        MirrorAt { thosePoints } ->
            thosePoints

        CreateDetail _ lineData ->
            selectedPointsFromDetailData lineData

        EditDetail _ lineData ->
            selectedPointsFromDetailData lineData


selectedLinesFromTool : Tool -> Those Line
selectedLinesFromTool tool =
    let
        empty =
            Those.fromList []

        selectedLinesFromPointData pointData =
            case pointData of
                LeftOf _ ->
                    empty

                RightOf _ ->
                    empty

                Above _ ->
                    empty

                Below _ ->
                    empty

                AtAngle _ ->
                    empty

                BetweenRatio _ ->
                    empty

                BetweenLength _ ->
                    empty

                CircleCircle _ ->
                    empty

                LineLine { maybeThatLineA, maybeThatLineB } ->
                    [ maybeThatLineA, maybeThatLineB ]
                        |> List.filterMap identity
                        |> Those.fromList

                CircleLine { maybeThatLineA } ->
                    maybeThatLineA
                        |> maybeToList
                        |> Those.fromList
    in
    case tool of
        CreatePoint _ pointData ->
            selectedLinesFromPointData pointData

        EditPoint _ pointData ->
            selectedLinesFromPointData pointData

        CreateCircle _ _ ->
            empty

        EditCircle _ _ ->
            empty

        CreateLine _ _ ->
            empty

        EditLine _ _ ->
            empty

        FromTo _ ->
            empty

        MirrorAt { maybeThatLineA } ->
            maybeThatLineA
                |> maybeToList
                |> Those.fromList

        CreateDetail _ _ ->
            empty

        EditDetail _ _ ->
            empty


selectedLineSegmentsFromTool : Tool -> Those LineSegment
selectedLineSegmentsFromTool tool =
    let
        empty =
            Those.fromList []

        selectedLineSegmentsFromPointData pointData =
            case pointData of
                LeftOf _ ->
                    empty

                RightOf _ ->
                    empty

                Above _ ->
                    empty

                Below _ ->
                    empty

                AtAngle _ ->
                    empty

                BetweenRatio _ ->
                    empty

                BetweenLength _ ->
                    empty

                CircleCircle _ ->
                    empty

                LineLine _ ->
                    empty

                CircleLine _ ->
                    empty
    in
    case tool of
        CreatePoint _ pointData ->
            selectedLineSegmentsFromPointData pointData

        EditPoint _ pointData ->
            selectedLineSegmentsFromPointData pointData

        CreateCircle _ _ ->
            empty

        EditCircle _ _ ->
            empty

        CreateLine _ _ ->
            empty

        EditLine _ _ ->
            empty

        FromTo _ ->
            empty

        MirrorAt _ ->
            empty

        CreateDetail _ _ ->
            empty

        EditDetail _ _ ->
            empty


selectedDetailsFromTool : Tool -> Those Detail
selectedDetailsFromTool tool =
    let
        empty =
            Those.fromList []

        selectedDetailsFromPointData pointData =
            case pointData of
                LeftOf _ ->
                    empty

                RightOf _ ->
                    empty

                Above _ ->
                    empty

                Below _ ->
                    empty

                AtAngle _ ->
                    empty

                BetweenRatio _ ->
                    empty

                BetweenLength _ ->
                    empty

                CircleCircle _ ->
                    empty

                LineLine _ ->
                    empty

                CircleLine _ ->
                    empty
    in
    case tool of
        CreatePoint _ pointData ->
            selectedDetailsFromPointData pointData

        EditPoint _ pointData ->
            selectedDetailsFromPointData pointData

        EditCircle _ _ ->
            empty

        CreateCircle _ _ ->
            empty

        CreateLine _ _ ->
            empty

        EditLine _ _ ->
            empty

        FromTo _ ->
            empty

        MirrorAt _ ->
            empty

        CreateDetail _ _ ->
            empty

        EditDetail _ _ ->
            empty


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]


init : ( Model, Cmd Msg )
init =
    ( { maybeDrag = Nothing
      , patternContainerDimensions = Nothing
      , maybeModal = Nothing
      , hoveredPoint = Nothing
      , maybeTool = Nothing
      , preventActionMenuClose = False
      , rightToolbarVisible = True
      , variablesVisible = True
      , maybeVariableDialog = Nothing
      , pointsVisible = False
      , circlesVisible = False
      , linesVisible = False
      , lineSegmentsVisible = False
      , detailsVisible = False
      }
    , Cmd.none
    )



---- VIEW


view : String -> StoredPattern -> Model -> Document Msg
view prefix storedPattern model =
    { title = "Sewing Pattern Editor"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
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
            , Element.inFront <|
                case model.maybeModal of
                    Nothing ->
                        Element.none

                    Just modal ->
                        viewModal storedPattern.pattern modal
            ]
            (viewEditor prefix storedPattern model)
        ]
    }


viewModal : Pattern -> Modal -> Element Msg
viewModal pattern modal =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color View.Design.grayDark
        ]
        (case modal of
            DetailDeleteConfirm thatDetail ->
                Element.column
                    [ Element.centerX
                    , Element.centerY
                    , Element.width (Element.px 400)
                    , Border.width 1
                    , Border.rounded 4
                    , Border.color View.Design.black
                    , View.Design.fontNormal
                    , Background.color View.Design.white
                    , Element.htmlAttribute (Html.Attributes.attribute "role" "dialog")
                    , Element.htmlAttribute (Html.Attributes.attribute "aria-modal" "true")
                    , Element.htmlAttribute <|
                        Html.Attributes.attribute "aria-labelledby" "dialog--title"
                    , Element.htmlAttribute <|
                        Html.Attributes.attribute "aria-describedby" "dialog--body"

                    -- TODO: fix display error in Chromium with scale-factor=1.5
                    , Element.htmlAttribute (Html.Attributes.style "padding-left" "1px")
                    , Element.htmlAttribute (Html.Attributes.style "padding-right" "1px")
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.padding View.Design.small
                        , Background.color View.Design.secondary
                        , Border.roundEach
                            { topLeft = 4
                            , topRight = 4
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        ]
                        [ Element.el
                            [ Element.htmlAttribute (Html.Attributes.id "dialog--title")
                            , Element.centerX
                            , Font.bold
                            ]
                            (Element.text <|
                                "Delete «"
                                    ++ detailName pattern thatDetail
                                    ++ "»?"
                            )
                        , Element.el [ Element.alignRight ] <|
                            View.Input.btnIcon
                                { onPress = Just DetailRemoveDialogCancelClicked
                                , icon = "times"
                                }
                        ]
                    , Element.paragraph
                        [ Element.htmlAttribute (Html.Attributes.id "dialog--body")
                        , Element.width Element.fill
                        , Element.padding View.Design.normal
                        , Background.color View.Design.white
                        ]
                        [ Element.text "Do you want to remove the detail "
                        , Element.el [ Font.bold ]
                            (Element.text ("«" ++ detailName pattern thatDetail ++ "»"))
                        , Element.text "?"
                        ]
                    , Element.row
                        [ Element.width Element.fill
                        , Element.padding View.Design.small
                        , Border.widthEach
                            { top = 1
                            , bottom = 0
                            , left = 0
                            , right = 0
                            }
                        , Border.color View.Design.secondary
                        ]
                        [ View.Input.btnDanger
                            { onPress = Just DetailRemoveDialogDeleteClicked
                            , label = "Delete detail"
                            }
                        , Element.el [ Element.alignRight ] <|
                            View.Input.btnCancel
                                { onPress = Just DetailRemoveDialogCancelClicked
                                , label = "Cancel"
                                }
                        ]
                    ]
        )


viewEditor : String -> StoredPattern -> Model -> Element Msg
viewEditor prefix storedPattern model =
    let
        { pattern, name } =
            storedPattern
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.padding Design.small
            , Element.spacing Design.xSmall
            , Border.widthEach
                { top = 0
                , bottom = 1
                , left = 0
                , right = 0
                }
            , Font.size Design.small
            , Background.color View.Design.white
            , Border.color View.Design.black
            , Font.color View.Design.black
            ]
            [ View.Navigation.link
                { url = "/"
                , label = "Patterns"
                }
            , Element.el [] (View.Icon.fa "angle-right")
            , Element.el [] (Element.text name)
            , Element.newTabLink
                [ Element.alignRight
                , Font.color View.Design.black
                , Font.size Design.small
                , Element.mouseOver
                    [ Font.color View.Design.primaryDark ]
                ]
                { url = "https://github.com/kirchner/sewing-pattern-editor"
                , label = View.Icon.dev "github-plain"
                }
            ]
        , Element.row
            [ Element.height Element.fill
            , Element.width Element.fill

            -- FIXME this seems to be a bug, c.f.
            -- https://github.com/mdgriffith/elm-ui/issues/12
            , Element.clip
            , Element.htmlAttribute <|
                Html.Attributes.style "flex-shrink" "1"
            ]
            [ viewLeftToolbar prefix pattern model
            , Element.el
                [ Element.htmlAttribute <|
                    Html.Attributes.id "pattern-container"
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.inFront <|
                    Element.el
                        [ Element.alignRight
                        , Element.alignBottom
                        ]
                        (viewZoom model)
                , Background.color (Element.rgb 255 255 255)
                ]
                (viewWorkspace storedPattern model)
            , viewRightToolbar pattern model
            ]
        ]


viewLeftToolbar : String -> Pattern -> Model -> Element Msg
viewLeftToolbar prefix pattern model =
    Element.column
        [ Element.width (Element.maximum 330 Element.fill)
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color View.Design.white
        , Border.widthEach
            { left = 0
            , right = 1
            , top = 0
            , bottom = 0
            }
        , Border.color View.Design.black
        ]
        [ case model.maybeTool of
            Nothing ->
                viewToolSelector prefix

            Just tool ->
                viewTool pattern tool
        ]



---- WORKSPACE


viewWorkspace : StoredPattern -> Model -> Element Msg
viewWorkspace storedPattern model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.html <|
            viewPattern
                model.patternContainerDimensions
                model.maybeDrag
                storedPattern
                model
        )


viewPattern :
    Maybe Dimensions
    -> Maybe Drag
    -> StoredPattern
    -> Model
    -> Html Msg
viewPattern maybeDimensions maybeDrag storedPattern model =
    let
        { pattern, center, zoom } =
            storedPattern

        selections =
            { points =
                model.maybeTool
                    |> Maybe.map selectedPointsFromTool
                    |> Maybe.withDefault (Those.fromList [])
            , lines =
                model.maybeTool
                    |> Maybe.map selectedLinesFromTool
                    |> Maybe.withDefault (Those.fromList [])
            , lineSegments =
                model.maybeTool
                    |> Maybe.map selectedLineSegmentsFromTool
                    |> Maybe.withDefault (Those.fromList [])
            , details =
                model.maybeTool
                    |> Maybe.map selectedDetailsFromTool
                    |> Maybe.withDefault (Those.fromList [])
            }

        localFrame =
            ( x, y )
                |> Point2d.fromCoordinates
                |> Frame2d.atPoint

        { x, y } =
            case maybeDrag of
                Nothing ->
                    center

                Just drag ->
                    { x = center.x + (drag.start.x - drag.current.x) / zoom
                    , y = center.y + (drag.start.y - drag.current.y) / zoom
                    }
    in
    case maybeDimensions of
        Nothing ->
            Html.text ""

        Just { width, height } ->
            Svg.svg
                [ Svg.Attributes.viewBox <|
                    String.join " "
                        [ String.fromFloat (width / -2)
                        , String.fromFloat (height / -2)
                        , String.fromFloat width
                        , String.fromFloat height
                        ]
                , Html.Attributes.style "user-select" "none"
                , Html.Events.preventDefaultOn "dragstart" (Decode.succeed ( NoOp, True ))
                , Html.Events.on "mousedown" <|
                    Decode.map MouseDown <|
                        Decode.map2 Position
                            (Decode.field "screenX" Decode.float)
                            (Decode.field "screenY" Decode.float)
                ]
                [ Svg.relativeTo localFrame <|
                    Svg.scaleAbout (Frame2d.originPoint localFrame) zoom <|
                        Svg.g []
                            [ Pattern.draw selections True zoom model.hoveredPoint pattern
                            , Svg.Lazy.lazy4 drawHoverPolygons
                                width
                                height
                                localFrame
                                storedPattern
                            ]
                ]


drawHoverPolygons : Float -> Float -> Frame2d -> StoredPattern -> Svg Msg
drawHoverPolygons width height localFrame { pattern, center, zoom } =
    let
        ( geometry, _ ) =
            Pattern.geometry pattern

        hoverPolygons =
            VoronoiDiagram2d.fromVerticesBy
                (\( _, _, p2d ) -> p2d)
                (Array.fromList geometry.points)
                |> Result.toMaybe
                |> Maybe.map (VoronoiDiagram2d.polygons boundingBox2d)
                |> Maybe.withDefault []

        boundingBox2d =
            BoundingBox2d.fromExtrema
                { minX = -1.1 * width
                , maxX = 1.1 * width
                , minY = -1.1 * height
                , maxY = 1.1 * height
                }
                |> BoundingBox2d.scaleAbout Point2d.origin (1 / zoom)
                |> BoundingBox2d.translateBy
                    (Vector2d.from
                        Point2d.origin
                        (Frame2d.originPoint localFrame)
                    )
    in
    Svg.g []
        (List.map drawHoverPolygon hoverPolygons)


drawHoverPolygon : ( ( That Point, Maybe String, Point2d ), Polygon2d ) -> Svg Msg
drawHoverPolygon ( ( thatPoint, _, _ ), polygon2d ) =
    Svg.polygon2d
        [ Svg.Attributes.fill "transparent"
        , Svg.Events.onMouseOver (PointHovered (Just thatPoint))
        , Svg.Events.onMouseOut (PointHovered Nothing)
        ]
        polygon2d



---- ZOOM


viewZoom : Model -> Element Msg
viewZoom model =
    Element.row
        [ Element.padding 20
        , Element.spacing 10
        ]
        [ View.Input.btnIconLarge
            { onPress = Just ZoomPlusClicked
            , icon = "search-plus"
            }
        , View.Input.btnIconLarge
            { onPress = Just ZoomMinusClicked
            , icon = "search-minus"
            }
        ]


viewRightToolbar : Pattern -> Model -> Element Msg
viewRightToolbar pattern model =
    Element.row
        [ Element.height Element.fill
        , Background.color View.Design.white
        ]
        [ Input.button
            [ Element.height Element.fill
            , Element.padding 5
            , Font.color View.Design.black
            , Border.widthEach
                { left = 1
                , right = 0
                , top = 0
                , bottom = 0
                }
            , Border.color View.Design.black
            , Element.mouseOver
                [ Font.color View.Design.primaryDark
                , Border.color View.Design.primaryDark
                ]
            ]
            { onPress = Just ToolbarToggleClicked
            , label =
                Element.column
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.spacing View.Design.xSmall
                    ]
                    (List.repeat 3
                        (Element.el
                            [ Element.centerY
                            , Element.centerX
                            ]
                            (if model.rightToolbarVisible then
                                View.Icon.fa "chevron-right"

                             else
                                View.Icon.fa "chevron-left"
                            )
                        )
                    )
            }
        , if model.rightToolbarVisible then
            Element.column
                [ Element.width (Element.px 400)
                , Element.height Element.fill
                , Element.padding View.Design.xSmall
                , Element.spacing View.Design.xSmall
                , Element.scrollbarY
                ]
                (case model.maybeVariableDialog of
                    Nothing ->
                        [ viewVariables pattern model
                        , viewPoints pattern model
                        , viewCircles pattern model
                        , viewLines pattern model
                        , viewLineSegments pattern model
                        , viewDetails pattern model
                        ]

                    Just (VariableDialogCreate { name, value }) ->
                        [ viewVariable name value ]
                )

          else
            Element.none
        ]



-- TOOL


viewTool : Pattern -> Tool -> Element Msg
viewTool pattern tool =
    let
        points =
            Pattern.points pattern

        circles =
            Pattern.circles pattern

        lines =
            Pattern.lines pattern

        lineSegments =
            Pattern.lineSegments pattern

        details =
            Pattern.details pattern

        createDialog name content =
            Element.column
                [ Element.width Element.fill
                , Element.padding Design.small
                , Element.spacing Design.normal
                ]
                [ Element.el
                    [ View.Design.fontNormal
                    , Font.color View.Design.black
                    ]
                    (toolDescription (toolToTag tool))
                , case name of
                    Valid _ ->
                        Element.none

                    Invalid value help ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.padding View.Design.small
                            , Element.spacing View.Design.small
                            , Border.width 1
                            , Border.rounded 4
                            , Border.color View.Design.danger
                            , Element.htmlAttribute <|
                                Html.Attributes.id "validation-messages"
                            , Element.htmlAttribute <|
                                Html.Attributes.tabindex -1
                            ]
                            [ Element.el
                                [ Region.heading 2
                                , View.Design.fontLarge
                                , Font.bold
                                ]
                                (Element.text "There's a problem")
                            , Input.button
                                [ View.Design.fontNormal
                                , Font.color View.Design.danger
                                , Font.underline
                                , Element.mouseOver
                                    [ Font.color View.Design.dangerDark ]
                                ]
                                { onPress = Just ValidationNameClicked
                                , label = Element.text help
                                }
                            ]
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Design.small
                    ]
                    (nameInput name :: content)
                , Element.row
                    [ Element.width Element.fill
                    , Element.spacing 5
                    ]
                    [ btnCreate
                    , btnCancel
                    ]
                ]

        editDialog name content =
            Element.column
                [ Element.width Element.fill
                , Element.padding Design.small
                , Element.spacing Design.normal
                ]
                [ Element.el
                    [ View.Design.fontNormal
                    , Font.color View.Design.black
                    ]
                    (toolDescriptionEdit name (toolToTag tool))
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Design.small
                    ]
                    content
                , Element.row
                    [ Element.width Element.fill
                    , Element.spacing 5
                    ]
                    [ btnUpdate
                    , btnCancel
                    ]
                ]

        pointDialog pointData =
            case pointData of
                LeftOf data ->
                    viewSimpleDistanceTool pattern points "leftof" data

                RightOf data ->
                    viewSimpleDistanceTool pattern points "rightof" data

                Above data ->
                    viewSimpleDistanceTool pattern points "above" data

                Below data ->
                    viewSimpleDistanceTool pattern points "below" data

                AtAngle data ->
                    viewAngle pattern points data

                BetweenRatio data ->
                    viewBetweenRatio pattern points data

                BetweenLength data ->
                    viewBetweenLength pattern points data

                CircleCircle data ->
                    viewCircleCircle pattern circles data

                LineLine data ->
                    viewLineLine pattern lines data

                CircleLine data ->
                    viewCircleLine pattern circles lines data

        lineDialog lineData =
            case lineData of
                ThroughTwoPoints data ->
                    viewThroughTwoPoints pattern points data

                ThroughOnePoint data ->
                    viewThroughOnePoint pattern points data

        detailDialog detailData =
            viewDetail pattern points detailData

        btnCreate =
            Element.el [ Element.alignLeft ] <|
                View.Input.btnPrimary
                    { onPress = Just CreateClicked
                    , label = "Create"
                    }

        btnUpdate =
            Element.el [ Element.alignLeft ] <|
                View.Input.btnPrimary
                    { onPress = Just UpdateClicked
                    , label = "Update"
                    }

        btnCancel =
            Element.el [ Element.alignRight ] <|
                View.Input.btnCancel
                    { onPress = Just ToolDialogCancelClicked
                    , label = "Cancel"
                    }
    in
    case tool of
        CreatePoint name pointData ->
            createDialog name (pointDialog pointData)

        EditPoint thatPoint pointData ->
            editDialog (pointName pattern thatPoint) (pointDialog pointData)

        CreateCircle name (CenteredAt data) ->
            createDialog name (viewCenteredAt pattern points data)

        EditCircle thatCircle (CenteredAt data) ->
            editDialog (circleName pattern thatCircle) (viewCenteredAt pattern points data)

        CreateLine name lineData ->
            createDialog name (lineDialog lineData)

        EditLine thatLine lineData ->
            editDialog (lineName pattern thatLine) (lineDialog lineData)

        FromTo data ->
            createDialog (Valid data.name) (viewFromTo pattern points data)

        MirrorAt data ->
            Element.column
                [ Element.width Element.fill
                , Element.padding Design.small
                , Element.spacing Design.normal
                ]
                [ Element.el
                    [ View.Design.fontNormal
                    , Font.color View.Design.black
                    ]
                    (toolDescription (toolToTag tool))
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Design.small
                    ]
                    (viewMirrorAt pattern points lines data)
                , Element.row
                    [ Element.width Element.fill
                    , Element.spacing 5
                    ]
                    [ btnCreate
                    , btnCancel
                    ]
                ]

        CreateDetail name detailData ->
            createDialog name (detailDialog detailData)

        EditDetail thatDetail detailData ->
            editDialog (detailName pattern thatDetail) (detailDialog detailData)


nameInput name =
    case name of
        Valid value ->
            View.Input.text "name-input"
                { onChange = NameChanged
                , text = value
                , label = "Pick a name"
                , help = Nothing
                }

        Invalid value help ->
            View.Input.text "name-input"
                { onChange = NameChanged
                , text = value
                , label = "Pick a name"
                , help = Just help
                }


viewSimpleDistanceTool pattern points toolId data =
    [ View.Input.dropdown (toolId ++ "--anchor")
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "Start point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.formula "distance--input"
        { onChange = DistanceChanged
        , text = data.distance
        , label = "Distance"
        }
    ]


viewAngle pattern points data =
    [ View.Input.dropdown "at-angle--anchor"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "Start point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.formula "angle--input"
        { onChange = AngleChanged
        , text = data.angle
        , label = "Angle"
        }
    , View.Input.formula "distance--input"
        { onChange = DistanceChanged
        , text = data.distance
        , label = "Distance"
        }
    ]


viewBetweenRatio pattern points data =
    [ View.Input.dropdown "between-ratio--anchor-a"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "1st point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.dropdown "between-ratio--anchor-b"
        { lift = DropdownAnchorBMsg
        , entryToString = pointName pattern
        , label = "2nd point"
        , options = points
        , dropdown = data.dropdownAnchorB
        , selection = data.maybeThatAnchorB
        }
    , View.Input.formula "ratio--input"
        { onChange = RatioChanged
        , text = data.ratio
        , label = "Ratio"
        }
    ]


viewBetweenLength pattern points data =
    [ View.Input.dropdown "between-length--anchor-a"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "1st point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.dropdown "between-length--anchor-b"
        { lift = DropdownAnchorBMsg
        , entryToString = pointName pattern
        , label = "2nd point"
        , options = points
        , dropdown = data.dropdownAnchorB
        , selection = data.maybeThatAnchorB
        }
    , View.Input.formula "length--input"
        { onChange = LengthChanged
        , text = data.length
        , label = "Length"
        }
    ]


viewCircleCircle pattern circles data =
    [ View.Input.dropdown "circle-circle--circle-a"
        { lift = DropdownCircleAMsg
        , entryToString = circleName pattern
        , label = "1st circle"
        , options = circles
        , dropdown = data.dropdownCircleA
        , selection = data.maybeThatCircleA
        }
    , View.Input.dropdown "circle-circle--circle-b"
        { lift = DropdownCircleBMsg
        , entryToString = circleName pattern
        , label = "2nd circle"
        , options = circles
        , dropdown = data.dropdownCircleB
        , selection = data.maybeThatCircleB
        }
    , View.Input.radioRow "circle-circle--intersection"
        { onChange = FirstChanged
        , options =
            [ View.Input.option True "first"
            , View.Input.option False "second"
            ]
        , selected = Just data.first
        , label =
            "Which intersection?"
        }
    ]


viewLineLine pattern lines data =
    [ View.Input.dropdown "line-line--line-a"
        { lift = DropdownLineAMsg
        , entryToString = lineName pattern
        , label = "1st line"
        , options = lines
        , dropdown = data.dropdownLineA
        , selection = data.maybeThatLineA
        }
    , View.Input.dropdown "line-line--line-b"
        { lift = DropdownLineBMsg
        , entryToString = lineName pattern
        , label = "2nd line"
        , options = lines
        , dropdown = data.dropdownLineB
        , selection = data.maybeThatLineB
        }
    ]


viewCircleLine pattern circles lines data =
    [ View.Input.dropdown "circle-line--circle"
        { lift = DropdownCircleAMsg
        , entryToString = circleName pattern
        , label = "Circle"
        , options = circles
        , dropdown = data.dropdownCircleA
        , selection = data.maybeThatCircleA
        }
    , View.Input.dropdown "circle-line--line"
        { lift = DropdownLineAMsg
        , entryToString = lineName pattern
        , label = "Line"
        , options = lines
        , dropdown = data.dropdownLineA
        , selection = data.maybeThatLineA
        }
    , View.Input.radioRow "circle-line--intersection"
        { onChange = FirstChanged
        , options =
            [ View.Input.option True "first"
            , View.Input.option False "second"
            ]
        , selected = Just data.first
        , label =
            "Which intersection?"
        }
    ]


viewCenteredAt pattern points data =
    [ View.Input.dropdown "centered-at--anchor"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "Center point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.formula "radius--input"
        { onChange = RadiusChanged
        , text = data.radius
        , label = "Radius"
        }
    ]


viewThroughTwoPoints pattern points data =
    [ View.Input.dropdown "through-two-points--anchor-a"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "1st point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.dropdown "through-two-points--anchor-b"
        { lift = DropdownAnchorBMsg
        , entryToString = pointName pattern
        , label = "2nd point"
        , options = points
        , dropdown = data.dropdownAnchorB
        , selection = data.maybeThatAnchorB
        }
    ]


viewThroughOnePoint pattern points data =
    [ View.Input.dropdown "through-two-points--anchor-a"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "Point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.formula "angle--input"
        { onChange = AngleChanged
        , text = data.angle
        , label = "Angle"
        }
    ]


viewFromTo pattern points data =
    [ View.Input.dropdown "from-to--anchor-a"
        { lift = DropdownAnchorAMsg
        , entryToString = pointName pattern
        , label = "Start point"
        , options = points
        , dropdown = data.dropdownAnchorA
        , selection = data.maybeThatAnchorA
        }
    , View.Input.dropdown "from-to--anchor-b"
        { lift = DropdownAnchorBMsg
        , entryToString = pointName pattern
        , label = "End point"
        , options = points
        , dropdown = data.dropdownAnchorB
        , selection = data.maybeThatAnchorB
        }
    ]


viewMirrorAt pattern points lines data =
    [ View.Input.dropdown "mirror-at-line--line"
        { lift = DropdownLineAMsg
        , entryToString = lineName pattern
        , label = "Mirror line"
        , options = lines
        , dropdown = data.dropdownLineA
        , selection = data.maybeThatLineA
        }
    , View.Input.listbox "mirror-at-points--points"
        { lift = ListboxPointsMsg
        , entryToString = pointName pattern
        , label = "Mirrored points"
        , options = points
        , listbox = data.listbox
        , selection = data.thosePoints
        }
    ]


viewDetail pattern points data =
    let
        buttonAddPointAtEnd =
            View.Input.btnSecondary "detail--add-point--button"
                { onPress = Just DetailAddPointAtEnd
                , label = "Add point"
                }

        viewDropdownPoint id index maybeActionMenu lift label dropdown maybeThatPoint =
            Element.column
                [ Element.width Element.fill
                , Element.spacing Design.xxSmall
                ]
                [ View.Input.dropdownWithMenu
                    (case maybeActionMenu of
                        Nothing ->
                            Element.none

                        Just actionMenu ->
                            viewActionMenu index actionMenu
                    )
                    id
                    { lift = lift
                    , entryToString = pointName pattern
                    , label = label
                    , options = points
                    , dropdown = dropdown
                    , selection = maybeThatPoint
                    }
                ]

        viewActionMenu index actionMenu =
            Element.row
                (if actionMenu == Closed then
                    [ Element.spacing Design.xxSmall
                    , Element.alignRight
                    ]

                 else
                    [ Element.spacing Design.xxSmall
                    , Element.alignRight
                    , Element.htmlAttribute <|
                        Html.Attributes.style "z-index" "1"
                    ]
                )
                [ Input.button
                    [ Element.paddingEach
                        { left = Design.xxSmall
                        , right = Design.xxSmall
                        , top = Design.xxSmall
                        , bottom = Design.xxSmall - 2
                        }
                    , Font.size 10
                    , Font.color View.Design.black
                    , Border.widthEach
                        { left = 0
                        , right = 0
                        , top = 0
                        , bottom = 2
                        }
                    , Border.color View.Design.secondary
                    , Background.color View.Design.secondary
                    , Element.mouseOver
                        [ Background.color View.Design.secondaryDark
                        , Border.color View.Design.black
                        ]
                    , Element.focused
                        [ Border.color View.Design.black ]
                    , Element.htmlAttribute <|
                        Html.Attributes.style "transition" <|
                            String.join "; "
                                [ "background-color 0.2s ease-in-out 0s"
                                , "border-color 0.2s ease-in-out 0s"
                                ]
                    , Element.below <|
                        case actionMenu of
                            Closed ->
                                Element.none

                            _ ->
                                let
                                    viewAction msg label =
                                        Element.el
                                            [ Element.paddingXY 8 7
                                            , Element.width Element.fill
                                            , Background.color View.Design.secondary
                                            , Element.mouseOver
                                                [ Background.color View.Design.secondaryDark ]
                                            , Element.htmlAttribute <|
                                                Html.Attributes.tabindex -1
                                            , Element.htmlAttribute <|
                                                Html.Events.stopPropagationOn "click" <|
                                                    Decode.succeed
                                                        ( msg, True )
                                            ]
                                            (Element.text label)
                                in
                                Element.column
                                    [ Events.onMouseDown (ActionMenuMouseDown index)
                                    , Events.onMouseUp (ActionMenuMouseUp index)
                                    , Element.moveDown 2
                                    , Font.size 14
                                    , Font.color View.Design.black
                                    ]
                                    [ viewAction NoOp "Move down"
                                    , viewAction NoOp "Move up"
                                    , viewAction NoOp "Insert point before"
                                    , viewAction NoOp "Insert point after"
                                    , viewAction (DetailRemovePointClicked index) "Remove"
                                    ]
                    , Events.onLoseFocus (ActionMenuLostFocus index)
                    ]
                    { onPress = Just (ActionMenuClicked index)
                    , label =
                        Element.row
                            [ Element.spacing Design.xxSmall ]
                            [ Element.text "Actions"
                            , View.Icon.fa "angle-down"
                            ]
                    }
                ]
    in
    case data of
        DetailOnePoint detailData ->
            [ viewDropdownPoint "detail-point--first-point"
                0
                Nothing
                (DropdownPointMsg 0)
                "Point #1"
                detailData.firstPointDropdown
                detailData.firstPointMaybeThat
            , buttonAddPointAtEnd
            ]

        DetailManyPoints detailData ->
            let
                viewPoint index { dropdown, maybeThat, actionMenu, connectionPrevious } =
                    [ viewConnection (index + 1)
                        (ConnectionChanged (index + 1))
                        connectionPrevious
                        ("Connection from point #"
                            ++ String.fromInt (index + 2)
                            ++ " to point #"
                            ++ String.fromInt (index + 3)
                        )
                    , viewDropdownPoint
                        ("detail-point--point-"
                            ++ String.fromInt (index + 2)
                        )
                        (index + 2)
                        (Just actionMenu)
                        (DropdownPointMsg (index + 2))
                        ("Point #" ++ String.fromInt (index + 3))
                        dropdown
                        maybeThat
                    ]

                viewConnection index lift connection label =
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Design.xSmall
                        ]
                        [ View.Input.radioRow
                            ("detail--connection-" ++ String.fromInt index)
                            { onChange = lift
                            , options =
                                [ View.Input.option ConnectionStraightTag "straight"
                                , View.Input.option ConnectionQuadraticTag "quadratic"
                                ]
                            , selected =
                                Just <|
                                    case connection of
                                        ConnectionStraight ->
                                            ConnectionStraightTag

                                        ConnectionQuadratic _ ->
                                            ConnectionQuadraticTag
                            , label = label
                            }
                        , case connection of
                            ConnectionStraight ->
                                Element.none

                            ConnectionQuadratic { dropdown, maybeThat } ->
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.paddingEach
                                        { top = 0
                                        , bottom = 0
                                        , left = Design.small
                                        , right = 0
                                        }
                                    ]
                                    [ viewDropdownPoint
                                        ("detail-point--control-point-"
                                            ++ String.fromInt index
                                        )
                                        index
                                        Nothing
                                        (DropdownControlPointMsg index)
                                        "Control point"
                                        dropdown
                                        maybeThat
                                    ]
                        ]
            in
            List.concat
                [ [ viewDropdownPoint "detail-point--point-0"
                        0
                        (Just detailData.firstPointActionMenu)
                        (DropdownPointMsg 0)
                        "Point #1"
                        detailData.firstPointDropdown
                        detailData.firstPointMaybeThat
                  , viewConnection 0
                        (ConnectionChanged 0)
                        detailData.connectionFirstSecond
                        "Connection from point #1 to point #2"
                  , viewDropdownPoint "detail-point--point-1"
                        1
                        (Just detailData.secondPointActionMenu)
                        (DropdownPointMsg 1)
                        "Point #2"
                        detailData.secondPointDropdown
                        detailData.secondPointMaybeThat
                  ]
                , List.concat (List.indexedMap viewPoint detailData.otherPoints)
                , [ buttonAddPointAtEnd
                  , viewConnection (1 + List.length detailData.otherPoints)
                        (ConnectionChanged (1 + List.length detailData.otherPoints))
                        detailData.connectionLastFirst
                        ("Connection from point #"
                            ++ String.fromInt (2 + List.length detailData.otherPoints)
                            ++ " to point #1"
                        )
                  ]
                ]


pointName : Pattern -> That Point -> String
pointName pattern =
    Pattern.getPoint pattern
        >> Maybe.andThen .name
        >> Maybe.withDefault "<unnamed>"


circleName : Pattern -> That Circle -> String
circleName pattern =
    Pattern.getCircle pattern
        >> Maybe.andThen .name
        >> Maybe.withDefault "<unnamed>"


lineName : Pattern -> That Line -> String
lineName pattern =
    Pattern.getLine pattern
        >> Maybe.andThen .name
        >> Maybe.withDefault "<unnamed>"


lineSegmentName : Pattern -> That LineSegment -> String
lineSegmentName pattern =
    Pattern.getLineSegment pattern
        >> Maybe.andThen .name
        >> Maybe.withDefault "<unnamed>"


detailName : Pattern -> That Detail -> String
detailName pattern =
    Pattern.getDetail pattern
        >> Maybe.andThen .name
        >> Maybe.withDefault "<unnamed>"



-- VARIABLE


viewVariable : String -> String -> Element Msg
viewVariable name value =
    Element.column
        [ Element.width Element.fill
        , Element.padding 15
        , Element.spacing 15
        ]
        [ Element.paragraph
            [ Font.size 12
            , Font.color View.Design.black
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
            [ View.Input.text "name-input"
                { onChange = VariableNameChanged
                , text = name
                , label = "Pick a name"
                , help = Nothing
                }
            , View.Input.formula "variable-value--input"
                { onChange = VariableValueChanged
                , text = value
                , label = "Value"
                }
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 5
            ]
            [ Element.el [ Element.alignLeft ] <|
                View.Input.btnPrimary
                    { onPress = Just VariableCreateSubmitClicked
                    , label = "Create"
                    }
            , Element.el [ Element.alignRight ] <|
                View.Input.btnCancel
                    { onPress = Just VariableDialogCancelClicked
                    , label = "Cancel"
                    }
            ]
        ]



-- TOOL SELECTOR


viewToolSelector : String -> Element Msg
viewToolSelector prefix =
    let
        viewGroup name buttons =
            Element.column
                [ Element.spacing Design.xSmall
                , Element.width Element.fill
                ]
                [ Element.el
                    [ Font.size 12
                    , Font.color View.Design.black
                    ]
                    (Element.text name)
                , Element.column
                    [ Element.spacing Design.xxSmall
                    , Element.width Element.fill
                    ]
                    buttons
                ]

        button toolTag label =
            View.Input.btnSecondaryWide (toolTagToId toolTag ++ "-button")
                { onPress = Just (SelectToolClicked toolTag)
                , label = label
                }
    in
    Element.column
        [ Element.padding Design.small
        , Element.spacing Design.small
        , Element.width Element.fill
        ]
        [ viewGroup "Create a point"
            [ button LeftOfTag "Left of a point"
            , button RightOfTag "Right of a point"
            , button AboveTag "Above a point"
            , button BelowTag "Below a point"
            , button AtAngleTag "Relative to a point by angle and distance"
            , button BetweenRatioTag "Between two points at ratio"
            , button BetweenLengthTag "Between two points at length"
            , button CircleCircleTag "At intersection of two circles"
            , button LineLineTag "At intersection of two lines"
            , button CircleLineTag "At intersection of a circle and a line"
            ]
        , viewGroup "Create a circle"
            [ button CenteredAtTag "Centered at a point"
            ]
        , viewGroup "Create a line"
            [ button ThroughTwoPointsTag "Through two points"
            , button ThroughOnePointTag "Through one point"
            ]
        , viewGroup "Create a line segment"
            [ button FromToTag "Between two points"
            ]
        , viewGroup "Create a transformation"
            [ button MirrorAtTag "Mirror points at a line"
            ]
        , viewGroup "Create a detail"
            [ button DetailTag "Detail" ]
        ]



-- TABLES


viewVariables : Pattern -> Model -> Element Msg
viewVariables pattern model =
    View.Navigation.accordion
        { onPress = VariablesRulerClicked
        , label = "Variables"
        , open = model.variablesVisible
        , content =
            Element.column
                [ Element.width Element.fill
                , Element.spacing Design.small
                ]
                [ Element.table
                    [ Element.spacing Design.xSmall ]
                    { data = List.sortBy .name (Pattern.variables pattern)
                    , columns =
                        [ View.Table.column
                            { label = "Name"
                            , recordToString = .name
                            }
                        , View.Table.columnFloat
                            { label = "Value"
                            , recordToFloat = Just << .computed
                            }
                        , View.Table.columnActions
                            { onEditPress = always Nothing
                            , onRemovePress = always Nothing
                            }
                        ]
                    }
                , Element.el [ Element.alignRight ] <|
                    View.Input.btnSecondary "create-variable--button"
                        { onPress = Just VariableCreateClicked
                        , label = "Create variable"
                        }
                ]
        }


viewPoints : Pattern -> Model -> Element Msg
viewPoints pattern model =
    View.Navigation.accordion
        { onPress = PointsRulerClicked
        , label = "Points"
        , open = model.pointsVisible
        , content =
            View.Table.table
                { data =
                    List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
                        (Pattern.points pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            \( _, { name } ) ->
                                Maybe.withDefault "<no name>" name
                        }
                    , View.Table.columnFloat
                        { label = "x"
                        , recordToFloat =
                            \( thatPoint, _ ) ->
                                thatPoint
                                    |> Pattern.getPointGeometry pattern
                                    |> Maybe.map Point2d.xCoordinate
                        }
                    , View.Table.columnFloat
                        { label = "y"
                        , recordToFloat =
                            \( thatPoint, _ ) ->
                                thatPoint
                                    |> Pattern.getPointGeometry pattern
                                    |> Maybe.map Point2d.yCoordinate
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << EditPointClicked << Tuple.first
                        , onRemovePress = always Nothing
                        }
                    ]
                }
        }


viewCircles : Pattern -> Model -> Element Msg
viewCircles pattern model =
    View.Navigation.accordion
        { onPress = CirclesRulerClicked
        , label = "Circles"
        , open = model.circlesVisible
        , content =
            View.Table.table
                { data =
                    List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
                        (Pattern.circles pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            \( _, { name } ) ->
                                Maybe.withDefault "<no name>" name
                        }
                    , View.Table.columnFloat
                        { label = "x"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getCircleGeometry pattern
                                    |> Maybe.map
                                        (Circle2d.centerPoint >> Point2d.xCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "y"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getCircleGeometry pattern
                                    |> Maybe.map
                                        (Circle2d.centerPoint >> Point2d.yCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "r"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getCircleGeometry pattern
                                    |> Maybe.map Circle2d.radius
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << CircleEditClicked << Tuple.first
                        , onRemovePress = always Nothing
                        }
                    ]
                }
        }


viewLines : Pattern -> Model -> Element Msg
viewLines pattern model =
    View.Navigation.accordion
        { onPress = LinesRulerClicked
        , label = "Lines"
        , open = model.linesVisible
        , content =
            View.Table.table
                { data =
                    List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
                        (Pattern.lines pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            \( _, { name } ) ->
                                Maybe.withDefault "<no name>" name
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << LineEditClicked << Tuple.first
                        , onRemovePress = always Nothing
                        }
                    ]
                }
        }


viewLineSegments : Pattern -> Model -> Element Msg
viewLineSegments pattern model =
    View.Navigation.accordion
        { onPress = LineSegmentsRulerClicked
        , label = "Line segments"
        , open = model.lineSegmentsVisible
        , content =
            View.Table.table
                { data =
                    List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
                        (Pattern.lineSegments pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            \( _, { name } ) ->
                                Maybe.withDefault "<no name>" name
                        }
                    , View.Table.columnFloat
                        { label = "x1"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getLineSegmentGeometry pattern
                                    |> Maybe.map
                                        (LineSegment2d.startPoint >> Point2d.xCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "y1"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getLineSegmentGeometry pattern
                                    |> Maybe.map
                                        (LineSegment2d.startPoint >> Point2d.yCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "x2"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getLineSegmentGeometry pattern
                                    |> Maybe.map
                                        (LineSegment2d.endPoint >> Point2d.xCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "y2"
                        , recordToFloat =
                            \( thatCircle, _ ) ->
                                thatCircle
                                    |> Pattern.getLineSegmentGeometry pattern
                                    |> Maybe.map
                                        (LineSegment2d.endPoint >> Point2d.yCoordinate)
                        }
                    , View.Table.columnActions
                        { onEditPress = always Nothing
                        , onRemovePress = always Nothing
                        }
                    ]
                }
        }


viewDetails : Pattern -> Model -> Element Msg
viewDetails pattern model =
    View.Navigation.accordion
        { onPress = DetailsRulerClicked
        , label = "Details"
        , open = model.detailsVisible
        , content =
            View.Table.table
                { data =
                    List.sortBy (Tuple.second >> .name >> Maybe.withDefault "")
                        (Pattern.details pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            \( _, { name } ) ->
                                Maybe.withDefault "<no name>" name
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << DetailEditClicked << Tuple.first
                        , onRemovePress =
                            \( thatDetail, _ ) ->
                                Just (DetailRemoveClicked thatDetail)
                        }
                    ]
                }
        }



---- UPDATE


type Msg
    = NoOp
    | WindowResized
    | PatternContainerViewportRequested
    | PatternContainerViewportReceived (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ZoomPlusClicked
    | ZoomMinusClicked
    | MouseDown Position
    | MouseMove Position
    | MouseUp Position
      -- TOOL POINTS
    | SelectToolClicked ToolTag
    | EditPointClicked (That Point)
      -- TOOL PARAMETERS
    | NameChanged String
    | AngleChanged String
    | DistanceChanged String
    | RatioChanged String
    | LengthChanged String
    | RadiusChanged String
    | DropdownAnchorAMsg (Dropdown.Msg (That Point))
    | DropdownAnchorBMsg (Dropdown.Msg (That Point))
    | DropdownLineAMsg (Dropdown.Msg (That Line))
    | DropdownLineBMsg (Dropdown.Msg (That Line))
    | ListboxPointsMsg (Listbox.Msg (That Point))
    | DropdownCircleAMsg (Dropdown.Msg (That Circle))
    | DropdownCircleBMsg (Dropdown.Msg (That Circle))
    | FirstChanged Bool
      -- DETAIL
    | DropdownPointMsg Int (Dropdown.Msg (That Point))
    | DropdownControlPointMsg Int (Dropdown.Msg (That Point))
    | ActionMenuClicked Int
    | ActionMenuLostFocus Int
    | ActionMenuMouseDown Int
    | ActionMenuMouseUp Int
    | DetailRemovePointClicked Int
    | ConnectionChanged Int ConnectionTag
    | DetailAddPointAtEnd
      -- TOOL ACTIONS
    | CreateClicked
    | UpdateClicked
    | ToolDialogCancelClicked
    | ValidationNameClicked
      -- PATTERN
    | PointHovered (Maybe (That Point))
      -- RIGHT TOOLBAR
    | ToolbarToggleClicked
    | VariablesRulerClicked
    | VariableCreateClicked
    | PointsRulerClicked
    | CirclesRulerClicked
    | CircleEditClicked (That Circle)
    | LinesRulerClicked
    | LineEditClicked (That Line)
    | LineSegmentsRulerClicked
    | DetailsRulerClicked
    | DetailEditClicked (That Detail)
    | DetailRemoveClicked (That Detail)
    | DetailRemoveDialogDeleteClicked
    | DetailRemoveDialogCancelClicked
      -- VARIABLE DIALOG
    | VariableNameChanged String
    | VariableValueChanged String
    | VariableCreateSubmitClicked
    | VariableDialogCancelClicked


update :
    Navigation.Key
    -> StoredPattern
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, Maybe StoredPattern )
update key ({ pattern, zoom, center } as storedPattern) msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Nothing )

        WindowResized ->
            ( model
            , Browser.Dom.getViewportOf "pattern-container"
                |> Task.attempt PatternContainerViewportReceived
            , Nothing
            )

        PatternContainerViewportRequested ->
            ( model
            , Process.sleep 500
                |> Task.andThen (\_ -> Browser.Dom.getViewportOf "pattern-container")
                |> Task.attempt PatternContainerViewportReceived
            , Nothing
            )

        PatternContainerViewportReceived result ->
            case result of
                Err _ ->
                    ( model, Cmd.none, Nothing )

                Ok viewport ->
                    ( { model
                        | patternContainerDimensions =
                            Just
                                { width = viewport.viewport.width
                                , height = viewport.viewport.height
                                }
                      }
                    , Cmd.none
                    , Nothing
                    )

        ZoomPlusClicked ->
            ( model
            , Cmd.none
            , Just { storedPattern | zoom = zoom * 1.1 }
            )

        ZoomMinusClicked ->
            ( model
            , Cmd.none
            , Just { storedPattern | zoom = zoom / 1.1 }
            )

        MouseDown position ->
            ( { model
                | maybeDrag =
                    Just
                        { start = position
                        , current = position
                        }
              }
            , Cmd.none
            , Nothing
            )

        MouseMove position ->
            case model.maybeDrag of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just drag ->
                    ( { model | maybeDrag = Just { drag | current = position } }
                    , Cmd.none
                    , Nothing
                    )

        MouseUp position ->
            let
                newCenter =
                    case model.maybeDrag of
                        Nothing ->
                            center

                        Just drag ->
                            { x = center.x + (drag.start.x - position.x) / zoom
                            , y = center.y + (drag.start.y - position.y) / zoom
                            }
            in
            ( { model | maybeDrag = Nothing }
            , Cmd.none
            , Just { storedPattern | center = newCenter }
            )

        -- POINTS
        SelectToolClicked toolTag ->
            let
                createPoint toPointData =
                    CreatePoint (Valid "") << toPointData

                createCircle toCircleData =
                    CreateCircle (Valid "") << toCircleData

                createLine toLineData =
                    CreateLine (Valid "") << toLineData

                tool =
                    case toolTag of
                        LeftOfTag ->
                            createPoint LeftOf
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , distance = ""
                                }

                        RightOfTag ->
                            createPoint RightOf
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , distance = ""
                                }

                        AboveTag ->
                            createPoint Above
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , distance = ""
                                }

                        BelowTag ->
                            createPoint Below
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , distance = ""
                                }

                        AtAngleTag ->
                            createPoint AtAngle
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , angle = ""
                                , distance = ""
                                }

                        BetweenRatioTag ->
                            createPoint BetweenRatio
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , dropdownAnchorB = Dropdown.init
                                , maybeThatAnchorB = Nothing
                                , ratio = ""
                                }

                        BetweenLengthTag ->
                            createPoint BetweenLength
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , dropdownAnchorB = Dropdown.init
                                , maybeThatAnchorB = Nothing
                                , length = ""
                                }

                        CircleCircleTag ->
                            createPoint CircleCircle
                                { dropdownCircleA = Dropdown.init
                                , maybeThatCircleA = Nothing
                                , dropdownCircleB = Dropdown.init
                                , maybeThatCircleB = Nothing
                                , first = True
                                }

                        LineLineTag ->
                            createPoint LineLine
                                { dropdownLineA = Dropdown.init
                                , maybeThatLineA = Nothing
                                , dropdownLineB = Dropdown.init
                                , maybeThatLineB = Nothing
                                }

                        CircleLineTag ->
                            createPoint CircleLine
                                { dropdownCircleA = Dropdown.init
                                , maybeThatCircleA = Nothing
                                , dropdownLineA = Dropdown.init
                                , maybeThatLineA = Nothing
                                , first = True
                                }

                        CenteredAtTag ->
                            createCircle CenteredAt
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , radius = ""
                                }

                        ThroughTwoPointsTag ->
                            createLine ThroughTwoPoints
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , dropdownAnchorB = Dropdown.init
                                , maybeThatAnchorB = Nothing
                                }

                        ThroughOnePointTag ->
                            createLine ThroughOnePoint
                                { dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , angle = ""
                                }

                        FromToTag ->
                            FromTo
                                { name = ""
                                , dropdownAnchorA = Dropdown.init
                                , maybeThatAnchorA = Nothing
                                , dropdownAnchorB = Dropdown.init
                                , maybeThatAnchorB = Nothing
                                }

                        MirrorAtTag ->
                            MirrorAt
                                { dropdownLineA = Dropdown.init
                                , maybeThatLineA = Nothing
                                , listbox = Listbox.init
                                , thosePoints = Those.none
                                }

                        DetailTag ->
                            (CreateDetail (Valid "") << DetailOnePoint)
                                { firstPointDropdown = Dropdown.init
                                , firstPointMaybeThat = Nothing
                                }
            in
            ( { model | maybeTool = Just tool }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        EditPointClicked thatPoint ->
            case Pattern.getPoint pattern thatPoint of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just { name, value } ->
                    let
                        editPoint toPointData =
                            Just << EditPoint thatPoint << toPointData

                        maybeTool =
                            case value of
                                Pattern.Origin _ _ ->
                                    Nothing

                                Pattern.LeftOf thatAnchor distance ->
                                    editPoint LeftOf
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchor
                                        , distance = distance
                                        }

                                Pattern.RightOf thatAnchor distance ->
                                    editPoint RightOf
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchor
                                        , distance = distance
                                        }

                                Pattern.Above thatAnchor distance ->
                                    editPoint Above
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchor
                                        , distance = distance
                                        }

                                Pattern.Below thatAnchor distance ->
                                    editPoint Below
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchor
                                        , distance = distance
                                        }

                                Pattern.AtAngle thatAnchor angle distance ->
                                    editPoint AtAngle
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchor
                                        , angle = angle
                                        , distance = distance
                                        }

                                Pattern.BetweenRatio thatAnchorA thatAnchorB ratio ->
                                    editPoint BetweenRatio
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchorA
                                        , dropdownAnchorB = Dropdown.init
                                        , maybeThatAnchorB = Just thatAnchorB
                                        , ratio = ratio
                                        }

                                Pattern.BetweenLength thatAnchorA thatAnchorB length ->
                                    editPoint BetweenLength
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchorA
                                        , dropdownAnchorB = Dropdown.init
                                        , maybeThatAnchorB = Just thatAnchorB
                                        , length = length
                                        }

                                Pattern.LineLine thatLineA thatLineB ->
                                    editPoint LineLine
                                        { dropdownLineA = Dropdown.init
                                        , maybeThatLineA = Just thatLineA
                                        , dropdownLineB = Dropdown.init
                                        , maybeThatLineB = Just thatLineB
                                        }

                                Pattern.FirstCircleCircle thatCircleA thatCircleB ->
                                    editPoint CircleCircle
                                        { dropdownCircleA = Dropdown.init
                                        , maybeThatCircleA = Just thatCircleA
                                        , dropdownCircleB = Dropdown.init
                                        , maybeThatCircleB = Just thatCircleB
                                        , first = True
                                        }

                                Pattern.SecondCircleCircle thatCircleA thatCircleB ->
                                    editPoint CircleCircle
                                        { dropdownCircleA = Dropdown.init
                                        , maybeThatCircleA = Just thatCircleA
                                        , dropdownCircleB = Dropdown.init
                                        , maybeThatCircleB = Just thatCircleB
                                        , first = False
                                        }

                                Pattern.FirstCircleLine thatCircle thatLine ->
                                    editPoint CircleLine
                                        { dropdownCircleA = Dropdown.init
                                        , maybeThatCircleA = Just thatCircle
                                        , dropdownLineA = Dropdown.init
                                        , maybeThatLineA = Just thatLine
                                        , first = True
                                        }

                                Pattern.SecondCircleLine thatCircle thatLine ->
                                    editPoint CircleLine
                                        { dropdownCircleA = Dropdown.init
                                        , maybeThatCircleA = Just thatCircle
                                        , dropdownLineA = Dropdown.init
                                        , maybeThatLineA = Just thatLine
                                        , first = False
                                        }

                                Pattern.TransformBy _ _ ->
                                    Nothing
                    in
                    ( { model | maybeTool = maybeTool }
                    , Cmd.none
                    , Nothing
                    )

        -- TOOL PARAMETERS
        NameChanged newName ->
            let
                updateName toTool data =
                    ( { model | maybeTool = Just (toTool { data | name = newName }) }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just (CreatePoint name toolData) ->
                    ( { model
                        | maybeTool =
                            Just (CreatePoint (updateValue newName name) toolData)
                      }
                    , Cmd.none
                    , Nothing
                    )

                Just (EditPoint _ _) ->
                    ( model, Cmd.none, Nothing )

                Just (CreateCircle name toolData) ->
                    ( { model
                        | maybeTool =
                            Just (CreateCircle (updateValue newName name) toolData)
                      }
                    , Cmd.none
                    , Nothing
                    )

                Just (EditCircle _ _) ->
                    ( model, Cmd.none, Nothing )

                Just (CreateLine name toolData) ->
                    ( { model
                        | maybeTool =
                            Just (CreateLine (updateValue newName name) toolData)
                      }
                    , Cmd.none
                    , Nothing
                    )

                Just (EditLine _ _) ->
                    ( model, Cmd.none, Nothing )

                Just (FromTo data) ->
                    updateName FromTo data

                Just (MirrorAt _) ->
                    ( model, Cmd.none, Nothing )

                Just (CreateDetail name toolData) ->
                    ( { model
                        | maybeTool =
                            Just (CreateDetail (updateValue newName name) toolData)
                      }
                    , Cmd.none
                    , Nothing
                    )

                Just (EditDetail _ _) ->
                    ( model, Cmd.none, Nothing )

        AngleChanged newAngle ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        AtAngle data ->
                            updateDataWith (toTool << AtAngle) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateLineDataWith toTool lineData =
                    case lineData of
                        ThroughOnePoint data ->
                            updateDataWith (toTool << ThroughOnePoint) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    ( { model | maybeTool = Just (toTool { data | angle = newAngle }) }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                Just (CreateLine name pointData) ->
                    updateLineDataWith (CreateLine name) pointData

                Just (EditLine thatLine pointData) ->
                    updateLineDataWith (EditLine thatLine) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        DistanceChanged newDistance ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        LeftOf data ->
                            updateDataWith (toTool << LeftOf) data

                        RightOf data ->
                            updateDataWith (toTool << RightOf) data

                        Above data ->
                            updateDataWith (toTool << Above) data

                        Below data ->
                            updateDataWith (toTool << Below) data

                        AtAngle data ->
                            updateDataWith (toTool << AtAngle) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    ( { model
                        | maybeTool = Just (toTool { data | distance = newDistance })
                      }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        RatioChanged newRatio ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        BetweenRatio data ->
                            updateDataWith (toTool << BetweenRatio) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    ( { model | maybeTool = Just (toTool { data | ratio = newRatio }) }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        LengthChanged newLength ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        BetweenLength data ->
                            updateDataWith (toTool << BetweenLength) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    ( { model | maybeTool = Just (toTool { data | length = newLength }) }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        RadiusChanged newRadius ->
            let
                updateCircleDataWith toTool circleData =
                    case circleData of
                        CenteredAt data ->
                            updateDataWith (toTool << CenteredAt) data

                updateDataWith toTool data =
                    ( { model | maybeTool = Just (toTool { data | radius = newRadius }) }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreateCircle name circleData) ->
                    updateCircleDataWith (CreateCircle name) circleData

                Just (EditCircle thatCircle circleData) ->
                    updateCircleDataWith (EditCircle thatCircle) circleData

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownAnchorAMsg dropdownMsg ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        LeftOf data ->
                            updateDataWith (toTool << LeftOf) data

                        RightOf data ->
                            updateDataWith (toTool << RightOf) data

                        Above data ->
                            updateDataWith (toTool << Above) data

                        Below data ->
                            updateDataWith (toTool << Below) data

                        AtAngle data ->
                            updateDataWith (toTool << AtAngle) data

                        BetweenRatio data ->
                            updateDataWith (toTool << BetweenRatio) data

                        BetweenLength data ->
                            updateDataWith (toTool << BetweenLength) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateCircleDataWith toTool circleData =
                    case circleData of
                        CenteredAt data ->
                            updateDataWith (toTool << CenteredAt) data

                updateLineDataWith toTool lineData =
                    case lineData of
                        ThroughTwoPoints data ->
                            updateDataWith (toTool << ThroughTwoPoints) data

                        ThroughOnePoint data ->
                            updateDataWith (toTool << ThroughOnePoint) data

                updateDataWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatAnchor ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                data.dropdownAnchorA
                                data.maybeThatAnchorA

                        newTool =
                            toTool
                                { data
                                    | dropdownAnchorA = newDropdown
                                    , maybeThatAnchorA = newMaybeThatAnchor
                                }
                    in
                    ( { model | maybeTool = Just newTool }
                    , Cmd.map DropdownAnchorAMsg dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                Just (CreateCircle name circleData) ->
                    updateCircleDataWith (CreateCircle name) circleData

                Just (EditCircle thatCircle circleData) ->
                    updateCircleDataWith (EditCircle thatCircle) circleData

                Just (CreateLine name circleData) ->
                    updateLineDataWith (CreateLine name) circleData

                Just (EditLine thatLine circleData) ->
                    updateLineDataWith (EditLine thatLine) circleData

                Just (FromTo data) ->
                    updateDataWith FromTo data

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownAnchorBMsg dropdownMsg ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        BetweenRatio data ->
                            updateDataWith (toTool << BetweenRatio) data

                        BetweenLength data ->
                            updateDataWith (toTool << BetweenLength) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateLineDataWith toTool lineData =
                    case lineData of
                        ThroughTwoPoints data ->
                            updateDataWith (toTool << ThroughTwoPoints) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatAnchor ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.points pattern)
                                dropdownMsg
                                data.dropdownAnchorB
                                data.maybeThatAnchorB

                        newData =
                            { data
                                | dropdownAnchorB = newDropdown
                                , maybeThatAnchorB = newMaybeThatAnchor
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map DropdownAnchorBMsg dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just tool ->
                    case tool of
                        CreatePoint name pointData ->
                            updatePointDataWith (CreatePoint name) pointData

                        EditPoint thatPoint pointData ->
                            updatePointDataWith (EditPoint thatPoint) pointData

                        CreateLine name pointData ->
                            updateLineDataWith (CreateLine name) pointData

                        EditLine thatLine pointData ->
                            updateLineDataWith (EditLine thatLine) pointData

                        FromTo data ->
                            updateDataWith FromTo data

                        _ ->
                            ( model, Cmd.none, Nothing )

        DropdownLineAMsg dropdownMsg ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        CircleLine data ->
                            updateDataWith (toTool << CircleLine) data

                        LineLine data ->
                            updateDataWith (toTool << LineLine) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatLine ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.lines pattern)
                                dropdownMsg
                                data.dropdownLineA
                                data.maybeThatLineA

                        newData =
                            { data
                                | dropdownLineA = newDropdown
                                , maybeThatLineA = newMaybeThatLine
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map DropdownLineAMsg dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                Just (MirrorAt data) ->
                    updateDataWith MirrorAt data

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownLineBMsg dropdownMsg ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        LineLine data ->
                            updateDataWith (toTool << LineLine) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatLine ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.lines pattern)
                                dropdownMsg
                                data.dropdownLineB
                                data.maybeThatLineB

                        newData =
                            { data
                                | dropdownLineB = newDropdown
                                , maybeThatLineB = newMaybeThatLine
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map DropdownLineBMsg dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        ListboxPointsMsg listboxMsg ->
            let
                updateDataWith toTool data =
                    let
                        ( newListbox, listboxCmd, newPoints ) =
                            Listbox.update listboxUpdateConfig
                                (optionsWith Pattern.points pattern)
                                listboxMsg
                                data.listbox
                                (Those.toList data.thosePoints)

                        newData =
                            { data
                                | listbox = newListbox
                                , thosePoints = Those.fromList newPoints
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map ListboxPointsMsg listboxCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (MirrorAt data) ->
                    updateDataWith MirrorAt data

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownCircleAMsg dropdownMsg ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        CircleCircle data ->
                            updateDataWith (toTool << CircleCircle) data

                        CircleLine data ->
                            updateDataWith (toTool << CircleLine) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatCircle ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.circles pattern)
                                dropdownMsg
                                data.dropdownCircleA
                                data.maybeThatCircleA

                        newData =
                            { data
                                | dropdownCircleA = newDropdown
                                , maybeThatCircleA = newMaybeThatCircle
                            }
                    in
                    ( { model
                        | maybeTool = Just (toTool newData)
                      }
                    , Cmd.map DropdownCircleAMsg dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownCircleBMsg dropdownMsg ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        CircleCircle data ->
                            updateDataWith (toTool << CircleCircle) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatCircle ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.circles pattern)
                                dropdownMsg
                                data.dropdownCircleB
                                data.maybeThatCircleB

                        newData =
                            { data
                                | dropdownCircleB = newDropdown
                                , maybeThatCircleB = newMaybeThatCircle
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map DropdownCircleBMsg dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        FirstChanged newFirst ->
            let
                updatePointDataWith toTool pointData =
                    case pointData of
                        CircleCircle data ->
                            updateDataWith (toTool << CircleCircle) data

                        CircleLine data ->
                            updateDataWith (toTool << CircleLine) data

                        _ ->
                            ( model, Cmd.none, Nothing )

                updateDataWith toTool data =
                    ( { model | maybeTool = Just (toTool { data | first = newFirst }) }
                    , Cmd.none
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    updatePointDataWith (CreatePoint name) pointData

                Just (EditPoint thatPoint pointData) ->
                    updatePointDataWith (EditPoint thatPoint) pointData

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownPointMsg index dropdownMsg ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailOnePoint data ->
                            if index /= 0 then
                                ( model, Cmd.none, Nothing )

                            else
                                updateFirstPointWith (toTool << DetailOnePoint) data

                        DetailManyPoints data ->
                            if index < 0 || index > 1 + List.length data.otherPoints then
                                ( model, Cmd.none, Nothing )

                            else if index == 0 then
                                updateFirstPointWith (toTool << DetailManyPoints) data

                            else if index == 1 then
                                updateSecondPointWith (toTool << DetailManyPoints) data

                            else
                                case List.head (List.drop (index - 2) data.otherPoints) of
                                    Nothing ->
                                        ( model, Cmd.none, Nothing )

                                    Just { dropdown, maybeThat } ->
                                        updateIndexedPointWith toTool dropdown maybeThat data

                updateFirstPointWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatPoint ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.points pattern)
                                dropdownMsg
                                data.firstPointDropdown
                                data.firstPointMaybeThat

                        newData =
                            { data
                                | firstPointDropdown = newDropdown
                                , firstPointMaybeThat = newMaybeThatPoint
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map (DropdownPointMsg index) dropdownCmd
                    , Nothing
                    )

                updateSecondPointWith toTool data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatPoint ) =
                            Dropdown.update dropdownUpdateConfig
                                (optionsWith Pattern.points pattern)
                                dropdownMsg
                                data.secondPointDropdown
                                data.secondPointMaybeThat

                        newData =
                            { data
                                | secondPointDropdown = newDropdown
                                , secondPointMaybeThat = newMaybeThatPoint
                            }
                    in
                    ( { model | maybeTool = Just (toTool newData) }
                    , Cmd.map (DropdownPointMsg index) dropdownCmd
                    , Nothing
                    )

                updateIndexedPointWith toTool dropdown maybeThat data =
                    let
                        ( newDropdown, dropdownCmd, newMaybeThatPoint ) =
                            Dropdown.update dropdownUpdateConfig
                                (Pattern.points pattern
                                    |> List.map (Tuple.first >> Listbox.option)
                                )
                                dropdownMsg
                                dropdown
                                maybeThat

                        newData =
                            { data
                                | otherPoints =
                                    List.updateAt (index - 2)
                                        (\stuff ->
                                            { stuff
                                                | dropdown = newDropdown
                                                , maybeThat = newMaybeThatPoint
                                            }
                                        )
                                        data.otherPoints
                            }
                    in
                    ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                    , Cmd.map (DropdownPointMsg index) dropdownCmd
                    , Nothing
                    )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail name detailData) ->
                    updateDetailDataWith (EditDetail name) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        DropdownControlPointMsg index dropdownMsg ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailManyPoints data ->
                            if index < 0 || index > 1 + List.length data.otherPoints then
                                ( model, Cmd.none, Nothing )

                            else if index == 0 then
                                case data.connectionFirstSecond of
                                    ConnectionStraight ->
                                        ( model, Cmd.none, Nothing )

                                    ConnectionQuadratic { dropdown, maybeThat } ->
                                        let
                                            ( newDropdown, dropdownCmd, newMaybeThatPoint ) =
                                                Dropdown.update dropdownUpdateConfig
                                                    (Pattern.points pattern
                                                        |> List.map (Tuple.first >> Listbox.option)
                                                    )
                                                    dropdownMsg
                                                    dropdown
                                                    maybeThat

                                            newData =
                                                { data
                                                    | connectionFirstSecond =
                                                        ConnectionQuadratic
                                                            { dropdown = newDropdown
                                                            , maybeThat = newMaybeThatPoint
                                                            }
                                                }
                                        in
                                        ( { model
                                            | maybeTool =
                                                Just (toTool (DetailManyPoints newData))
                                          }
                                        , Cmd.map (DropdownControlPointMsg index) dropdownCmd
                                        , Nothing
                                        )

                            else if index == 1 + List.length data.otherPoints then
                                case data.connectionLastFirst of
                                    ConnectionStraight ->
                                        ( model, Cmd.none, Nothing )

                                    ConnectionQuadratic { dropdown, maybeThat } ->
                                        let
                                            ( newDropdown, dropdownCmd, newMaybeThatPoint ) =
                                                Dropdown.update dropdownUpdateConfig
                                                    (Pattern.points pattern
                                                        |> List.map (Tuple.first >> Listbox.option)
                                                    )
                                                    dropdownMsg
                                                    dropdown
                                                    maybeThat

                                            newData =
                                                { data
                                                    | connectionLastFirst =
                                                        ConnectionQuadratic
                                                            { dropdown = newDropdown
                                                            , maybeThat = newMaybeThatPoint
                                                            }
                                                }
                                        in
                                        ( { model
                                            | maybeTool =
                                                Just (toTool (DetailManyPoints newData))
                                          }
                                        , Cmd.map (DropdownControlPointMsg index) dropdownCmd
                                        , Nothing
                                        )

                            else
                                case List.head (List.drop (index - 1) data.otherPoints) of
                                    Nothing ->
                                        ( model, Cmd.none, Nothing )

                                    Just { connectionPrevious } ->
                                        case connectionPrevious of
                                            ConnectionStraight ->
                                                ( model, Cmd.none, Nothing )

                                            ConnectionQuadratic { dropdown, maybeThat } ->
                                                let
                                                    ( newDropdown, dropdownCmd, newMaybeThatPoint ) =
                                                        Dropdown.update dropdownUpdateConfig
                                                            (Pattern.points pattern
                                                                |> List.map (Tuple.first >> Listbox.option)
                                                            )
                                                            dropdownMsg
                                                            dropdown
                                                            maybeThat

                                                    newData =
                                                        { data
                                                            | otherPoints =
                                                                List.updateAt (index - 1)
                                                                    (\stuff ->
                                                                        { stuff
                                                                            | connectionPrevious =
                                                                                ConnectionQuadratic
                                                                                    { dropdown = newDropdown
                                                                                    , maybeThat = newMaybeThatPoint
                                                                                    }
                                                                        }
                                                                    )
                                                                    data.otherPoints
                                                        }
                                                in
                                                ( { model
                                                    | maybeTool =
                                                        Just (toTool (DetailManyPoints newData))
                                                  }
                                                , Cmd.map (DropdownControlPointMsg index) dropdownCmd
                                                , Nothing
                                                )

                        _ ->
                            ( model, Cmd.none, Nothing )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail thatDetail detailData) ->
                    updateDetailDataWith (EditDetail thatDetail) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        ActionMenuClicked index ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailManyPoints data ->
                            if index < 0 || index > 1 + List.length data.otherPoints then
                                ( model, Cmd.none, Nothing )

                            else if index == 0 then
                                let
                                    newData =
                                        { data
                                            | firstPointActionMenu =
                                                case data.firstPointActionMenu of
                                                    Closed ->
                                                        MoveDown

                                                    _ ->
                                                        Closed
                                        }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                            else if index == 1 then
                                let
                                    newData =
                                        { data
                                            | secondPointActionMenu =
                                                case data.secondPointActionMenu of
                                                    Closed ->
                                                        MoveDown

                                                    _ ->
                                                        Closed
                                        }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                            else
                                let
                                    newData =
                                        { data
                                            | otherPoints =
                                                List.updateAt (index - 2)
                                                    (\stuff ->
                                                        { stuff
                                                            | actionMenu =
                                                                case stuff.actionMenu of
                                                                    Closed ->
                                                                        MoveDown

                                                                    _ ->
                                                                        Closed
                                                        }
                                                    )
                                                    data.otherPoints
                                        }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                        _ ->
                            ( model, Cmd.none, Nothing )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail thatDetail detailData) ->
                    updateDetailDataWith (EditDetail thatDetail) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        ActionMenuLostFocus index ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailManyPoints data ->
                            if index < 0 || index > 1 + List.length data.otherPoints then
                                ( model, Cmd.none, Nothing )

                            else if index == 0 && not model.preventActionMenuClose then
                                let
                                    newData =
                                        { data | firstPointActionMenu = Closed }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                            else if index == 1 && not model.preventActionMenuClose then
                                let
                                    newData =
                                        { data | secondPointActionMenu = Closed }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                            else if not model.preventActionMenuClose then
                                let
                                    newData =
                                        { data
                                            | otherPoints =
                                                List.updateAt (index - 2)
                                                    (\stuff -> { stuff | actionMenu = Closed })
                                                    data.otherPoints
                                        }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                            else
                                ( model, Cmd.none, Nothing )

                        _ ->
                            ( model, Cmd.none, Nothing )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail thatDetail detailData) ->
                    updateDetailDataWith (EditDetail thatDetail) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        ActionMenuMouseDown _ ->
            ( { model | preventActionMenuClose = True }
            , Cmd.none
            , Nothing
            )

        ActionMenuMouseUp _ ->
            ( { model | preventActionMenuClose = False }
            , Cmd.none
            , Nothing
            )

        DetailRemovePointClicked index ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailManyPoints data ->
                            if index == 0 then
                                case List.head data.otherPoints of
                                    Nothing ->
                                        let
                                            newData =
                                                { firstPointDropdown = data.secondPointDropdown
                                                , firstPointMaybeThat = data.secondPointMaybeThat
                                                }
                                        in
                                        ( { model | maybeTool = Just (toTool (DetailOnePoint newData)) }
                                        , Cmd.none
                                        , Nothing
                                        )

                                    Just { dropdown, maybeThat, actionMenu } ->
                                        let
                                            newData =
                                                { data
                                                    | firstPointDropdown = data.secondPointDropdown
                                                    , firstPointMaybeThat = data.secondPointMaybeThat
                                                    , firstPointActionMenu = data.secondPointActionMenu
                                                    , connectionFirstSecond = ConnectionStraight
                                                    , secondPointDropdown = dropdown
                                                    , secondPointMaybeThat = maybeThat
                                                    , secondPointActionMenu = actionMenu
                                                    , otherPoints =
                                                        List.drop 1 data.otherPoints
                                                }
                                        in
                                        ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                        , Cmd.none
                                        , Nothing
                                        )

                            else if index == 1 then
                                case List.head data.otherPoints of
                                    Nothing ->
                                        let
                                            newData =
                                                { firstPointDropdown = data.firstPointDropdown
                                                , firstPointMaybeThat = data.firstPointMaybeThat
                                                }
                                        in
                                        ( { model | maybeTool = Just (toTool (DetailOnePoint newData)) }
                                        , Cmd.none
                                        , Nothing
                                        )

                                    Just { dropdown, maybeThat, actionMenu } ->
                                        let
                                            newData =
                                                { data
                                                    | connectionFirstSecond = ConnectionStraight
                                                    , secondPointDropdown = dropdown
                                                    , secondPointMaybeThat = maybeThat
                                                    , secondPointActionMenu = Closed
                                                    , otherPoints =
                                                        List.drop 1 data.otherPoints
                                                }
                                        in
                                        ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                        , Cmd.none
                                        , Nothing
                                        )

                            else if index > 1 + List.length data.otherPoints then
                                ( model, Cmd.none, Nothing )

                            else
                                let
                                    newData =
                                        { data
                                            | otherPoints =
                                                List.removeAt (index - 2) data.otherPoints
                                        }
                                in
                                ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                                , Cmd.none
                                , Nothing
                                )

                        _ ->
                            ( model, Cmd.none, Nothing )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail thatDetail detailData) ->
                    updateDetailDataWith (EditDetail thatDetail) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        DetailAddPointAtEnd ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailOnePoint data ->
                            let
                                newData =
                                    { firstPointDropdown = data.firstPointDropdown
                                    , firstPointMaybeThat = data.firstPointMaybeThat
                                    , firstPointActionMenu = Closed
                                    , connectionFirstSecond = ConnectionStraight
                                    , secondPointDropdown = Dropdown.init
                                    , secondPointMaybeThat = Nothing
                                    , secondPointActionMenu = Closed
                                    , otherPoints = []
                                    , connectionLastFirst = ConnectionStraight
                                    }
                            in
                            ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                            , Browser.Dom.focus "detail--connection-0"
                                |> Task.attempt (\_ -> NoOp)
                            , Nothing
                            )

                        DetailManyPoints data ->
                            let
                                newData =
                                    { data
                                        | otherPoints =
                                            data.otherPoints
                                                ++ [ { dropdown = Dropdown.init
                                                     , maybeThat = Nothing
                                                     , actionMenu = Closed
                                                     , connectionPrevious = ConnectionStraight
                                                     }
                                                   ]
                                    }
                            in
                            ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                            , Browser.Dom.focus
                                ("detail--connection-"
                                    ++ String.fromInt (1 + List.length data.otherPoints)
                                )
                                |> Task.attempt (\_ -> NoOp)
                            , Nothing
                            )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail thatDetail detailData) ->
                    updateDetailDataWith (EditDetail thatDetail) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        ConnectionChanged index newConnectionTag ->
            let
                updateDetailDataWith toTool detailData =
                    case detailData of
                        DetailManyPoints data ->
                            let
                                newData =
                                    if index < 0 || index > pointCount - 1 then
                                        data

                                    else if index == 0 then
                                        { data
                                            | connectionFirstSecond =
                                                case newConnectionTag of
                                                    ConnectionStraightTag ->
                                                        ConnectionStraight

                                                    ConnectionQuadraticTag ->
                                                        ConnectionQuadratic
                                                            { dropdown = Dropdown.init
                                                            , maybeThat = Nothing
                                                            }
                                        }

                                    else if index == pointCount - 1 then
                                        { data
                                            | connectionLastFirst =
                                                case newConnectionTag of
                                                    ConnectionStraightTag ->
                                                        ConnectionStraight

                                                    ConnectionQuadraticTag ->
                                                        ConnectionQuadratic
                                                            { dropdown = Dropdown.init
                                                            , maybeThat = Nothing
                                                            }
                                        }

                                    else
                                        { data
                                            | otherPoints =
                                                List.updateAt (index - 1)
                                                    (\stuff ->
                                                        { stuff
                                                            | connectionPrevious =
                                                                case newConnectionTag of
                                                                    ConnectionStraightTag ->
                                                                        ConnectionStraight

                                                                    ConnectionQuadraticTag ->
                                                                        ConnectionQuadratic
                                                                            { dropdown = Dropdown.init
                                                                            , maybeThat = Nothing
                                                                            }
                                                        }
                                                    )
                                                    data.otherPoints
                                        }

                                pointCount =
                                    2 + List.length data.otherPoints
                            in
                            ( { model | maybeTool = Just (toTool (DetailManyPoints newData)) }
                            , Cmd.none
                            , Nothing
                            )

                        _ ->
                            ( model, Cmd.none, Nothing )
            in
            case model.maybeTool of
                Just (CreateDetail name detailData) ->
                    updateDetailDataWith (CreateDetail name) detailData

                Just (EditDetail thatDetail detailData) ->
                    updateDetailDataWith (EditDetail thatDetail) detailData

                _ ->
                    ( model, Cmd.none, Nothing )

        --
        ToolDialogCancelClicked ->
            ( { model | maybeTool = Nothing }
            , case model.maybeTool of
                Nothing ->
                    Cmd.none

                Just tool ->
                    tool
                        |> toolToTag
                        |> toolTagToId
                        |> (\tag -> tag ++ "-button")
                        |> Browser.Dom.focus
                        |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        ValidationNameClicked ->
            ( model
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        CreateClicked ->
            let
                insertSimpleDistance constructor name data =
                    case data.maybeThatAnchorA of
                        Nothing ->
                            ( model, Cmd.none, Nothing )

                        Just thatAnchor ->
                            constructor pattern thatAnchor data.distance
                                |> Maybe.map (insertPoint name storedPattern model)
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )
            in
            case model.maybeTool of
                Just (CreatePoint name pointData) ->
                    if valueOf name == "" then
                        let
                            newName =
                                Invalid (valueOf name) "Enter a name"
                        in
                        ( { model | maybeTool = Just (CreatePoint newName pointData) }
                        , Browser.Dom.focus "validation-messages"
                            |> Task.attempt (\_ -> NoOp)
                        , Nothing
                        )

                    else
                        let
                            actualName =
                                valueOf name
                        in
                        case pointData of
                            LeftOf data ->
                                insertSimpleDistance Pattern.leftOf actualName data

                            RightOf data ->
                                insertSimpleDistance Pattern.rightOf actualName data

                            Above data ->
                                insertSimpleDistance Pattern.above actualName data

                            Below data ->
                                insertSimpleDistance Pattern.below actualName data

                            AtAngle data ->
                                case data.maybeThatAnchorA of
                                    Nothing ->
                                        ( model, Cmd.none, Nothing )

                                    Just thatAnchor ->
                                        Pattern.atAngle pattern
                                            thatAnchor
                                            data.angle
                                            data.distance
                                            |> Maybe.map (insertPoint actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )

                            BetweenRatio data ->
                                Maybe.map2
                                    (\thatAnchorA thatAnchorB ->
                                        Pattern.betweenRatio pattern
                                            thatAnchorA
                                            thatAnchorB
                                            data.ratio
                                            |> Maybe.map (insertPoint actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatAnchorA
                                    data.maybeThatAnchorB
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                            BetweenLength data ->
                                Maybe.map2
                                    (\thatAnchorA thatAnchorB ->
                                        Pattern.betweenLength pattern
                                            thatAnchorA
                                            thatAnchorB
                                            data.length
                                            |> Maybe.map (insertPoint actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatAnchorA
                                    data.maybeThatAnchorB
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                            CircleCircle data ->
                                Maybe.map2
                                    (\thatCircleA thatCircleB ->
                                        (if data.first then
                                            Pattern.firstCircleCircle pattern
                                                thatCircleA
                                                thatCircleB

                                         else
                                            Pattern.secondCircleCircle pattern
                                                thatCircleA
                                                thatCircleB
                                        )
                                            |> Maybe.map (insertPoint actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatCircleA
                                    data.maybeThatCircleB
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                            LineLine data ->
                                Maybe.map2
                                    (\thatLineA thatLineB ->
                                        Pattern.lineLine pattern thatLineA thatLineB
                                            |> Maybe.map (insertPoint actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatLineA
                                    data.maybeThatLineB
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                            CircleLine data ->
                                Maybe.map2
                                    (\thatCircle thatLine ->
                                        (if data.first then
                                            Pattern.firstCircleLine pattern
                                                thatCircle
                                                thatLine

                                         else
                                            Pattern.secondCircleLine pattern
                                                thatCircle
                                                thatLine
                                        )
                                            |> Maybe.map (insertPoint actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatCircleA
                                    data.maybeThatLineA
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (EditPoint _ _) ->
                    ( model, Cmd.none, Nothing )

                Just (CreateCircle name circleData) ->
                    if valueOf name == "" then
                        let
                            newName =
                                Invalid (valueOf name) "Enter a name"
                        in
                        ( { model | maybeTool = Just (CreateCircle newName circleData) }
                        , Browser.Dom.focus "validation-messages"
                            |> Task.attempt (\_ -> NoOp)
                        , Nothing
                        )

                    else
                        let
                            actualName =
                                valueOf name
                        in
                        case circleData of
                            CenteredAt data ->
                                case data.maybeThatAnchorA of
                                    Nothing ->
                                        ( model, Cmd.none, Nothing )

                                    Just thatAnchor ->
                                        Pattern.centeredAt pattern thatAnchor data.radius
                                            |> Maybe.map (insertCircle actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (EditCircle _ _) ->
                    ( model, Cmd.none, Nothing )

                Just (CreateLine name lineData) ->
                    if valueOf name == "" then
                        let
                            newName =
                                Invalid (valueOf name) "Enter a name"
                        in
                        ( { model | maybeTool = Just (CreateLine newName lineData) }
                        , Browser.Dom.focus "validation-messages"
                            |> Task.attempt (\_ -> NoOp)
                        , Nothing
                        )

                    else
                        let
                            actualName =
                                valueOf name
                        in
                        case lineData of
                            ThroughTwoPoints data ->
                                Maybe.map2
                                    (\thatAnchorA thatAnchorB ->
                                        Pattern.throughTwoPoints pattern thatAnchorA thatAnchorB
                                            |> Maybe.map (insertLine actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatAnchorA
                                    data.maybeThatAnchorB
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                            ThroughOnePoint data ->
                                Maybe.map
                                    (\thatAnchorA ->
                                        Pattern.throughOnePoint pattern thatAnchorA data.angle
                                            |> Maybe.map (insertLine actualName storedPattern model)
                                            |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                    )
                                    data.maybeThatAnchorA
                                    |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (EditLine _ _) ->
                    ( model, Cmd.none, Nothing )

                Just (FromTo data) ->
                    case ( data.maybeThatAnchorA, data.maybeThatAnchorB ) of
                        ( Just thatAnchorA, Just thatAnchorB ) ->
                            let
                                newLineSegment =
                                    Pattern.FromTo
                                        thatAnchorA
                                        thatAnchorB

                                ( newPattern, _ ) =
                                    Pattern.insertLineSegment
                                        (if data.name == "" then
                                            Nothing

                                         else
                                            Just data.name
                                        )
                                        newLineSegment
                                        pattern
                            in
                            ( { model | maybeTool = Nothing }
                            , Cmd.none
                            , Just { storedPattern | pattern = newPattern }
                            )

                        _ ->
                            ( model, Cmd.none, Nothing )

                Just (MirrorAt { maybeThatLineA, thosePoints }) ->
                    case maybeThatLineA of
                        Just thatLine ->
                            let
                                newTransformation =
                                    Pattern.MirrorAt thatLine

                                newPoints =
                                    thosePoints
                                        |> Those.toList
                                        |> List.map
                                            (Pattern.TransformBy thatTransformation)

                                ( newPattern, thatTransformation ) =
                                    Pattern.insertTransformation newTransformation pattern

                                insertPointHelp nextPattern point =
                                    Pattern.insertPoint Nothing nextPattern point
                                        |> Tuple.first
                            in
                            ( { model | maybeTool = Nothing }
                            , Cmd.none
                            , Just
                                { storedPattern
                                    | pattern =
                                        List.foldl insertPointHelp newPattern newPoints
                                }
                            )

                        _ ->
                            ( model, Cmd.none, Nothing )

                Just (CreateDetail name (DetailManyPoints data)) ->
                    if valueOf name == "" then
                        let
                            newName =
                                Invalid (valueOf name) "Enter a name"
                        in
                        ( { model
                            | maybeTool =
                                Just (CreateDetail newName (DetailManyPoints data))
                          }
                        , Browser.Dom.focus "validation-messages"
                            |> Task.attempt (\_ -> NoOp)
                        , Nothing
                        )

                    else
                        let
                            actualName =
                                valueOf name

                            extract otherPoints =
                                List.foldr
                                    (\{ maybeThat, connectionPrevious } result ->
                                        case result of
                                            Just sum ->
                                                case maybeThat of
                                                    Nothing ->
                                                        Nothing

                                                    Just that ->
                                                        case connectionPrevious of
                                                            ConnectionStraight ->
                                                                Just
                                                                    (( that
                                                                     , Pattern.Straight
                                                                     )
                                                                        :: sum
                                                                    )

                                                            ConnectionQuadratic quadraticData ->
                                                                case quadraticData.maybeThat of
                                                                    Nothing ->
                                                                        Nothing

                                                                    Just controlThat ->
                                                                        Just
                                                                            (( that
                                                                             , Pattern.Quadratic controlThat
                                                                             )
                                                                                :: sum
                                                                            )

                                            Nothing ->
                                                Nothing
                                    )
                                    (Just [])
                                    otherPoints

                            connectionHelp connection =
                                case connection of
                                    ConnectionStraight ->
                                        Just Pattern.Straight

                                    ConnectionQuadratic { dropdown, maybeThat } ->
                                        Maybe.map Pattern.Quadratic maybeThat
                        in
                        Maybe.map5
                            (\firstPointThat secondPointThat connectionFirstSecond otherPoints connectionLastFirst ->
                                let
                                    newDetail =
                                        Pattern.FromPoints
                                            { firstPoint = firstPointThat
                                            , segments =
                                                ( secondPointThat
                                                , connectionFirstSecond
                                                )
                                                    :: otherPoints
                                            , lastToFirst = connectionLastFirst
                                            }

                                    ( newPattern, _ ) =
                                        Pattern.insertDetail
                                            (Just actualName)
                                            newDetail
                                            pattern
                                in
                                ( { model | maybeTool = Nothing }
                                , Cmd.none
                                , Just { storedPattern | pattern = newPattern }
                                )
                            )
                            data.firstPointMaybeThat
                            data.secondPointMaybeThat
                            (connectionHelp data.connectionFirstSecond)
                            (extract data.otherPoints)
                            (connectionHelp data.connectionLastFirst)
                            |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (CreateDetail _ (DetailOnePoint _)) ->
                    ( model, Cmd.none, Nothing )

                Just (EditDetail _ _) ->
                    ( model, Cmd.none, Nothing )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        UpdateClicked ->
            case model.maybeTool of
                Just (EditPoint thatPoint pointData) ->
                    case pointData of
                        LeftOf data ->
                            case data.maybeThatAnchorA of
                                Nothing ->
                                    ( model, Cmd.none, Nothing )

                                Just thatAnchor ->
                                    Pattern.leftOf pattern thatAnchor data.distance
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        RightOf data ->
                            case data.maybeThatAnchorA of
                                Nothing ->
                                    ( model, Cmd.none, Nothing )

                                Just thatAnchor ->
                                    Pattern.rightOf pattern thatAnchor data.distance
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        Above data ->
                            case data.maybeThatAnchorA of
                                Nothing ->
                                    ( model, Cmd.none, Nothing )

                                Just thatAnchor ->
                                    Pattern.above pattern thatAnchor data.distance
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        Below data ->
                            case data.maybeThatAnchorA of
                                Nothing ->
                                    ( model, Cmd.none, Nothing )

                                Just thatAnchor ->
                                    Pattern.below pattern thatAnchor data.distance
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        AtAngle data ->
                            case data.maybeThatAnchorA of
                                Nothing ->
                                    ( model, Cmd.none, Nothing )

                                Just thatAnchor ->
                                    Pattern.atAngle pattern
                                        thatAnchor
                                        data.angle
                                        data.distance
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        BetweenRatio data ->
                            Maybe.map2
                                (\thatAnchorA thatAnchorB ->
                                    Pattern.betweenRatio pattern
                                        thatAnchorA
                                        thatAnchorB
                                        data.ratio
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatAnchorA
                                data.maybeThatAnchorB
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        BetweenLength data ->
                            Maybe.map2
                                (\thatAnchorA thatAnchorB ->
                                    Pattern.betweenLength pattern
                                        thatAnchorA
                                        thatAnchorB
                                        data.length
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatAnchorA
                                data.maybeThatAnchorB
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        CircleCircle data ->
                            Maybe.map2
                                (\thatCircleA thatCircleB ->
                                    (if data.first then
                                        Pattern.firstCircleCircle pattern
                                            thatCircleA
                                            thatCircleB

                                     else
                                        Pattern.secondCircleCircle pattern
                                            thatCircleA
                                            thatCircleB
                                    )
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatCircleA
                                data.maybeThatCircleB
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        LineLine data ->
                            Maybe.map2
                                (\thatLineA thatLineB ->
                                    Pattern.lineLine pattern thatLineA thatLineB
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatLineA
                                data.maybeThatLineB
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        CircleLine data ->
                            Maybe.map2
                                (\thatCircle thatLine ->
                                    (if data.first then
                                        Pattern.firstCircleLine pattern
                                            thatCircle
                                            thatLine

                                     else
                                        Pattern.secondCircleLine pattern
                                            thatCircle
                                            thatLine
                                    )
                                        |> Maybe.map (updatePoint thatPoint storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatCircleA
                                data.maybeThatLineA
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (EditCircle thatCircle circleData) ->
                    case circleData of
                        CenteredAt data ->
                            case data.maybeThatAnchorA of
                                Nothing ->
                                    ( model, Cmd.none, Nothing )

                                Just thatAnchor ->
                                    Pattern.centeredAt pattern thatAnchor data.radius
                                        |> Maybe.map (updateCircle thatCircle storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (EditLine thatLine lineData) ->
                    case lineData of
                        ThroughTwoPoints data ->
                            Maybe.map2
                                (\thatAnchorA thatAnchorB ->
                                    Pattern.throughTwoPoints pattern thatAnchorA thatAnchorB
                                        |> Maybe.map (updateLine thatLine storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatAnchorA
                                data.maybeThatAnchorB
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                        ThroughOnePoint data ->
                            Maybe.map
                                (\thatAnchorA ->
                                    Pattern.throughOnePoint pattern thatAnchorA data.angle
                                        |> Maybe.map (updateLine thatLine storedPattern model)
                                        |> Maybe.withDefault ( model, Cmd.none, Nothing )
                                )
                                data.maybeThatAnchorA
                                |> Maybe.withDefault ( model, Cmd.none, Nothing )

                Just (EditDetail thatDetail (DetailManyPoints data)) ->
                    let
                        extract otherPoints =
                            List.foldr
                                (\{ maybeThat, connectionPrevious } result ->
                                    case result of
                                        Just sum ->
                                            case maybeThat of
                                                Nothing ->
                                                    Nothing

                                                Just that ->
                                                    case connectionPrevious of
                                                        ConnectionStraight ->
                                                            Just
                                                                (( that
                                                                 , Pattern.Straight
                                                                 )
                                                                    :: sum
                                                                )

                                                        ConnectionQuadratic quadraticData ->
                                                            case quadraticData.maybeThat of
                                                                Nothing ->
                                                                    Nothing

                                                                Just controlThat ->
                                                                    Just
                                                                        (( that
                                                                         , Pattern.Quadratic controlThat
                                                                         )
                                                                            :: sum
                                                                        )

                                        Nothing ->
                                            Nothing
                                )
                                (Just [])
                                otherPoints

                        connectionHelp connection =
                            case connection of
                                ConnectionStraight ->
                                    Just Pattern.Straight

                                ConnectionQuadratic { dropdown, maybeThat } ->
                                    Maybe.map Pattern.Quadratic maybeThat
                    in
                    Maybe.map5
                        (\firstPointThat secondPointThat connectionFirstSecond otherPoints connectionLastFirst ->
                            let
                                newDetail =
                                    Pattern.FromPoints
                                        { firstPoint = firstPointThat
                                        , segments =
                                            ( secondPointThat
                                            , connectionFirstSecond
                                            )
                                                :: otherPoints
                                        , lastToFirst = connectionLastFirst
                                        }

                                newPattern =
                                    Pattern.updateDetail
                                        thatDetail
                                        newDetail
                                        pattern
                            in
                            ( { model | maybeTool = Nothing }
                            , Cmd.none
                            , Just { storedPattern | pattern = newPattern }
                            )
                        )
                        data.firstPointMaybeThat
                        data.secondPointMaybeThat
                        (connectionHelp data.connectionFirstSecond)
                        (extract data.otherPoints)
                        (connectionHelp data.connectionLastFirst)
                        |> Maybe.withDefault ( model, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        -- PATTERN
        PointHovered newHoveredPoint ->
            ( { model | hoveredPoint = newHoveredPoint }
            , Cmd.none
            , Nothing
            )

        ToolbarToggleClicked ->
            ( { model
                | rightToolbarVisible = not model.rightToolbarVisible
                , patternContainerDimensions = Nothing
              }
            , Browser.Dom.getViewportOf "pattern-container"
                |> Task.attempt PatternContainerViewportReceived
            , Nothing
            )

        VariablesRulerClicked ->
            ( { model | variablesVisible = not model.variablesVisible }
            , Cmd.none
            , Nothing
            )

        VariableCreateClicked ->
            ( { model
                | maybeVariableDialog =
                    Just <|
                        VariableDialogCreate { name = "", value = "" }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        VariableNameChanged newName ->
            case model.maybeVariableDialog of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just (VariableDialogCreate data) ->
                    ( { model
                        | maybeVariableDialog =
                            Just <|
                                VariableDialogCreate { data | name = newName }
                      }
                    , Cmd.none
                    , Nothing
                    )

        VariableValueChanged newValue ->
            case model.maybeVariableDialog of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just (VariableDialogCreate data) ->
                    ( { model
                        | maybeVariableDialog =
                            Just <|
                                VariableDialogCreate { data | value = newValue }
                      }
                    , Cmd.none
                    , Nothing
                    )

        VariableCreateSubmitClicked ->
            case model.maybeVariableDialog of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just (VariableDialogCreate { name, value }) ->
                    let
                        newPattern =
                            Pattern.insertVariable name value pattern
                    in
                    ( { model | maybeVariableDialog = Nothing }
                    , Cmd.none
                    , Just { storedPattern | pattern = newPattern }
                    )

        VariableDialogCancelClicked ->
            ( { model | maybeVariableDialog = Nothing }
            , Cmd.none
            , Nothing
            )

        PointsRulerClicked ->
            ( { model | pointsVisible = not model.pointsVisible }
            , Cmd.none
            , Nothing
            )

        CirclesRulerClicked ->
            ( { model | circlesVisible = not model.circlesVisible }
            , Cmd.none
            , Nothing
            )

        CircleEditClicked thatCircle ->
            case Pattern.getCircle pattern thatCircle of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just { name, value } ->
                    let
                        editCircle toCircleData =
                            EditCircle thatCircle << toCircleData

                        tool =
                            case value of
                                Pattern.CenteredAt thatAnchor radius ->
                                    editCircle CenteredAt
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchor
                                        , radius = radius
                                        }
                    in
                    ( { model | maybeTool = Just tool }
                    , Cmd.none
                    , Nothing
                    )

        LinesRulerClicked ->
            ( { model | linesVisible = not model.linesVisible }
            , Cmd.none
            , Nothing
            )

        LineEditClicked thatLine ->
            case Pattern.getLine pattern thatLine of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just { name, value } ->
                    let
                        editLine toLineData =
                            EditLine thatLine << toLineData

                        tool =
                            case value of
                                Pattern.ThroughTwoPoints thatAnchorA thatAnchorB ->
                                    editLine ThroughTwoPoints
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchorA
                                        , dropdownAnchorB = Dropdown.init
                                        , maybeThatAnchorB = Just thatAnchorB
                                        }

                                Pattern.ThroughOnePoint thatAnchorA angle ->
                                    editLine ThroughOnePoint
                                        { dropdownAnchorA = Dropdown.init
                                        , maybeThatAnchorA = Just thatAnchorA
                                        , angle = angle
                                        }
                    in
                    ( { model | maybeTool = Just tool }
                    , Cmd.none
                    , Nothing
                    )

        LineSegmentsRulerClicked ->
            ( { model | lineSegmentsVisible = not model.lineSegmentsVisible }
            , Cmd.none
            , Nothing
            )

        DetailsRulerClicked ->
            ( { model | detailsVisible = not model.detailsVisible }
            , Cmd.none
            , Nothing
            )

        DetailEditClicked thatDetail ->
            case Pattern.getDetail pattern thatDetail of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just { name, value } ->
                    let
                        editDetail toDetailData =
                            Just << EditDetail thatDetail << toDetailData

                        toConnection connection =
                            case connection of
                                Pattern.Straight ->
                                    ConnectionStraight

                                Pattern.Quadratic thatPoint ->
                                    ConnectionQuadratic
                                        { dropdown = Dropdown.init
                                        , maybeThat = Just thatPoint
                                        }

                        maybeTool =
                            case value of
                                Pattern.FromPoints data ->
                                    case data.segments of
                                        [] ->
                                            Nothing

                                        second :: rest ->
                                            editDetail DetailManyPoints
                                                { firstPointDropdown = Dropdown.init
                                                , firstPointMaybeThat = Just data.firstPoint
                                                , firstPointActionMenu = Closed
                                                , secondPointDropdown = Dropdown.init
                                                , secondPointMaybeThat = Just (Tuple.first second)
                                                , secondPointActionMenu = Closed
                                                , connectionFirstSecond =
                                                    toConnection (Tuple.second second)
                                                , otherPoints =
                                                    List.map
                                                        (\segment ->
                                                            { dropdown = Dropdown.init
                                                            , maybeThat = Just (Tuple.first segment)
                                                            , actionMenu = Closed
                                                            , connectionPrevious =
                                                                toConnection (Tuple.second segment)
                                                            }
                                                        )
                                                        rest
                                                , connectionLastFirst =
                                                    toConnection data.lastToFirst
                                                }
                    in
                    ( { model | maybeTool = maybeTool }
                    , Cmd.none
                    , Nothing
                    )

        DetailRemoveClicked thatDetail ->
            ( { model | maybeModal = Just (DetailDeleteConfirm thatDetail) }
            , Cmd.none
            , Nothing
            )

        DetailRemoveDialogDeleteClicked ->
            case model.maybeModal of
                Nothing ->
                    ( model, Cmd.none, Nothing )

                Just (DetailDeleteConfirm thatDetail) ->
                    let
                        newPattern =
                            Pattern.deleteDetail thatDetail pattern
                    in
                    ( { model | maybeModal = Nothing }
                    , Cmd.none
                    , Just { storedPattern | pattern = newPattern }
                    )

        DetailRemoveDialogCancelClicked ->
            ( { model | maybeModal = Nothing }
            , Cmd.none
            , Nothing
            )


optionsWith objects pattern =
    pattern
        |> objects
        |> List.map (Tuple.first >> Listbox.option)


insertPoint name viewedPattern model newPoint =
    let
        ( newPattern, _ ) =
            Pattern.insertPoint
                (if name == "" then
                    Nothing

                 else
                    Just name
                )
                newPoint
                viewedPattern.pattern
    in
    ( { model | maybeTool = Nothing }
    , Cmd.none
    , Just { viewedPattern | pattern = newPattern }
    )


updatePoint thatPoint viewedPattern model newPoint =
    let
        newPattern =
            Pattern.updatePoint thatPoint newPoint viewedPattern.pattern
    in
    ( { model | maybeTool = Nothing }
    , Cmd.none
    , Just { viewedPattern | pattern = newPattern }
    )


insertCircle name viewedPattern model newCircle =
    let
        ( newPattern, _ ) =
            Pattern.insertCircle
                (if name == "" then
                    Nothing

                 else
                    Just name
                )
                newCircle
                viewedPattern.pattern
    in
    ( { model | maybeTool = Nothing }
    , Cmd.none
    , Just { viewedPattern | pattern = newPattern }
    )


updateCircle thatCircle viewedPattern model newCircle =
    let
        newPattern =
            Pattern.updateCircle thatCircle newCircle viewedPattern.pattern
    in
    ( { model | maybeTool = Nothing }
    , Cmd.none
    , Just { viewedPattern | pattern = newPattern }
    )


insertLine name viewedPattern model newLine =
    let
        ( newPattern, _ ) =
            Pattern.insertLine
                (if name == "" then
                    Nothing

                 else
                    Just name
                )
                newLine
                viewedPattern.pattern
    in
    ( { model | maybeTool = Nothing }
    , Cmd.none
    , Just { viewedPattern | pattern = newPattern }
    )


updateLine thatLine viewedPattern model newLine =
    let
        newPattern =
            Pattern.updateLine thatLine newLine viewedPattern.pattern
    in
    ( { model | maybeTool = Nothing }
    , Cmd.none
    , Just { viewedPattern | pattern = newPattern }
    )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.patternContainerDimensions of
            Just _ ->
                Sub.none

            Nothing ->
                Browser.Events.onAnimationFrame
                    (\_ -> PatternContainerViewportRequested)
        , Browser.Events.onResize
            (\_ _ -> WindowResized)
        , case model.maybeDrag of
            Nothing ->
                Sub.none

            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove <|
                        Decode.map MouseMove <|
                            Decode.map2 Position
                                (Decode.field "screenX" Decode.float)
                                (Decode.field "screenY" Decode.float)
                    , Browser.Events.onMouseUp <|
                        Decode.map MouseUp <|
                            Decode.map2 Position
                                (Decode.field "screenX" Decode.float)
                                (Decode.field "screenY" Decode.float)
                    ]
        ]



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
