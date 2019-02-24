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
import Api
import Array
import BoundingBox2d
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Circle2d
import Color
import Design
import Dialog
import Draw.Pattern as Pattern exposing (HoveredObject)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Element
import Element.Region as Region
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import LineSegment2d
import List.Extra as List
import Listbox exposing (Listbox)
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Pattern exposing (A, Axis, Circle, Curve, Detail, InsertHelp(..), Pattern, Point)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Process
import State
import Store exposing (Entry)
import StoredPattern exposing (StoredPattern)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Task
import Triple
import Url exposing (Url)
import Vector2d
import View.Icon
import View.Input
import View.Modal
import View.Navigation
import View.Table
import VoronoiDiagram2d



---- MODEL


type Model
    = Loading
    | Error
    | Loaded LoadedData


init : String -> ( Model, Cmd Msg )
init slug =
    ( Loading
    , Api.getPattern PatternReceived slug
    )


type alias LoadedData =
    { maybeDrag : Maybe Drag
    , patternContainerDimensions : Maybe Dimensions
    , maybeModal : Maybe Modal

    -- PATTERN
    , storedPattern : StoredPattern
    , hoveredObject : Maybe HoveredObject

    -- LEFT TOOLBAR
    , maybeDialog : Maybe Dialog
    , preventActionMenuClose : Bool

    -- RIGHT TOOLBAR
    , rightToolbarVisible : Bool
    , variablesVisible : Bool
    , maybeVariableDialog : Maybe VariableDialog
    , pointsVisible : Bool
    , axesVisible : Bool
    , curvesVisible : Bool
    , circlesVisible : Bool
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


type Dialog
    = CreateObject Dialog.Create
    | EditObject String Dialog.Edit


type Modal
    = PointDeleteConfirm (A Point)
    | AxisDeleteConfirm (A Axis)
    | CircleDeleteConfirm (A Circle)
    | CurveDeleteConfirm (A Curve)
    | DetailDeleteConfirm (A Detail)


type VariableDialog
    = VariableDialogCreate
        { name : String
        , nameHelp : Maybe String
        , value : String
        }



---- VIEW


view : String -> Model -> Document Msg
view prefix model =
    case model of
        Loading ->
            { title = "Loading pattern.."
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
                    ]
                    (Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Loading pattern..")
                    )
                ]
            }

        Error ->
            { title = "Something went wrong."
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
                    ]
                    (Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Loading pattern..")
                    )
                ]
            }

        Loaded data ->
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
                        case data.maybeModal of
                            Nothing ->
                                Element.none

                            Just modal ->
                                viewModal data.storedPattern.pattern modal
                    ]
                    (viewEditor prefix data.storedPattern data)
                ]
            }


viewModal : Pattern -> Modal -> Element Msg
viewModal pattern modal =
    case modal of
        PointDeleteConfirm aPoint ->
            let
                dependentObjects =
                    Pattern.objectsDependingOnPoint pattern aPoint

                viewDependentObjects =
                    [ Element.paragraph []
                        [ Element.el [ Font.bold ]
                            (Element.text "Note:")
                        , Element.text " The following objects depend on this point and will therefore be "
                        , Element.el [ Font.bold ]
                            (Element.text "removed, as well")
                        , Element.text ":"
                        ]
                    , if List.isEmpty dependentObjects.points then
                        Element.none

                      else
                        Element.paragraph
                            [ Element.paddingEach
                                { top = 0
                                , bottom = 0
                                , left = Design.small
                                , right = 0
                                }
                            ]
                            (List.concat
                                [ [ Element.text "The points " ]
                                , dependentObjects.points
                                    |> List.map
                                        (\aDependentPoint ->
                                            Element.el [ Font.bold ] <|
                                                Element.text
                                                    ("«"
                                                        ++ objectName aDependentPoint
                                                        ++ "»"
                                                    )
                                        )
                                    |> List.intersperse (Element.text ", ")
                                , [ Element.text "." ]
                                ]
                            )
                    ]
            in
            View.Modal.small
                { onCancelPress = ModalCancelPressed
                , title = "Delete «" ++ objectName aPoint ++ "»?"
                , content =
                    Element.column
                        [ Element.spacing Design.small
                        , Element.htmlAttribute (Html.Attributes.id "dialog--body")
                        , Element.width Element.fill
                        , Element.padding Design.small
                        , Background.color Design.white
                        ]
                        (Element.paragraph []
                            [ Element.text "Do you want to remove the point "
                            , Element.el [ Font.bold ]
                                (Element.text ("«" ++ objectName aPoint ++ "»"))
                            , Element.text "?"
                            ]
                            :: (if
                                    List.isEmpty dependentObjects.points
                                        && List.isEmpty dependentObjects.axes
                                        && List.isEmpty dependentObjects.circles
                                        && List.isEmpty dependentObjects.curves
                                        && List.isEmpty dependentObjects.details
                                then
                                    []

                                else
                                    viewDependentObjects
                               )
                        )
                , actions =
                    [ View.Input.btnDanger
                        { onPress = Just PointDeleteModalDeletePressed
                        , label = "Delete point"
                        }
                    , Element.el [ Element.alignRight ] <|
                        View.Input.btnCancel
                            { onPress = Just ModalCancelPressed
                            , label = "Cancel"
                            }
                    ]
                }

        AxisDeleteConfirm aAxis ->
            viewDeleteModal
                { name = objectName aAxis
                , kind = "axis"
                , onDeletePress = AxisDeleteModalDeletePressed
                }

        CircleDeleteConfirm aCircle ->
            viewDeleteModal
                { name = objectName aCircle
                , kind = "circle"
                , onDeletePress = CircleDeleteModalDeletePressed
                }

        CurveDeleteConfirm aCurve ->
            viewDeleteModal
                { name = objectName aCurve
                , kind = "curve"
                , onDeletePress = CurveDeleteModalDeletePressed
                }

        DetailDeleteConfirm aDetail ->
            viewDeleteModal
                { name = objectName aDetail
                , kind = "detail"
                , onDeletePress = DetailDeleteModalDeletePressed
                }


viewDeleteModal { name, kind, onDeletePress } =
    View.Modal.small
        { onCancelPress = ModalCancelPressed
        , title = "Delete «" ++ name ++ "»?"
        , content =
            Element.paragraph
                [ Element.htmlAttribute (Html.Attributes.id "dialog--body")
                , Element.width Element.fill
                , Element.padding Design.small
                , Background.color Design.white
                ]
                [ Element.text ("Do you want to remove the " ++ kind ++ " ")
                , Element.el [ Font.bold ]
                    (Element.text ("«" ++ name ++ "»"))
                , Element.text "?"
                ]
        , actions =
            [ View.Input.btnDanger
                { onPress = Just onDeletePress
                , label = "Delete " ++ kind
                }
            , Element.el [ Element.alignRight ] <|
                View.Input.btnCancel
                    { onPress = Just ModalCancelPressed
                    , label = "Cancel"
                    }
            ]
        }


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
            , Font.size Design.small
            , Background.color Design.secondary
            , Font.color Design.black
            ]
            [ View.Navigation.link
                { url = "/"
                , label = "Patterns"
                }
            , Element.el [] (View.Icon.fa "angle-right")
            , Element.el [] (Element.text name)
            , Element.newTabLink
                [ Element.alignRight
                , Font.color Design.black
                , Font.size Design.small
                , Element.mouseOver
                    [ Font.color Design.primaryDark ]
                ]
                { url = "https://github.com/kirchner/sewing-pattern-editor"
                , label = View.Icon.faBrandLarge "github"
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
            [ Element.lazy3 viewLeftToolbar
                prefix
                pattern
                model.maybeDialog
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


viewLeftToolbar : String -> Pattern -> Maybe Dialog -> Element Msg
viewLeftToolbar prefix pattern maybeDialog =
    Element.column
        [ Element.width (Element.maximum 400 Element.fill)
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color Design.white
        ]
        [ case maybeDialog of
            Nothing ->
                Element.lazy viewToolSelector prefix

            Just (CreateObject dialog) ->
                Element.map DialogCreateMsg <|
                    Dialog.createView
                        { pattern = pattern
                        , hoveredInCanvas = Nothing
                        }
                        dialog

            Just (EditObject name dialog) ->
                Element.map DialogEditMsg <|
                    Dialog.editView
                        { pattern = pattern
                        , name = name
                        , hoveredInCanvas = Nothing
                        }
                        dialog
        ]



---- WORKSPACE


viewWorkspace : StoredPattern -> LoadedData -> Element Msg
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


viewPattern : Maybe Dimensions -> Maybe Drag -> StoredPattern -> LoadedData -> Html Msg
viewPattern maybeDimensions maybeDrag storedPattern model =
    let
        { pattern, center, zoom } =
            storedPattern
    in
    case maybeDimensions of
        Nothing ->
            Html.text ""

        Just { width, height } ->
            let
                currentCenter =
                    case maybeDrag of
                        Nothing ->
                            center

                        Just drag ->
                            center
                                |> Point2d.translateBy
                                    (Vector2d.fromComponents
                                        ( drag.start.x - drag.current.x
                                        , drag.start.y - drag.current.y
                                        )
                                    )
            in
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
                [ Svg.translateBy (Vector2d.from currentCenter Point2d.origin) <|
                    State.finalValue pattern
                        (Pattern.draw
                            { preview = False
                            , zoom = zoom
                            , objectHovered = ObjectHovered
                            , hoveredObject = model.hoveredObject
                            }
                        )
                ]



---- ZOOM


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


viewRightToolbar pattern model =
    Element.row
        [ Element.height Element.fill
        , Background.color Design.white
        ]
        [ Input.button
            [ Element.height Element.fill
            , Element.padding 5
            , Font.color Design.black
            , Element.mouseOver
                [ Font.color Design.primaryDark
                , Border.color Design.primaryDark
                , Background.color Design.secondary
                ]
            ]
            { onPress = Just ToolbarToggleClicked
            , label =
                Element.column
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.spacing Design.xSmall
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
                , Element.padding Design.xSmall
                , Element.spacing Design.xSmall
                , Element.scrollbarY
                ]
                (case model.maybeVariableDialog of
                    Nothing ->
                        [ Element.lazy2 viewVariables pattern model.variablesVisible
                        , Element.lazy2 viewPoints pattern model.pointsVisible
                        , Element.lazy2 viewAxes pattern model.axesVisible
                        , Element.lazy2 viewCurves pattern model.curvesVisible
                        , Element.lazy2 viewCircles pattern model.circlesVisible
                        , Element.lazy2 viewDetails pattern model.detailsVisible
                        ]

                    Just (VariableDialogCreate stuff) ->
                        [ viewVariable stuff.name stuff.value ]
                )

          else
            Element.none
        ]



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
            , Font.color Design.black
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
                , help = Nothing
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
        button msg id label =
            View.Input.btnCallToAction (id ++ "-button")
                { onPress = Just msg
                , label = label
                }
    in
    Element.column
        [ Element.padding Design.small
        , Element.spacing Design.small
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.spacing Design.xSmall
            , Element.width Element.fill
            ]
            [ Element.column
                [ Element.spacing Design.xxSmall
                , Element.width Element.fill
                ]
                [ button CreatePointPressed "create-point" "Create a point"
                , button CreateAxisPressed "create-axis" "Create an axis"
                , button CreateCirclePressed "create-circle" "Create a circle"
                , button CreateCurvePressed "create-curve" "Create a curve"
                , button CreateDetailPressed "create-detail" "Create a detail"
                ]
            ]
        ]



-- TABLES


viewVariables : Pattern -> Bool -> Element Msg
viewVariables pattern variablesVisible =
    View.Navigation.accordion
        { onPress = VariablesRulerClicked
        , label = "Variables"
        , open = variablesVisible
        , content =
            Element.column
                [ Element.width Element.fill
                , Element.spacing Design.small
                ]
                [ Element.table
                    [ Element.spacing Design.xSmall ]
                    { data =
                        List.sortBy .name
                            (Pattern.variables pattern
                                |> List.filterMap
                                    (\variable ->
                                        case
                                            ( Pattern.variableInfo variable pattern
                                            , State.finalValue pattern <|
                                                Pattern.float variable
                                            )
                                        of
                                            ( Just rawExpr, Ok value ) ->
                                                Just
                                                    { name = variable
                                                    , rawExpr = rawExpr
                                                    , value = value
                                                    }

                                            _ ->
                                                Nothing
                                    )
                            )
                    , columns =
                        [ View.Table.column
                            { label = "Name"
                            , recordToString = .name
                            }
                        , View.Table.columnFloat
                            { label = "Value"
                            , recordToFloat = Just << .value
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


viewPoints : Pattern -> Bool -> Element Msg
viewPoints pattern pointsVisible =
    View.Navigation.accordion
        { onPress = PointsRulerClicked
        , label = "Points"
        , open = pointsVisible
        , content =
            View.Table.table
                { data = List.sortBy objectName (Pattern.points pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            Pattern.name
                                >> Maybe.withDefault "<no name>"
                        }
                    , View.Table.columnFloat
                        { label = "x"
                        , recordToFloat =
                            Pattern.point2d
                                >> State.finalValue pattern
                                >> Result.toMaybe
                                >> Maybe.map Point2d.xCoordinate
                        }
                    , View.Table.columnFloat
                        { label = "y"
                        , recordToFloat =
                            Pattern.point2d
                                >> State.finalValue pattern
                                >> Result.toMaybe
                                >> Maybe.map Point2d.yCoordinate
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << PointEditPressed
                        , onRemovePress = Just << PointDeletePressed
                        }
                    ]
                }
        }


viewAxes : Pattern -> Bool -> Element Msg
viewAxes pattern axesVisible =
    View.Navigation.accordion
        { onPress = AxesRulerClicked
        , label = "Axes"
        , open = axesVisible
        , content =
            View.Table.table
                { data = List.sortBy objectName (Pattern.axes pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            Pattern.name
                                >> Maybe.withDefault "<no name>"
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << AxisEditPressed
                        , onRemovePress = Just << AxisDeletePressed
                        }
                    ]
                }
        }


viewCircles : Pattern -> Bool -> Element Msg
viewCircles pattern circlesVisible =
    View.Navigation.accordion
        { onPress = CirclesRulerClicked
        , label = "Circles"
        , open = circlesVisible
        , content =
            View.Table.table
                { data = List.sortBy objectName (Pattern.circles pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            Pattern.name
                                >> Maybe.withDefault "<no name>"
                        }
                    , View.Table.columnFloat
                        { label = "x"
                        , recordToFloat =
                            Pattern.circle2d
                                >> State.finalValue pattern
                                >> Result.toMaybe
                                >> Maybe.map (Circle2d.centerPoint >> Point2d.xCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "y"
                        , recordToFloat =
                            Pattern.circle2d
                                >> State.finalValue pattern
                                >> Result.toMaybe
                                >> Maybe.map (Circle2d.centerPoint >> Point2d.yCoordinate)
                        }
                    , View.Table.columnFloat
                        { label = "r"
                        , recordToFloat =
                            Pattern.circle2d
                                >> State.finalValue pattern
                                >> Result.toMaybe
                                >> Maybe.map Circle2d.radius
                        }
                    , View.Table.columnActions
                        { onEditPress = always Nothing
                        , onRemovePress = always Nothing
                        }
                    ]
                }
        }


viewCurves : Pattern -> Bool -> Element Msg
viewCurves pattern curvesVisible =
    View.Navigation.accordion
        { onPress = CurvesRulerClicked
        , label = "Curves"
        , open = curvesVisible
        , content =
            View.Table.table
                { data = List.sortBy objectName (Pattern.curves pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            Pattern.name
                                >> Maybe.withDefault "<no name>"
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << CurveEditPressed
                        , onRemovePress = Just << CurveDeletePressed
                        }
                    ]
                }
        }


viewDetails : Pattern -> Bool -> Element Msg
viewDetails pattern curvesVisible =
    View.Navigation.accordion
        { onPress = DetailsRulerClicked
        , label = "Details"
        , open = curvesVisible
        , content =
            View.Table.table
                { data = List.sortBy objectName (Pattern.details pattern)
                , columns =
                    [ View.Table.column
                        { label = "Name"
                        , recordToString =
                            Pattern.name
                                >> Maybe.withDefault "<no name>"
                        }
                    , View.Table.columnActions
                        { onEditPress = Just << DetailEditPressed
                        , onRemovePress = Just << DetailDeletePressed
                        }
                    ]
                }
        }


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<no name>"



---- UPDATE


type Msg
    = NoOp
    | PatternReceived (Result Http.Error StoredPattern)
    | PatternUpdateReceived (Result Http.Error ())
    | WindowResized
    | PatternContainerViewportRequested
    | PatternContainerViewportReceived (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ZoomPlusClicked
    | ZoomMinusClicked
    | MouseDown Position
    | MouseMove Position
    | MouseUp Position
      -- PATTERN
    | ObjectHovered (Maybe HoveredObject)
      -- LEFT TOOLBAR
    | CreatePointPressed
    | CreateAxisPressed
    | CreateCirclePressed
    | CreateCurvePressed
    | CreateDetailPressed
    | DialogCreateMsg Dialog.CreateMsg
    | DialogEditMsg Dialog.EditMsg
      -- RIGHT TOOLBAR
    | ToolbarToggleClicked
    | VariablesRulerClicked
    | VariableCreateClicked
    | PointsRulerClicked
    | PointEditPressed (A Point)
    | PointDeletePressed (A Point)
    | AxesRulerClicked
    | AxisEditPressed (A Axis)
    | AxisDeletePressed (A Axis)
    | CirclesRulerClicked
    | CircleEditPressed (A Circle)
    | CircleDeletePressed (A Circle)
    | CurvesRulerClicked
    | CurveEditPressed (A Curve)
    | CurveDeletePressed (A Curve)
    | DetailsRulerClicked
    | DetailEditPressed (A Detail)
    | DetailDeletePressed (A Detail)
      -- VARIABLE DIALOG
    | VariableNameChanged String
    | VariableValueChanged String
    | VariableCreateSubmitClicked
    | VariableDialogCancelClicked
      -- MODALS
    | PointDeleteModalDeletePressed
    | AxisDeleteModalDeletePressed
    | CircleDeleteModalDeletePressed
    | CurveDeleteModalDeletePressed
    | DetailDeleteModalDeletePressed
    | ModalCancelPressed


update :
    Navigation.Key
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update key msg model =
    case model of
        Loading ->
            case msg of
                PatternReceived result ->
                    case result of
                        Err error ->
                            ( Error
                            , Cmd.none
                            )

                        Ok storedPattern ->
                            ( Loaded
                                { maybeDrag = Nothing
                                , patternContainerDimensions = Nothing
                                , maybeModal = Nothing

                                -- PATTERN
                                , storedPattern = storedPattern
                                , hoveredObject = Nothing

                                -- LEFT TOOLBAR
                                , maybeDialog = Nothing
                                , preventActionMenuClose = False

                                -- RIGHT TOOLBAR
                                , rightToolbarVisible = False
                                , variablesVisible = True
                                , maybeVariableDialog = Nothing
                                , pointsVisible = False
                                , axesVisible = False
                                , curvesVisible = False
                                , circlesVisible = False
                                , detailsVisible = False
                                }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        Error ->
            ( model, Cmd.none )

        Loaded data ->
            updateWithData key msg data
                |> Tuple.mapFirst Loaded


updateWithData : Navigation.Key -> Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateWithData key msg model =
    let
        { pattern, zoom, center } =
            storedPattern

        storedPattern =
            model.storedPattern
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PatternReceived result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok newStoredPattern ->
                    ( { model | storedPattern = newStoredPattern }
                    , Cmd.none
                    )

        PatternUpdateReceived result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , Cmd.none
                    )

        WindowResized ->
            ( model
            , Browser.Dom.getViewportOf "pattern-container"
                |> Task.attempt PatternContainerViewportReceived
            )

        PatternContainerViewportRequested ->
            ( model
            , Process.sleep 500
                |> Task.andThen (\_ -> Browser.Dom.getViewportOf "pattern-container")
                |> Task.attempt PatternContainerViewportReceived
            )

        PatternContainerViewportReceived result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok viewport ->
                    ( { model
                        | patternContainerDimensions =
                            Just
                                { width = viewport.viewport.width
                                , height = viewport.viewport.height
                                }
                      }
                    , Cmd.none
                    )

        ZoomPlusClicked ->
            let
                newStoredPattern =
                    { storedPattern
                        | zoom = zoom * 1.1
                        , center = Point2d.scaleAbout Point2d.origin 1.1 center
                    }
            in
            ( { model | storedPattern = newStoredPattern }
            , Api.updatePattern PatternUpdateReceived newStoredPattern
            )

        ZoomMinusClicked ->
            let
                newStoredPattern =
                    { storedPattern
                        | zoom = zoom / 1.1
                        , center = Point2d.scaleAbout Point2d.origin (1 / 1.1) center
                    }
            in
            ( { model | storedPattern = newStoredPattern }
            , Api.updatePattern PatternUpdateReceived newStoredPattern
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
            )

        MouseMove position ->
            case model.maybeDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just drag ->
                    ( { model | maybeDrag = Just { drag | current = position } }
                    , Cmd.none
                    )

        MouseUp position ->
            let
                newCenter =
                    case model.maybeDrag of
                        Nothing ->
                            center

                        Just drag ->
                            center
                                |> Point2d.translateBy
                                    (Vector2d.fromComponents
                                        ( drag.start.x - position.x
                                        , drag.start.y - position.y
                                        )
                                    )

                newStoredPattern =
                    { storedPattern | center = newCenter }
            in
            ( { model
                | maybeDrag = Nothing
                , storedPattern = newStoredPattern
              }
            , Api.updatePattern PatternUpdateReceived newStoredPattern
            )

        -- LEFT TOOLBAR
        CreatePointPressed ->
            ( { model | maybeDialog = Just (CreateObject Dialog.createPoint) }
            , Cmd.none
            )

        CreateAxisPressed ->
            ( { model | maybeDialog = Just (CreateObject Dialog.createAxis) }
            , Cmd.none
            )

        CreateCirclePressed ->
            ( { model | maybeDialog = Just (CreateObject Dialog.createCircle) }
            , Cmd.none
            )

        CreateCurvePressed ->
            ( { model | maybeDialog = Just (CreateObject Dialog.createCurve) }
            , Cmd.none
            )

        CreateDetailPressed ->
            ( { model | maybeDialog = Just (CreateObject Dialog.createDetail) }
            , Cmd.none
            )

        DialogCreateMsg dialogMsg ->
            case model.maybeDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (CreateObject dialog) ->
                    case Dialog.createUpdate pattern dialogMsg dialog of
                        Dialog.CreateOpen ( newDialog, dialogCmd ) ->
                            ( { model | maybeDialog = Just (CreateObject newDialog) }
                            , Cmd.map DialogCreateMsg dialogCmd
                            )

                        Dialog.CreateSucceeded newPattern ->
                            let
                                newStoredPattern =
                                    { storedPattern | pattern = newPattern }
                            in
                            ( { model
                                | maybeDialog = Nothing
                                , storedPattern = newStoredPattern
                              }
                            , Api.updatePattern PatternUpdateReceived newStoredPattern
                            )

                        Dialog.CreateCanceled ->
                            ( { model | maybeDialog = Nothing }
                            , Cmd.none
                            )

                Just (EditObject _ _) ->
                    ( model, Cmd.none )

        DialogEditMsg dialogMsg ->
            case model.maybeDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (CreateObject dialog) ->
                    ( model, Cmd.none )

                Just (EditObject name dialog) ->
                    case Dialog.editUpdate pattern dialogMsg dialog of
                        Dialog.EditOpen ( newDialog, dialogCmd ) ->
                            ( { model | maybeDialog = Just (EditObject name newDialog) }
                            , Cmd.map DialogEditMsg dialogCmd
                            )

                        Dialog.EditSucceeded newPattern ->
                            let
                                newStoredPattern =
                                    { storedPattern | pattern = newPattern }
                            in
                            ( { model
                                | maybeDialog = Nothing
                                , storedPattern = newStoredPattern
                              }
                            , Api.updatePattern PatternUpdateReceived newStoredPattern
                            )

                        Dialog.EditCanceled ->
                            ( { model | maybeDialog = Nothing }
                            , Cmd.none
                            )

        -- PATTERN
        ObjectHovered newHoveredObject ->
            ( { model | hoveredObject = newHoveredObject }
            , Cmd.none
            )

        -- RIGHT TOOLBAR
        ToolbarToggleClicked ->
            ( { model
                | rightToolbarVisible = not model.rightToolbarVisible
                , patternContainerDimensions = Nothing
              }
            , Browser.Dom.getViewportOf "pattern-container"
                |> Task.attempt PatternContainerViewportReceived
            )

        VariablesRulerClicked ->
            ( { model | variablesVisible = not model.variablesVisible }
            , Cmd.none
            )

        VariableCreateClicked ->
            ( { model
                | maybeVariableDialog =
                    Just <|
                        VariableDialogCreate
                            { name = ""
                            , nameHelp = Nothing
                            , value = ""
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        VariableNameChanged newName ->
            case model.maybeVariableDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (VariableDialogCreate data) ->
                    ( { model
                        | maybeVariableDialog =
                            Just <|
                                VariableDialogCreate { data | name = newName }
                      }
                    , Cmd.none
                    )

        VariableValueChanged newValue ->
            case model.maybeVariableDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (VariableDialogCreate data) ->
                    ( { model
                        | maybeVariableDialog =
                            Just <|
                                VariableDialogCreate { data | value = newValue }
                      }
                    , Cmd.none
                    )

        VariableCreateSubmitClicked ->
            case model.maybeVariableDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (VariableDialogCreate data) ->
                    case
                        Pattern.insertVariable data.name
                            data.value
                            storedPattern.pattern
                    of
                        Err NameTaken ->
                            ( { model
                                | maybeVariableDialog =
                                    Just <|
                                        VariableDialogCreate
                                            { data | nameHelp = Just "Name already taken" }
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                        Ok newPattern ->
                            let
                                newStoredPattern =
                                    { storedPattern | pattern = newPattern }
                            in
                            ( { model
                                | maybeVariableDialog = Nothing
                                , storedPattern = newStoredPattern
                              }
                            , Api.updatePattern PatternUpdateReceived newStoredPattern
                            )

        VariableDialogCancelClicked ->
            ( { model | maybeVariableDialog = Nothing }
            , Cmd.none
            )

        -- POINTS
        PointsRulerClicked ->
            ( { model | pointsVisible = not model.pointsVisible }
            , Cmd.none
            )

        PointEditPressed aPoint ->
            ( { model
                | maybeDialog =
                    Maybe.map2 EditObject
                        (Pattern.name aPoint)
                        (Dialog.editPoint pattern aPoint)
              }
            , Cmd.none
            )

        PointDeletePressed aPoint ->
            ( { model | maybeModal = Just (PointDeleteConfirm aPoint) }
            , Cmd.none
            )

        -- AXES
        AxesRulerClicked ->
            ( { model | axesVisible = not model.axesVisible }
            , Cmd.none
            )

        AxisEditPressed aAxis ->
            ( { model
                | maybeDialog =
                    Maybe.map2 EditObject
                        (Pattern.name aAxis)
                        (Dialog.editAxis pattern aAxis)
              }
            , Cmd.none
            )

        AxisDeletePressed aAxis ->
            ( { model | maybeModal = Just (AxisDeleteConfirm aAxis) }
            , Cmd.none
            )

        -- CIRCLES
        CirclesRulerClicked ->
            ( { model | circlesVisible = not model.circlesVisible }
            , Cmd.none
            )

        CircleEditPressed aCircle ->
            ( { model
                | maybeDialog =
                    Maybe.map2 EditObject
                        (Pattern.name aCircle)
                        (Dialog.editCircle pattern aCircle)
              }
            , Cmd.none
            )

        CircleDeletePressed aCircle ->
            ( { model | maybeModal = Just (CircleDeleteConfirm aCircle) }
            , Cmd.none
            )

        -- CURVES
        CurvesRulerClicked ->
            ( { model | curvesVisible = not model.curvesVisible }
            , Cmd.none
            )

        CurveEditPressed aCurve ->
            ( { model
                | maybeDialog =
                    Maybe.map2 EditObject
                        (Pattern.name aCurve)
                        (Dialog.editCurve pattern aCurve)
              }
            , Cmd.none
            )

        CurveDeletePressed aCurve ->
            ( { model | maybeModal = Just (CurveDeleteConfirm aCurve) }
            , Cmd.none
            )

        -- DETAILS
        DetailsRulerClicked ->
            ( { model | detailsVisible = not model.detailsVisible }
            , Cmd.none
            )

        DetailEditPressed aDetail ->
            ( { model
                | maybeDialog =
                    Maybe.map2 EditObject
                        (Pattern.name aDetail)
                        (Dialog.editDetail pattern aDetail)
              }
            , Cmd.none
            )

        DetailDeletePressed aDetail ->
            ( { model | maybeModal = Just (DetailDeleteConfirm aDetail) }
            , Cmd.none
            )

        -- MODALS
        PointDeleteModalDeletePressed ->
            case model.maybeModal of
                Just (PointDeleteConfirm aPoint) ->
                    let
                        newPattern =
                            Pattern.removePoint aPoint pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal = Nothing
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern PatternUpdateReceived newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        AxisDeleteModalDeletePressed ->
            case model.maybeModal of
                Just (AxisDeleteConfirm aAxis) ->
                    let
                        newPattern =
                            Pattern.removeAxis aAxis pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal = Nothing
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern PatternUpdateReceived newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        CircleDeleteModalDeletePressed ->
            case model.maybeModal of
                Just (CircleDeleteConfirm aCircle) ->
                    let
                        newPattern =
                            Pattern.removeCircle aCircle pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal = Nothing
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern PatternUpdateReceived newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        CurveDeleteModalDeletePressed ->
            case model.maybeModal of
                Just (CurveDeleteConfirm aCurve) ->
                    let
                        newPattern =
                            Pattern.removeCurve aCurve pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal = Nothing
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern PatternUpdateReceived newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        DetailDeleteModalDeletePressed ->
            case model.maybeModal of
                Just (DetailDeleteConfirm aDetail) ->
                    let
                        newPattern =
                            Pattern.removeDetail aDetail pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal = Nothing
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern PatternUpdateReceived newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        ModalCancelPressed ->
            ( { model | maybeModal = Nothing }
            , Cmd.none
            )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Error ->
            Sub.none

        Loaded data ->
            Sub.batch
                [ case data.patternContainerDimensions of
                    Just _ ->
                        Sub.none

                    Nothing ->
                        Browser.Events.onAnimationFrame
                            (\_ -> PatternContainerViewportRequested)
                , Browser.Events.onResize
                    (\_ _ -> WindowResized)
                , case data.maybeDrag of
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
