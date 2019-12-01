module Page.Editor exposing
    ( Model, init
    , view
    , Msg, update, subscriptions
    )

{-|

@docs Model, init
@docs view
@docs Msg, update, subscriptions

-}

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
import Dialog
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
import Length
import LineSegment2d
import List.Extra as List
import Listbox exposing (Listbox)
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Pattern exposing (A, Axis, Circle, Curve, Detail, InsertHelp(..), Pattern, Point)
import Pattern.Draw as Pattern exposing (Object(..))
import Pattern.Store exposing (StoredPattern)
import Pixels
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Process
import Quantity
import State
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Task
import Ui.Atom
import Ui.Atom.Tabs
import Ui.Color
import Ui.Modal
import Ui.Molecule.MenuBtn
import Ui.Molecule.ObjectList
import Ui.Molecule.Pattern
import Ui.Molecule.VariableList
import Ui.Navigation
import Ui.Space
import Ui.Table
import Ui.Typography
import Url exposing (Url)
import Vector2d
import VoronoiDiagram2d



---- MODEL


{-| -}
type Model
    = Loading
    | Error
    | Loaded LoadedData


type alias LoadedData =
    { maybeDrag : Maybe Drag
    , patternContainerDimensions : Maybe Dimensions
    , maybeModal : Maybe ( Modal, Ui.Modal.State )

    -- PATTERN
    , storedPattern : StoredPattern BottomLeft
    , patternState : Ui.Molecule.Pattern.State

    -- TOP TOOLBAR
    , createObjectMenuBtn : Ui.Molecule.MenuBtn.State

    -- LEFT TOOLBAR
    , selectedTab : Tab
    , focusedVariable : Maybe String
    , hoveredVariable : Maybe String

    -- RIGHT TOOLBAR
    , maybeDialog : Maybe Dialog
    , preventActionMenuClose : Bool
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type Modal
    = PointDeleteConfirm (A Point)
    | AxisDeleteConfirm (A Axis)
    | CircleDeleteConfirm (A Circle)
    | CurveDeleteConfirm (A Curve)
    | DetailDeleteConfirm (A Detail)
    | VariableDeleteConfirm String


type BottomLeft
    = BottomLeft BottomLeft


type Tab
    = ObjectsTab
    | VariablesTab


type Dialog
    = CreateObject Dialog.Create
    | EditObject String Dialog.Edit
    | CreateVariable
        { name : String
        , nameHelp : Maybe String
        , value : String
        }
    | EditVariable
        { name : String
        , value : String
        }


{-| -}
init : String -> ( Model, Cmd Msg )
init slug =
    ( Loading
    , Api.getPattern ReceivedPattern slug
    )


initLoaded : StoredPattern BottomLeft -> ( Model, Cmd Msg )
initLoaded storedPattern =
    ( Loaded
        { maybeDrag = Nothing
        , patternContainerDimensions = Nothing
        , maybeModal = Nothing

        -- PATTERN
        , storedPattern = storedPattern
        , patternState = Ui.Molecule.Pattern.init

        -- TOP TOOLBAR
        , createObjectMenuBtn = Ui.Molecule.MenuBtn.init

        -- LEFT TOOLBAR
        , selectedTab = ObjectsTab
        , focusedVariable = Nothing
        , hoveredVariable = Nothing

        -- RIGHT TOOLBAR
        , maybeDialog = Nothing
        , preventActionMenuClose = False
        }
    , Cmd.none
    )



---- VIEW


{-| -}
view : Model -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view model =
    case model of
        Loading ->
            { title = "Loading pattern.."
            , body =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Loading pattern..")
            , dialog = Nothing
            }

        Error ->
            { title = "Something went wrong."
            , body =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Loading pattern..")
            , dialog = Nothing
            }

        Loaded data ->
            { title = "Sewing Pattern Editor"
            , body = viewEditor data.storedPattern data
            , dialog = Maybe.map (viewModal data.storedPattern.pattern) data.maybeModal
            }



---- MODAL


viewModal : Pattern BottomLeft -> ( Modal, Ui.Modal.State ) -> Element Msg
viewModal pattern ( modal, state ) =
    case modal of
        PointDeleteConfirm aPoint ->
            let
                dependentObjects =
                    Pattern.objectsDependingOnPoint pattern aPoint

                viewDependentObjects =
                    [ Ui.Typography.paragraphBody
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
                        Element.el
                            [ Element.paddingEach
                                { top = 0
                                , bottom = 0
                                , left = Ui.Space.level1
                                , right = 0
                                }
                            ]
                        <|
                            Ui.Typography.paragraphBody
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
            Ui.Modal.small state
                { onCancelPress = UserPressedModalCancel
                , onClosed = UserClosedModal
                , title = "Delete «" ++ objectName aPoint ++ "»?"
                , content =
                    Element.column
                        [ Element.spacing Ui.Space.level3
                        , Element.htmlAttribute (Html.Attributes.id "dialog--body")
                        , Element.width Element.fill
                        , Element.padding Ui.Space.level2
                        , Background.color Ui.Color.white
                        ]
                        (Ui.Typography.paragraphBody
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
                    [ Ui.Atom.btnDanger
                        { id = "point-delete-modal__delete-btn"
                        , onPress = Just UserPressedPointDeleteModalDelete
                        , label = "Delete point"
                        }
                    , Element.el [ Element.alignRight ] <|
                        Ui.Atom.btnCancel
                            { id = "point-delete-modal__cancel-btn"
                            , onPress = Just UserPressedModalCancel
                            , label = "Cancel"
                            }
                    ]
                }

        AxisDeleteConfirm aAxis ->
            viewDeleteModal state
                { name = objectName aAxis
                , kind = "axis"
                , onDeletePress = UserPressedAxisDeleteModalDelete
                }

        CircleDeleteConfirm aCircle ->
            viewDeleteModal state
                { name = objectName aCircle
                , kind = "circle"
                , onDeletePress = UserPressedCircleDeleteModalDelete
                }

        CurveDeleteConfirm aCurve ->
            viewDeleteModal state
                { name = objectName aCurve
                , kind = "curve"
                , onDeletePress = UserPressedCurveDeleteModalDelete
                }

        DetailDeleteConfirm aDetail ->
            viewDeleteModal state
                { name = objectName aDetail
                , kind = "detail"
                , onDeletePress = UserPressedDetailDeleteModalDelete
                }

        VariableDeleteConfirm variable ->
            viewDeleteModal state
                { name = variable
                , kind = "variable"
                , onDeletePress = UserPressedVariableDeleteModalDelete
                }


viewDeleteModal : Ui.Modal.State -> { name : String, kind : String, onDeletePress : Msg } -> Element Msg
viewDeleteModal state { name, kind, onDeletePress } =
    Ui.Modal.small state
        { onCancelPress = UserPressedModalCancel
        , onClosed = UserClosedModal
        , title = "Delete «" ++ name ++ "»?"
        , content =
            Element.el [ Element.htmlAttribute (Html.Attributes.id "dialog--body") ] <|
                Ui.Typography.paragraphBody
                    [ Element.text ("Do you want to remove the " ++ kind ++ " ")
                    , Element.el [ Font.bold ]
                        (Element.text ("«" ++ name ++ "»"))
                    , Element.text "?"
                    ]
        , actions =
            [ Ui.Atom.btnDanger
                { id = "delete-modal__delete-btn"
                , onPress = Just onDeletePress
                , label = "Delete " ++ kind
                }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.btnCancel
                    { id = "delete-modal__cancel-btn"
                    , onPress = Just UserPressedModalCancel
                    , label = "Cancel"
                    }
            ]
        }



---- EDITOR


viewEditor : StoredPattern BottomLeft -> LoadedData -> Element Msg
viewEditor storedPattern model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewTopToolbar model storedPattern.name
        , horizontalRule
        , Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            ]
            [ viewLeftToolbar storedPattern.pattern model
            , viewWorkspace storedPattern model
            , viewRightToolbar storedPattern.pattern model
            ]
        , horizontalRule
        ]


horizontalRule : Element msg
horizontalRule =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 2)
        , Background.color Ui.Color.primary
        ]
        Element.none



---- TOP TOOLBAR


viewTopToolbar : LoadedData -> String -> Element Msg
viewTopToolbar model name =
    Element.row
        [ Element.width Element.fill
        , Element.padding (Ui.Space.level1 // 2)
        , Background.color Ui.Color.secondary
        ]
        [ Ui.Molecule.MenuBtn.viewPrimary
            { id = "create-object"
            , onMsg = CreateObjectMenuBtnMsg
            , actions =
                [ { label = "Create a point"
                  , action = CreatePoint
                  }
                , { label = "Create an axis"
                  , action = CreateAxis
                  }
                , { label = "Create a circle"
                  , action = CreateCircle
                  }
                , { label = "Create a curve"
                  , action = CreateCurve
                  }
                , { label = "Create a detail"
                  , action = CreateDetail
                  }
                ]
            }
            model.createObjectMenuBtn
        , Element.el
            [ Element.centerX
            , Font.bold
            ]
            (Ui.Typography.body name)
        , Element.newTabLink
            [ Element.alignRight
            , Element.mouseOver [ Font.color Ui.Color.primary ]
            , Element.paddingXY Ui.Space.level1 0
            ]
            { url = "https://github.com/kirchner/sewing-pattern-editor"
            , label = Ui.Atom.faBrandLarge "github"
            }
        ]



---- WORKSPACE


viewWorkspace : StoredPattern BottomLeft -> LoadedData -> Element Msg
viewWorkspace storedPattern model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront <|
            Element.el
                [ Element.alignRight
                , Element.alignBottom
                ]
                (viewZoom model)
        , Background.color (Element.rgb 255 255 255)
        , Border.widthEach
            { top = 1
            , bottom = 4
            , left = 1
            , right = 1
            }
        , Border.color Ui.Color.secondaryDark
        , Element.focused
            [ Border.color Ui.Color.complementary ]
        ]
        (Element.el
            [ Element.htmlAttribute (Html.Attributes.id "pattern-container")
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            (viewPattern model.patternContainerDimensions model.maybeDrag storedPattern model)
        )


viewPattern : Maybe Dimensions -> Maybe Drag -> StoredPattern BottomLeft -> LoadedData -> Element Msg
viewPattern maybeDimensions maybeDrag storedPattern model =
    let
        { pattern, center, zoom } =
            storedPattern
    in
    case maybeDimensions of
        Nothing ->
            Element.none

        Just { width, height } ->
            let
                resolution =
                    Pixels.pixels (zoom * width / 336)
                        |> Quantity.per (Length.millimeters 1)

                currentCenter =
                    case maybeDrag of
                        Nothing ->
                            center

                        Just drag ->
                            center
                                |> Point2d.translateBy
                                    (Vector2d.at_ resolution <|
                                        Vector2d.fromPixels
                                            { x = drag.start.x - drag.current.x
                                            , y = drag.start.y - drag.current.y
                                            }
                                    )
            in
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.htmlAttribute <|
                    Html.Events.preventDefaultOn "dragstart" (Decode.succeed ( NoOp, True ))
                , Element.htmlAttribute <|
                    Html.Events.on "mousedown" <|
                        Decode.map MouseDown <|
                            Decode.map2 Position
                                (Decode.field "screenX" Decode.float)
                                (Decode.field "screenY" Decode.float)
                ]
                (Element.map PatternMsg <|
                    Ui.Molecule.Pattern.view
                        { id = "pattern" }
                        { width = width
                        , height = height
                        , resolution = resolution
                        , center = currentCenter
                        }
                        pattern
                        model.patternState
                )


viewZoom : LoadedData -> Element Msg
viewZoom model =
    Element.row
        [ Element.padding 20
        , Element.spacing 10
        ]
        [ Ui.Atom.btnIconLarge
            { id = "zoom-plus-btn"
            , onPress = Just UserPressedZoomPlus
            , icon = "search-plus"
            }
        , Ui.Atom.btnIconLarge
            { id = "zoom-minus-btn"
            , onPress = Just UserPressedZoomMinus
            , icon = "search-minus"
            }
        ]



---- LEFT TOOLBAR


viewLeftToolbar : Pattern BottomLeft -> LoadedData -> Element Msg
viewLeftToolbar pattern model =
    Element.el
        [ Element.width (Element.px 400)
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Element.spacing Ui.Space.level1
        ]
        (Ui.Atom.Tabs.view
            { label = "Data"
            , tabs =
                [ { tag = ObjectsTab
                  , id = "objects"
                  , label = "Objects"
                  }
                , { tag = VariablesTab
                  , id = "variables"
                  , label = "Variables"
                  }
                ]
            , selected = model.selectedTab
            , onSelect = UserSelectedTab
            , content =
                \tag ->
                    case tag of
                        ObjectsTab ->
                            Ui.Molecule.ObjectList.view
                                { toMsg = ObjectListMsg
                                , hidePressed = UserPressedHideObject
                                , editPressed = UserPressedEditObject
                                , removePressed = UserPressedRemoveObject
                                }
                                pattern
                                model.patternState

                        VariablesTab ->
                            Ui.Molecule.VariableList.view
                                { onHover = UserHoveredVariable
                                , onLeave = UserLeftVariable
                                , onFocus = UserFocusedVariable
                                , onBlur = UserBluredVariable
                                , editPressed = UserPressedEditVariable
                                , removePressed = UserPressedRemoveVariable
                                }
                                pattern
                                model.focusedVariable
                                model.hoveredVariable
            }
        )



---- RIGHT TOOLBAR


viewRightToolbar : Pattern BottomLeft -> LoadedData -> Element Msg
viewRightToolbar pattern model =
    Element.el
        [ Element.width (Element.maximum 500 Element.fill)
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color Ui.Color.white
        , Border.widthEach
            { top = 1
            , bottom = 4
            , left = 1
            , right = 1
            }
        , Border.color Ui.Color.secondaryDark
        ]
        (case model.maybeDialog of
            Nothing ->
                Element.none

            Just (CreateVariable stuff) ->
                viewVariable stuff.name stuff.value

            Just (EditVariable stuff) ->
                viewEditVariable stuff.name stuff.value

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
        )



---- VARIABLE


viewVariable : String -> String -> Element Msg
viewVariable name value =
    Element.column
        [ Element.width Element.fill
        , Element.padding 15
        , Element.spacing 15
        ]
        [ Ui.Typography.paragraphBody
            [ Element.text "Create a new "
            , Element.el
                [ Font.bold ]
                (Element.text "variable")
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Ui.Atom.inputText
                { id = "name-input"
                , onChange = UserChangedVariableName
                , text = name
                , label = "Pick a name"
                , help = Nothing
                }
            , Ui.Atom.inputFormula
                { id = "variable-value--input"
                , onChange = UserChangedVariableValue
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
                Ui.Atom.btnPrimary
                    { id = "variable-create-submit-btn"
                    , onPress = Just UserPressedVariableCreateSubmit
                    , label = "Create"
                    }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.btnCancel
                    { id = "variable-create-cancel-btn"
                    , onPress = Just UserPressedVariableDialogCancel
                    , label = "Cancel"
                    }
            ]
        ]


viewEditVariable : String -> String -> Element Msg
viewEditVariable name value =
    Element.column
        [ Element.width Element.fill
        , Element.padding 15
        , Element.spacing 15
        ]
        [ Ui.Typography.paragraphBody
            [ Element.text "Edit the variable "
            , Element.el
                [ Font.bold ]
                (Element.text name)
            ]
        , Ui.Atom.inputFormula
            { id = "variable-value--input"
            , onChange = UserChangedVariableValue
            , text = value
            , label = "Value"
            , help = Nothing
            }
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 5
            ]
            [ Element.el [ Element.alignLeft ] <|
                Ui.Atom.btnPrimary
                    { id = "variable-edit-update-btn"
                    , onPress = Just UserPressedVariableEditUpdate
                    , label = "Update"
                    }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.btnCancel
                    { id = "variable-edit-cancel-btn"
                    , onPress = Just UserPressedVariableDialogCancel
                    , label = "Cancel"
                    }
            ]
        ]



---- UPDATE


{-| -}
type Msg
    = NoOp
    | ReceivedPattern (Result Http.Error (StoredPattern BottomLeft))
    | ReceivedPatternUpdate (Result Http.Error ())
      -- TOP TOOLBAR
    | CreateObjectMenuBtnMsg (Ui.Molecule.MenuBtn.Msg CreateAction)
      -- LEFT TOOLBAR
    | UserSelectedTab Tab String
      -- LEFT TOOLBAR OBJECTS
    | ObjectListMsg Ui.Molecule.ObjectList.Msg
    | UserPressedHideObject Object
    | UserPressedEditObject Object
    | UserPressedRemoveObject Object
      -- LEFT TOOLBAR VARIABLES
    | UserHoveredVariable String
    | UserLeftVariable String
    | UserFocusedVariable String
    | UserBluredVariable String
    | UserPressedEditVariable String
    | UserPressedRemoveVariable String
    | UserPressedCreateVariable
      -- PATTERN
    | PatternMsg Ui.Molecule.Pattern.Msg
    | UserResizedWindow
    | AnimationFrameRequestedPatternContainerViewport
    | UpdateRequestedPatternContainerViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | UserPressedZoomPlus
    | UserPressedZoomMinus
    | MouseDown Position
    | MouseMove Position
    | MouseUp Position
      -- RIGHT TOOLBAR
    | DialogCreateMsg Dialog.CreateMsg
    | DialogEditMsg Dialog.EditMsg
      -- VARIABLE DIALOG
    | UserChangedVariableName String
    | UserChangedVariableValue String
    | UserPressedVariableCreateSubmit
    | UserPressedVariableEditUpdate
    | UserPressedVariableDialogCancel
      -- MODALS
    | UserPressedPointDeleteModalDelete
    | UserPressedAxisDeleteModalDelete
    | UserPressedCircleDeleteModalDelete
    | UserPressedCurveDeleteModalDelete
    | UserPressedDetailDeleteModalDelete
    | UserPressedVariableDeleteModalDelete
    | ChangedModalState Ui.Modal.State
    | UserPressedModalCancel
    | UserClosedModal


type CreateAction
    = CreatePoint
    | CreateAxis
    | CreateCircle
    | CreateCurve
    | CreateDetail


{-| -}
update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case model of
        Loading ->
            case msg of
                ReceivedPattern result ->
                    case result of
                        Err error ->
                            ( Error, Cmd.none )

                        Ok storedPattern ->
                            initLoaded storedPattern

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

        ReceivedPattern result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok newStoredPattern ->
                    ( { model | storedPattern = newStoredPattern }
                    , Cmd.none
                    )

        ReceivedPatternUpdate result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , Cmd.none
                    )

        -- TOP TOOLBAR
        CreateObjectMenuBtnMsg menuBtnMsg ->
            let
                ( newMenuBtn, menuBtnCmd, maybeAction ) =
                    Ui.Molecule.MenuBtn.update menuBtnMsg model.createObjectMenuBtn
            in
            ( { model
                | createObjectMenuBtn = newMenuBtn
                , maybeDialog =
                    case maybeAction of
                        Nothing ->
                            model.maybeDialog

                        Just CreatePoint ->
                            Just (CreateObject Dialog.createPoint)

                        Just CreateAxis ->
                            Just (CreateObject Dialog.createAxis)

                        Just CreateCircle ->
                            Just (CreateObject Dialog.createCircle)

                        Just CreateCurve ->
                            Just (CreateObject Dialog.createCurve)

                        Just CreateDetail ->
                            Just (CreateObject Dialog.createDetail)
              }
            , Cmd.map CreateObjectMenuBtnMsg menuBtnCmd
            )

        -- LEFT TOOLBAR
        UserSelectedTab tab id ->
            ( { model | selectedTab = tab }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)
            )

        -- LEFT TOOLBAR OBJECTS
        ObjectListMsg objectListMsg ->
            ( { model
                | patternState =
                    Ui.Molecule.ObjectList.update objectListMsg
                        model.storedPattern.pattern
                        model.patternState
              }
            , Cmd.none
            )

        UserPressedHideObject object ->
            ( model, Cmd.none )

        UserPressedEditObject object ->
            ( { model
                | maybeDialog =
                    case object of
                        Point aPoint ->
                            Maybe.map2 EditObject
                                (Pattern.name aPoint)
                                (Dialog.editPoint pattern aPoint)

                        Axis aAxis ->
                            Maybe.map2 EditObject
                                (Pattern.name aAxis)
                                (Dialog.editAxis pattern aAxis)

                        Circle aCircle ->
                            Maybe.map2 EditObject
                                (Pattern.name aCircle)
                                (Dialog.editCircle pattern aCircle)

                        Curve aCurve ->
                            Maybe.map2 EditObject
                                (Pattern.name aCurve)
                                (Dialog.editCurve pattern aCurve)

                        Detail aDetail ->
                            Maybe.map2 EditObject
                                (Pattern.name aDetail)
                                (Dialog.editDetail pattern aDetail)
              }
            , Cmd.none
            )

        UserPressedRemoveObject object ->
            ( { model
                | maybeModal =
                    Just
                        ( case object of
                            Point aPoint ->
                                PointDeleteConfirm aPoint

                            Axis aAxis ->
                                AxisDeleteConfirm aAxis

                            Circle aCircle ->
                                CircleDeleteConfirm aCircle

                            Curve aCurve ->
                                CurveDeleteConfirm aCurve

                            Detail aDetail ->
                                DetailDeleteConfirm aDetail
                        , Ui.Modal.Opening
                        )
              }
            , Cmd.none
            )

        -- LEFT TOOLBAR VARIABLES
        UserHoveredVariable variable ->
            ( { model | hoveredVariable = Just variable }, Cmd.none )

        UserLeftVariable _ ->
            ( { model | hoveredVariable = Nothing }, Cmd.none )

        UserFocusedVariable variable ->
            ( { model | focusedVariable = Just variable }, Cmd.none )

        UserBluredVariable _ ->
            ( { model | focusedVariable = Nothing }, Cmd.none )

        UserPressedEditVariable variable ->
            ( { model
                | maybeDialog =
                    case Pattern.variableInfo variable pattern of
                        Nothing ->
                            Nothing

                        Just value ->
                            Just <|
                                EditVariable
                                    { name = variable
                                    , value = value
                                    }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        UserPressedRemoveVariable variable ->
            ( { model
                | maybeModal =
                    Just
                        ( VariableDeleteConfirm variable
                        , Ui.Modal.Opening
                        )
              }
            , Cmd.none
            )

        UserPressedCreateVariable ->
            ( { model
                | maybeDialog =
                    Just <|
                        CreateVariable
                            { name = ""
                            , nameHelp = Nothing
                            , value = ""
                            }
              }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        -- PATTERN
        PatternMsg patternMsg ->
            ( { model
                | patternState =
                    Ui.Molecule.Pattern.update patternMsg
                        model.storedPattern.pattern
                        model.patternState
              }
            , Cmd.none
            )

        UserResizedWindow ->
            ( model
            , Browser.Dom.getViewportOf "pattern-container"
                |> Task.attempt UpdateRequestedPatternContainerViewport
            )

        AnimationFrameRequestedPatternContainerViewport ->
            ( model
            , Process.sleep 500
                |> Task.andThen (\_ -> Browser.Dom.getViewportOf "pattern-container")
                |> Task.attempt UpdateRequestedPatternContainerViewport
            )

        UpdateRequestedPatternContainerViewport result ->
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

        UserPressedZoomPlus ->
            let
                newStoredPattern =
                    { storedPattern
                        | zoom = zoom * 1.1
                        , center = Point2d.scaleAbout Point2d.origin 1.1 center
                    }
            in
            ( { model | storedPattern = newStoredPattern }
            , Api.updatePattern ReceivedPatternUpdate newStoredPattern
            )

        UserPressedZoomMinus ->
            let
                newStoredPattern =
                    { storedPattern
                        | zoom = zoom / 1.1
                        , center = Point2d.scaleAbout Point2d.origin (1 / 1.1) center
                    }
            in
            ( { model | storedPattern = newStoredPattern }
            , Api.updatePattern ReceivedPatternUpdate newStoredPattern
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
            case model.patternContainerDimensions of
                Nothing ->
                    ( { model | maybeDrag = Nothing }
                    , Cmd.none
                    )

                Just { width } ->
                    let
                        resolution =
                            Pixels.pixels (zoom * width / 336)
                                |> Quantity.per (Length.millimeters 1)

                        newCenter =
                            case model.maybeDrag of
                                Nothing ->
                                    center

                                Just drag ->
                                    center
                                        |> Point2d.translateBy
                                            (Vector2d.at_ resolution <|
                                                Vector2d.fromPixels
                                                    { x = drag.start.x - drag.current.x
                                                    , y = drag.start.y - drag.current.y
                                                    }
                                            )

                        newStoredPattern =
                            { storedPattern | center = newCenter }
                    in
                    ( { model
                        | maybeDrag = Nothing
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

        -- RIGHT TOOLBAR
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
                            , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                            )

                        Dialog.CreateCanceled ->
                            ( { model | maybeDialog = Nothing }
                            , Cmd.none
                            )

                Just (EditObject _ _) ->
                    ( model, Cmd.none )

                Just (CreateVariable _) ->
                    ( model, Cmd.none )

                Just (EditVariable _) ->
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
                            , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                            )

                        Dialog.EditCanceled ->
                            ( { model | maybeDialog = Nothing }
                            , Cmd.none
                            )

                Just (CreateVariable _) ->
                    ( model, Cmd.none )

                Just (EditVariable _) ->
                    ( model, Cmd.none )

        -- VARIABLE DIALOG
        UserChangedVariableName newName ->
            case model.maybeDialog of
                Just (CreateVariable data) ->
                    ( { model
                        | maybeDialog =
                            Just <|
                                CreateVariable { data | name = newName }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UserChangedVariableValue newValue ->
            case model.maybeDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (CreateObject _) ->
                    ( model, Cmd.none )

                Just (EditObject _ _) ->
                    ( model, Cmd.none )

                Just (CreateVariable data) ->
                    ( { model
                        | maybeDialog =
                            Just <|
                                CreateVariable { data | value = newValue }
                      }
                    , Cmd.none
                    )

                Just (EditVariable data) ->
                    ( { model
                        | maybeDialog =
                            Just <|
                                EditVariable { data | value = newValue }
                      }
                    , Cmd.none
                    )

        UserPressedVariableCreateSubmit ->
            case model.maybeDialog of
                Just (CreateVariable data) ->
                    case
                        Pattern.insertVariable data.name
                            data.value
                            storedPattern.pattern
                    of
                        Err NameTaken ->
                            ( { model
                                | maybeDialog =
                                    Just <|
                                        CreateVariable
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
                                | maybeDialog = Nothing
                                , storedPattern = newStoredPattern
                              }
                            , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                            )

                _ ->
                    ( model, Cmd.none )

        UserPressedVariableEditUpdate ->
            case model.maybeDialog of
                Just (EditVariable data) ->
                    case Pattern.replaceVariable data.name data.value pattern of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok newPattern ->
                            let
                                newStoredPattern =
                                    { storedPattern | pattern = newPattern }
                            in
                            ( { model
                                | maybeDialog = Nothing
                                , storedPattern = newStoredPattern
                              }
                            , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                            )

                _ ->
                    ( model, Cmd.none )

        UserPressedVariableDialogCancel ->
            ( { model | maybeDialog = Nothing }
            , Cmd.none
            )

        -- MODALS
        UserPressedPointDeleteModalDelete ->
            case model.maybeModal of
                Just ( PointDeleteConfirm aPoint, state ) ->
                    let
                        newPattern =
                            Pattern.removePoint aPoint pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( PointDeleteConfirm aPoint
                                , Ui.Modal.Closing
                                )
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedAxisDeleteModalDelete ->
            case model.maybeModal of
                Just ( AxisDeleteConfirm aAxis, state ) ->
                    let
                        newPattern =
                            Pattern.removeAxis aAxis pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( AxisDeleteConfirm aAxis
                                , Ui.Modal.Closing
                                )
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedCircleDeleteModalDelete ->
            case model.maybeModal of
                Just ( CircleDeleteConfirm aCircle, state ) ->
                    let
                        newPattern =
                            Pattern.removeCircle aCircle pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( CircleDeleteConfirm aCircle
                                , Ui.Modal.Closing
                                )
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedCurveDeleteModalDelete ->
            case model.maybeModal of
                Just ( CurveDeleteConfirm aCurve, state ) ->
                    let
                        newPattern =
                            Pattern.removeCurve aCurve pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( CurveDeleteConfirm aCurve
                                , Ui.Modal.Closing
                                )
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedDetailDeleteModalDelete ->
            case model.maybeModal of
                Just ( DetailDeleteConfirm aDetail, state ) ->
                    let
                        newPattern =
                            Pattern.removeDetail aDetail pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( DetailDeleteConfirm aDetail
                                , Ui.Modal.Closing
                                )
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedVariableDeleteModalDelete ->
            case model.maybeModal of
                Just ( VariableDeleteConfirm variable, state ) ->
                    let
                        newPattern =
                            Pattern.removeVariable variable pattern

                        newStoredPattern =
                            { storedPattern | pattern = newPattern }
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( VariableDeleteConfirm variable
                                , Ui.Modal.Closing
                                )
                        , storedPattern = newStoredPattern
                      }
                    , Api.updatePattern ReceivedPatternUpdate newStoredPattern
                    )

                _ ->
                    ( model, Cmd.none )

        ChangedModalState newState ->
            case model.maybeModal of
                Nothing ->
                    ( model, Cmd.none )

                Just ( modal, _ ) ->
                    ( { model | maybeModal = Just ( modal, newState ) }
                    , Cmd.none
                    )

        UserPressedModalCancel ->
            case model.maybeModal of
                Nothing ->
                    ( model, Cmd.none )

                Just ( modal, state ) ->
                    ( { model | maybeModal = Just ( modal, Ui.Modal.Closing ) }
                    , Cmd.none
                    )

        UserClosedModal ->
            ( { model | maybeModal = Nothing }
            , Cmd.none
            )



---- SUBSCRIPTIONS


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Error ->
            Sub.none

        Loaded data ->
            Sub.batch
                [ case data.maybeModal of
                    Nothing ->
                        Sub.none

                    Just ( _, state ) ->
                        Sub.map ChangedModalState (Ui.Modal.subscriptions state)
                , case data.patternContainerDimensions of
                    Just _ ->
                        Sub.none

                    Nothing ->
                        Browser.Events.onAnimationFrame
                            (\_ -> AnimationFrameRequestedPatternContainerViewport)
                , Browser.Events.onResize
                    (\_ _ -> UserResizedWindow)
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



---- HELPER


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<no name>"
