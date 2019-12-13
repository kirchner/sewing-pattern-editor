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
import Git
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Length exposing (Meters)
import LineSegment2d
import List.Extra as List
import Listbox exposing (Listbox)
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import LocalStorage
import Pattern exposing (A, Axis, Circle, Curve, Detail, InsertHelp(..), Object(..), Pattern, Point)
import Pattern.Store exposing (StoredPattern)
import Pixels
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Process
import Quantity
import Route
import State
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Task
import Ui.Atom
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Atom.Tabs
import Ui.Molecule.MenuBtn
import Ui.Molecule.Modal
import Ui.Molecule.ObjectList
import Ui.Molecule.Pattern
import Ui.Molecule.VariableList
import Ui.Organism.Dialog
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Url exposing (Url)
import Url.Builder
import Vector2d
import VoronoiDiagram2d



---- MODEL


{-| -}
type Model
    = RequestingToken RequestingTokenData
    | Loading LoadingData
    | Error
    | Loaded LoadedData


type alias RequestingTokenData =
    { repo : Git.Repo
    , ref : Git.Ref
    }


type alias LoadingData =
    { identity : Git.Identity
    , repo : Git.Repo
    , ref : Git.Ref
    , maybePatternData : Maybe (Git.PatternData BottomLeft)
    , maybeMeta : Maybe Git.Meta
    , maybePermissions : Maybe Git.Permissions
    }


type alias LoadedData =
    { maybeDrag : Maybe Drag
    , patternContainerDimensions : Maybe Dimensions
    , maybeModal : Maybe ( Modal, Ui.Molecule.Modal.State )

    -- PATTERN
    , identity : Git.Identity
    , repo : Git.Repo
    , ref : Git.Ref
    , permissions : Git.Permissions
    , sha : String
    , pattern : Pattern BottomLeft
    , name : String
    , zoom : Float
    , center : Point2d Meters BottomLeft
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
    = CreateObject Ui.Organism.Dialog.Create
    | EditObject String Ui.Organism.Dialog.Edit
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
init : Git.Repo -> Git.Ref -> Maybe String -> ( Model, Cmd Msg )
init repo ref maybeCode =
    case maybeCode of
        Nothing ->
            initLoading repo ref Git.Anonymous

        Just code ->
            ( RequestingToken
                { repo = repo
                , ref = ref
                }
            , Http.get
                { url =
                    Url.Builder.absolute [ "access_token" ]
                        [ Url.Builder.string "code" code ]
                , expect =
                    Http.expectJson ReceivedGithubAccessToken
                        (Decode.field "access_token" Decode.string)
                }
            )


initLoading : Git.Repo -> Git.Ref -> Git.Identity -> ( Model, Cmd Msg )
initLoading repo ref identity =
    ( Loading
        { identity = identity
        , repo = repo
        , ref = ref
        , maybePatternData = Nothing
        , maybeMeta = Nothing
        , maybePermissions = Nothing
        }
    , Cmd.batch
        [ Git.getPattern identity
            { repo = repo
            , ref = ref
            , onPatternData = ReceivedPatternData
            }
        , Git.getMeta identity
            { repo = repo
            , ref = ref
            , onMeta = ReceivedMeta
            }
        , Git.getPermissions identity
            { repo = repo
            , onPermissions = ReceivedPermissions
            }
        ]
    )


initLoaded :
    Git.Identity
    -> Git.Repo
    -> Git.Ref
    -> String
    -> Pattern BottomLeft
    -> Git.Meta
    -> Git.Permissions
    -> ( Model, Cmd Msg )
initLoaded identity repo ref sha pattern meta permissions =
    ( Loaded
        { maybeDrag = Nothing
        , patternContainerDimensions = Nothing
        , maybeModal = Nothing

        -- PATTERN
        , identity = identity
        , repo = repo
        , ref = ref
        , permissions = permissions
        , sha = sha
        , pattern = pattern
        , name = meta.name
        , zoom = 1
        , center = Point2d.origin
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
    , Cmd.batch
        [ LocalStorage.requestZoom
            { repo = repo
            , ref = ref
            }
        , LocalStorage.requestCenter
            { repo = repo
            , ref = ref
            }
        ]
    )



---- VIEW


{-| -}
view : Model -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view model =
    case model of
        RequestingToken _ ->
            { title = "Requesting GitHub API Access Token..."
            , body =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Requesting GitHub API Access Token...")
            , dialog = Nothing
            }

        Loading _ ->
            { title = "Loading pattern..."
            , body =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Loading pattern...")
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
            , body = viewEditor data
            , dialog = Maybe.map (viewModal data.pattern) data.maybeModal
            }



---- MODAL


viewModal : Pattern BottomLeft -> ( Modal, Ui.Molecule.Modal.State ) -> Element Msg
viewModal pattern ( modal, state ) =
    case modal of
        PointDeleteConfirm aPoint ->
            let
                dependentObjects =
                    Pattern.objectsDependingOnPoint pattern aPoint

                viewDependentObjects =
                    [ Ui.Theme.Typography.paragraphBody
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
                                , left = Ui.Theme.Spacing.level1
                                , right = 0
                                }
                            ]
                        <|
                            Ui.Theme.Typography.paragraphBody
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
            Ui.Molecule.Modal.small state
                { onCancelPress = UserPressedModalCancel
                , onClosed = UserClosedModal
                , title = "Delete «" ++ objectName aPoint ++ "»?"
                , content =
                    Element.column
                        [ Element.spacing Ui.Theme.Spacing.level3
                        , Element.htmlAttribute (Html.Attributes.id "dialog--body")
                        , Element.width Element.fill
                        , Element.padding Ui.Theme.Spacing.level2
                        , Background.color Ui.Theme.Color.white
                        ]
                        (Ui.Theme.Typography.paragraphBody
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
                    [ Ui.Atom.Input.btnDanger
                        { id = "point-delete-modal__delete-btn"
                        , onPress = Just UserPressedPointDeleteModalDelete
                        , label = "Delete point"
                        }
                    , Element.el [ Element.alignRight ] <|
                        Ui.Atom.Input.btnCancel
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


viewDeleteModal : Ui.Molecule.Modal.State -> { name : String, kind : String, onDeletePress : Msg } -> Element Msg
viewDeleteModal state { name, kind, onDeletePress } =
    Ui.Molecule.Modal.small state
        { onCancelPress = UserPressedModalCancel
        , onClosed = UserClosedModal
        , title = "Delete «" ++ name ++ "»?"
        , content =
            Element.el [ Element.htmlAttribute (Html.Attributes.id "dialog--body") ] <|
                Ui.Theme.Typography.paragraphBody
                    [ Element.text ("Do you want to remove the " ++ kind ++ " ")
                    , Element.el [ Font.bold ]
                        (Element.text ("«" ++ name ++ "»"))
                    , Element.text "?"
                    ]
        , actions =
            [ Ui.Atom.Input.btnDanger
                { id = "delete-modal__delete-btn"
                , onPress = Just onDeletePress
                , label = "Delete " ++ kind
                }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.Input.btnCancel
                    { id = "delete-modal__cancel-btn"
                    , onPress = Just UserPressedModalCancel
                    , label = "Cancel"
                    }
            ]
        }



---- EDITOR


viewEditor : LoadedData -> Element Msg
viewEditor model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewTopToolbar model
        , horizontalRule
        , Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            ]
            [ viewLeftToolbar model
            , viewWorkspace model
            , viewRightToolbar model
            ]
        , horizontalRule
        ]


horizontalRule : Element msg
horizontalRule =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 2)
        , Background.color Ui.Theme.Color.primary
        ]
        Element.none



---- TOP TOOLBAR


viewTopToolbar : LoadedData -> Element Msg
viewTopToolbar model =
    Element.row
        [ Element.width Element.fill
        , Element.padding (Ui.Theme.Spacing.level1 // 2)
        , Background.color Ui.Theme.Color.secondary
        ]
        [ if model.permissions.push then
            viewCreateBtn model

          else
            viewSignInBtn
        , Element.el
            [ Element.centerX
            , Font.bold
            ]
            (Ui.Theme.Typography.body model.name)
        , Element.row
            [ Element.paddingXY Ui.Theme.Spacing.level4 0
            , Element.spacing Ui.Theme.Spacing.level1
            , Font.color Ui.Theme.Color.grayDark
            ]
            [ Ui.Theme.Typography.button "github"
            , Ui.Theme.Typography.button "/"
            , Ui.Theme.Typography.button model.repo.owner
            , Ui.Theme.Typography.button "/"
            , Ui.Theme.Typography.button model.repo.name
            ]
        ]


viewCreateBtn : LoadedData -> Element Msg
viewCreateBtn model =
    Ui.Molecule.MenuBtn.viewPrimary
        { id = "create-object"
        , onMsg = CreateObjectMenuBtnMsg
        , actions =
            [ { icon = Ui.Atom.Icon.point
              , label = "Create a point"
              , action = CreatePoint
              }
            , { icon = Ui.Atom.Icon.axis
              , label = "Create an axis"
              , action = CreateAxis
              }
            , { icon = Ui.Atom.Icon.circle
              , label = "Create a circle"
              , action = CreateCircle
              }
            , { icon = Ui.Atom.Icon.curve
              , label = "Create a curve"
              , action = CreateCurve
              }
            , { icon = Ui.Atom.Icon.detail
              , label = "Create a detail"
              , action = CreateDetail
              }
            ]
        }
        model.createObjectMenuBtn


viewSignInBtn : Element Msg
viewSignInBtn =
    Element.el []
        (Ui.Atom.Input.btnPrimary
            { id = "sign-in-btn"
            , onPress = Just UserPressedSignIn
            , label = "Sign in via GitHub"
            }
        )



---- WORKSPACE


viewWorkspace : LoadedData -> Element Msg
viewWorkspace model =
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
        , Border.color Ui.Theme.Color.secondaryDark
        , Element.focused
            [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.htmlAttribute (Html.Attributes.id "pattern-container")
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            (viewPattern model.patternContainerDimensions model.maybeDrag model)
        )


viewPattern : Maybe Dimensions -> Maybe Drag -> LoadedData -> Element Msg
viewPattern maybeDimensions maybeDrag model =
    case maybeDimensions of
        Nothing ->
            Element.none

        Just { width, height } ->
            let
                resolution =
                    Pixels.pixels (model.zoom * width / 336)
                        |> Quantity.per (Length.millimeters 1)

                currentCenter =
                    case maybeDrag of
                        Nothing ->
                            model.center

                        Just drag ->
                            model.center
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
                        model.pattern
                        model.patternState
                )


viewZoom : LoadedData -> Element Msg
viewZoom model =
    Element.row
        [ Element.padding 20
        , Element.spacing 10
        ]
        [ Ui.Atom.Input.btnIconLarge
            { id = "zoom-plus-btn"
            , onPress = Just UserPressedZoomPlus
            , icon = "search-plus"
            }
        , Ui.Atom.Input.btnIconLarge
            { id = "zoom-minus-btn"
            , onPress = Just UserPressedZoomMinus
            , icon = "search-minus"
            }
        ]



---- LEFT TOOLBAR


viewLeftToolbar : LoadedData -> Element Msg
viewLeftToolbar model =
    Element.el
        [ Element.width (Element.px 400)
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Element.spacing Ui.Theme.Spacing.level1
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
                                model.pattern
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
                                model.pattern
                                model.focusedVariable
                                model.hoveredVariable
            }
        )



---- RIGHT TOOLBAR


viewRightToolbar : LoadedData -> Element Msg
viewRightToolbar model =
    Element.el
        [ Element.width (Element.maximum 500 Element.fill)
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color Ui.Theme.Color.white
        , Border.widthEach
            { top = 1
            , bottom = 4
            , left = 1
            , right = 1
            }
        , Border.color Ui.Theme.Color.secondaryDark
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
                    Ui.Organism.Dialog.createView
                        { pattern = model.pattern
                        , hoveredInCanvas = Nothing
                        }
                        dialog

            Just (EditObject name dialog) ->
                Element.map DialogEditMsg <|
                    Ui.Organism.Dialog.editView
                        { pattern = model.pattern
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
        [ Ui.Theme.Typography.paragraphBody
            [ Element.text "Create a new "
            , Element.el
                [ Font.bold ]
                (Element.text "variable")
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Ui.Atom.Input.text
                { id = "name-input"
                , onChange = UserChangedVariableName
                , text = name
                , label = "Pick a name"
                , help = Nothing
                }
            , Ui.Atom.Input.formula
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
                Ui.Atom.Input.btnPrimary
                    { id = "variable-create-submit-btn"
                    , onPress = Just UserPressedVariableCreateSubmit
                    , label = "Create"
                    }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.Input.btnCancel
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
        [ Ui.Theme.Typography.paragraphBody
            [ Element.text "Edit the variable "
            , Element.el
                [ Font.bold ]
                (Element.text name)
            ]
        , Ui.Atom.Input.formula
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
                Ui.Atom.Input.btnPrimary
                    { id = "variable-edit-update-btn"
                    , onPress = Just UserPressedVariableEditUpdate
                    , label = "Update"
                    }
            , Element.el [ Element.alignRight ] <|
                Ui.Atom.Input.btnCancel
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
    | ReceivedGithubAccessToken (Result Http.Error String)
    | ReceivedPatternData (Result Http.Error (Git.PatternData BottomLeft))
    | ReceivedSha (Result Http.Error String)
    | ReceivedMeta (Result Http.Error Git.Meta)
    | ReceivedPermissions (Result Http.Error Git.Permissions)
    | ReceivedPatternUpdate (Result Http.Error ())
      -- LOCAL STORAGE
    | ChangedZoom LocalStorage.Address Float
    | ChangedCenter LocalStorage.Address (Point2d Meters BottomLeft)
    | ChangedWhatever
      -- TOP TOOLBAR
    | CreateObjectMenuBtnMsg (Ui.Molecule.MenuBtn.Msg CreateAction)
    | UserPressedSignIn
    | ReceivedClientId (Result Http.Error String)
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
    | DialogCreateMsg Ui.Organism.Dialog.CreateMsg
    | DialogEditMsg Ui.Organism.Dialog.EditMsg
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
    | ChangedModalState Ui.Molecule.Modal.State
    | UserPressedModalCancel
    | UserClosedModal


type CreateAction
    = CreatePoint
    | CreateAxis
    | CreateCircle
    | CreateCurve
    | CreateDetail


{-| -}
update : Navigation.Key -> String -> Msg -> Model -> ( Model, Cmd Msg )
update key domain msg model =
    case model of
        RequestingToken data ->
            case msg of
                ReceivedGithubAccessToken (Ok accessToken) ->
                    initLoading data.repo data.ref (Git.OauthToken accessToken)

                ReceivedGithubAccessToken (Err _) ->
                    ( Error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Loading data ->
            let
                tryInitLoaded newModel =
                    case newModel of
                        Loading newData ->
                            case
                                ( newData.maybePatternData
                                , newData.maybeMeta
                                , newData.maybePermissions
                                )
                            of
                                ( Just patternData, Just meta, Just permissions ) ->
                                    initLoaded
                                        newData.identity
                                        newData.repo
                                        newData.ref
                                        patternData.sha
                                        patternData.pattern
                                        meta
                                        permissions

                                _ ->
                                    ( newModel, Cmd.none )

                        _ ->
                            ( newModel, Cmd.none )
            in
            tryInitLoaded <|
                case msg of
                    ReceivedPatternData result ->
                        case result of
                            Err error ->
                                Error

                            Ok patternData ->
                                Loading { data | maybePatternData = Just patternData }

                    ReceivedMeta result ->
                        case result of
                            Err error ->
                                Error

                            Ok meta ->
                                Loading { data | maybeMeta = Just meta }

                    ReceivedPermissions result ->
                        case result of
                            Err error ->
                                Error

                            Ok permissions ->
                                Loading { data | maybePermissions = Just permissions }

                    _ ->
                        model

        Error ->
            ( model, Cmd.none )

        Loaded data ->
            updateWithData key domain msg data
                |> Tuple.mapFirst Loaded


updateWithData : Navigation.Key -> String -> Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateWithData key domain msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedGithubAccessToken _ ->
            ( model, Cmd.none )

        ReceivedPatternData result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok patternData ->
                    ( { model
                        | sha = patternData.sha
                        , pattern = patternData.pattern
                      }
                    , Cmd.none
                    )

        ReceivedSha result ->
            case result of
                Err error ->
                    ( model, Cmd.none )

                Ok newSha ->
                    ( { model | sha = newSha }
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

        ReceivedMeta result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok meta ->
                    ( { model | name = meta.name }
                    , Cmd.none
                    )

        ReceivedPermissions result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok newPermissions ->
                    ( { model | permissions = newPermissions }
                    , Cmd.none
                    )

        -- LOCAL STORAGE
        ChangedZoom { repo, ref } zoom ->
            if model.repo == repo && model.ref == ref then
                ( { model | zoom = zoom }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ChangedCenter { repo, ref } center ->
            if model.repo == repo && model.ref == ref then
                ( { model
                    | center = center
                    , maybeDrag = Nothing
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ChangedWhatever ->
            ( model, Cmd.none )

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
                            Just (CreateObject Ui.Organism.Dialog.createPoint)

                        Just CreateAxis ->
                            Just (CreateObject Ui.Organism.Dialog.createAxis)

                        Just CreateCircle ->
                            Just (CreateObject Ui.Organism.Dialog.createCircle)

                        Just CreateCurve ->
                            Just (CreateObject Ui.Organism.Dialog.createCurve)

                        Just CreateDetail ->
                            Just (CreateObject Ui.Organism.Dialog.createDetail)
              }
            , Cmd.map CreateObjectMenuBtnMsg menuBtnCmd
            )

        UserPressedSignIn ->
            ( model
            , Http.get
                { url = "/client_id"
                , expect = Http.expectString ReceivedClientId
                }
            )

        ReceivedClientId result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok clientId ->
                    ( model
                    , Navigation.load <|
                        Url.Builder.crossOrigin "https://github.com"
                            [ "login", "oauth", "authorize" ]
                            [ Url.Builder.string "client_id" clientId
                            , Url.Builder.string "redirect_uri"
                                (Url.Builder.crossOrigin domain
                                    [ Route.toString
                                        (Route.GitHub model.repo model.ref Nothing)
                                    ]
                                    []
                                )
                            , Url.Builder.string "scope" "repo"
                            ]
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
                    Ui.Molecule.ObjectList.update objectListMsg model.pattern model.patternState
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
                                (Ui.Organism.Dialog.editPoint model.pattern aPoint)

                        Axis aAxis ->
                            Maybe.map2 EditObject
                                (Pattern.name aAxis)
                                (Ui.Organism.Dialog.editAxis model.pattern aAxis)

                        Circle aCircle ->
                            Maybe.map2 EditObject
                                (Pattern.name aCircle)
                                (Ui.Organism.Dialog.editCircle model.pattern aCircle)

                        Curve aCurve ->
                            Maybe.map2 EditObject
                                (Pattern.name aCurve)
                                (Ui.Organism.Dialog.editCurve model.pattern aCurve)

                        Detail aDetail ->
                            Maybe.map2 EditObject
                                (Pattern.name aDetail)
                                (Ui.Organism.Dialog.editDetail model.pattern aDetail)
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
                        , Ui.Molecule.Modal.Opening
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
                    case Pattern.variableInfo variable model.pattern of
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
                        , Ui.Molecule.Modal.Opening
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
                        model.pattern
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
            ( model
            , LocalStorage.updateZoom
                { repo = model.repo
                , ref = model.ref
                }
                (model.zoom * 1.1)
            )

        UserPressedZoomMinus ->
            ( model
            , LocalStorage.updateZoom
                { repo = model.repo
                , ref = model.ref
                }
                (model.zoom / 1.1)
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
                            Pixels.pixels (model.zoom * width / 336)
                                |> Quantity.per (Length.millimeters 1)

                        newCenter =
                            case model.maybeDrag of
                                Nothing ->
                                    model.center

                                Just drag ->
                                    model.center
                                        |> Point2d.translateBy
                                            (Vector2d.at_ resolution <|
                                                Vector2d.fromPixels
                                                    { x = drag.start.x - drag.current.x
                                                    , y = drag.start.y - drag.current.y
                                                    }
                                            )
                    in
                    ( model
                    , LocalStorage.updateCenter
                        { repo = model.repo
                        , ref = model.ref
                        }
                        newCenter
                    )

        -- RIGHT TOOLBAR
        DialogCreateMsg dialogMsg ->
            case model.maybeDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just (CreateObject dialog) ->
                    case Ui.Organism.Dialog.createUpdate model.pattern dialogMsg dialog of
                        Ui.Organism.Dialog.CreateOpen ( newDialog, dialogCmd ) ->
                            ( { model | maybeDialog = Just (CreateObject newDialog) }
                            , Cmd.map DialogCreateMsg dialogCmd
                            )

                        Ui.Organism.Dialog.CreateSucceeded newPattern ->
                            ( { model
                                | maybeDialog = Nothing
                                , pattern = newPattern
                              }
                            , Git.putPattern model.identity
                                { repo = model.repo
                                , message = "create object"
                                , pattern = newPattern
                                , sha = model.sha
                                , onSha = ReceivedSha
                                }
                            )

                        Ui.Organism.Dialog.CreateCanceled ->
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
                    case Ui.Organism.Dialog.editUpdate model.pattern dialogMsg dialog of
                        Ui.Organism.Dialog.EditOpen ( newDialog, dialogCmd ) ->
                            ( { model | maybeDialog = Just (EditObject name newDialog) }
                            , Cmd.map DialogEditMsg dialogCmd
                            )

                        Ui.Organism.Dialog.EditSucceeded newPattern ->
                            ( { model
                                | maybeDialog = Nothing
                                , pattern = newPattern
                              }
                            , Git.putPattern model.identity
                                { repo = model.repo
                                , message = "edit object"
                                , pattern = newPattern
                                , sha = model.sha
                                , onSha = ReceivedSha
                                }
                            )

                        Ui.Organism.Dialog.EditCanceled ->
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
                    case Pattern.insertVariable data.name data.value model.pattern of
                        Err NameTaken ->
                            ( { model
                                | maybeDialog =
                                    Just (CreateVariable { data | nameHelp = Just "Name already taken" })
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                        Ok newPattern ->
                            ( { model
                                | maybeDialog = Nothing
                                , pattern = newPattern
                              }
                            , Git.putPattern model.identity
                                { repo = model.repo
                                , message = "create variable"
                                , pattern = newPattern
                                , sha = model.sha
                                , onSha = ReceivedSha
                                }
                            )

                _ ->
                    ( model, Cmd.none )

        UserPressedVariableEditUpdate ->
            case model.maybeDialog of
                Just (EditVariable data) ->
                    case Pattern.replaceVariable data.name data.value model.pattern of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok newPattern ->
                            ( { model
                                | maybeDialog = Nothing
                                , pattern = newPattern
                              }
                            , Git.putPattern model.identity
                                { repo = model.repo
                                , message = "edit variable"
                                , pattern = newPattern
                                , sha = model.sha
                                , onSha = ReceivedSha
                                }
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
                            Pattern.removePoint aPoint model.pattern
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( PointDeleteConfirm aPoint
                                , Ui.Molecule.Modal.Closing
                                )
                        , pattern = newPattern
                      }
                    , Git.putPattern model.identity
                        { repo = model.repo
                        , message = "delete point"
                        , pattern = newPattern
                        , sha = model.sha
                        , onSha = ReceivedSha
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedAxisDeleteModalDelete ->
            case model.maybeModal of
                Just ( AxisDeleteConfirm aAxis, state ) ->
                    let
                        newPattern =
                            Pattern.removeAxis aAxis model.pattern
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( AxisDeleteConfirm aAxis
                                , Ui.Molecule.Modal.Closing
                                )
                        , pattern = newPattern
                      }
                    , Git.putPattern model.identity
                        { repo = model.repo
                        , message = "delete axis"
                        , pattern = newPattern
                        , sha = model.sha
                        , onSha = ReceivedSha
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedCircleDeleteModalDelete ->
            case model.maybeModal of
                Just ( CircleDeleteConfirm aCircle, state ) ->
                    let
                        newPattern =
                            Pattern.removeCircle aCircle model.pattern
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( CircleDeleteConfirm aCircle
                                , Ui.Molecule.Modal.Closing
                                )
                        , pattern = newPattern
                      }
                    , Git.putPattern model.identity
                        { repo = model.repo
                        , message = "delete circle"
                        , pattern = newPattern
                        , sha = model.sha
                        , onSha = ReceivedSha
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedCurveDeleteModalDelete ->
            case model.maybeModal of
                Just ( CurveDeleteConfirm aCurve, state ) ->
                    let
                        newPattern =
                            Pattern.removeCurve aCurve model.pattern
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( CurveDeleteConfirm aCurve
                                , Ui.Molecule.Modal.Closing
                                )
                        , pattern = newPattern
                      }
                    , Git.putPattern model.identity
                        { repo = model.repo
                        , message = "delete curve"
                        , pattern = newPattern
                        , sha = model.sha
                        , onSha = ReceivedSha
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedDetailDeleteModalDelete ->
            case model.maybeModal of
                Just ( DetailDeleteConfirm aDetail, state ) ->
                    let
                        newPattern =
                            Pattern.removeDetail aDetail model.pattern
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( DetailDeleteConfirm aDetail
                                , Ui.Molecule.Modal.Closing
                                )
                        , pattern = newPattern
                      }
                    , Git.putPattern model.identity
                        { repo = model.repo
                        , message = "delete detail"
                        , pattern = newPattern
                        , sha = model.sha
                        , onSha = ReceivedSha
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedVariableDeleteModalDelete ->
            case model.maybeModal of
                Just ( VariableDeleteConfirm variable, state ) ->
                    let
                        newPattern =
                            Pattern.removeVariable variable model.pattern
                    in
                    ( { model
                        | maybeModal =
                            Just
                                ( VariableDeleteConfirm variable
                                , Ui.Molecule.Modal.Closing
                                )
                        , pattern = newPattern
                      }
                    , Git.putPattern model.identity
                        { repo = model.repo
                        , message = "delete variable"
                        , pattern = newPattern
                        , sha = model.sha
                        , onSha = ReceivedSha
                        }
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
                    ( { model | maybeModal = Just ( modal, Ui.Molecule.Modal.Closing ) }
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
        RequestingToken _ ->
            Sub.none

        Loading _ ->
            Sub.none

        Error ->
            Sub.none

        Loaded data ->
            Sub.batch
                [ case data.maybeModal of
                    Nothing ->
                        Sub.none

                    Just ( _, state ) ->
                        Sub.map ChangedModalState (Ui.Molecule.Modal.subscriptions state)
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
                , LocalStorage.changedStore
                    { changedZoom = ChangedZoom
                    , changedCenter = ChangedCenter
                    , changedWhatever = ChangedWhatever
                    }
                ]



---- HELPER


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<no name>"
