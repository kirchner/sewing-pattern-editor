module Page.Pattern exposing
    ( Model, init, toSession
    , view
    , Msg, update, subscriptions
    )

{-|

@docs Model, init, toSession
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

import BoundingBox2d
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy as Element
import Github
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse
import Html.Events.Extra.Touch
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Length exposing (Meters)
import List.Extra as List
import LocalStorage
import Pattern
    exposing
        ( A
        , Axis
        , Circle
        , Curve
        , Detail
        , InsertHelp(..)
        , Intersectable(..)
        , Object(..)
        , Orientation(..)
        , Pattern
        , Point
        )
import Pattern.Viewport
import Pixels
import Point2d exposing (Point2d)
import Process
import Quantity
import Route
import Session exposing (Session)
import State
import StateResult
import Task
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Atom.Object exposing (Resolution)
import Ui.Atom.Tabs
import Ui.Molecule.MenuBtn
import Ui.Molecule.Modal
import Ui.Molecule.ObjectList
import Ui.Molecule.Pattern
import Ui.Molecule.VariableList
import Ui.Organism.Dialog
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Vector2d



---- MODEL


{-| -}
type Model
    = Loading LoadingData
    | Error Session
    | Loaded LoadedData


type alias LoadingData =
    { session : Session
    , address : LocalStorage.Address
    , maybePatternData : Maybe (Github.PatternData BottomLeft)
    , maybeMeta : Maybe Github.Meta
    , maybePermissions : Maybe Github.Permissions
    }


type alias LoadedData =
    { session : Session
    , patternContainerDimensions : Maybe Pattern.Viewport.Dimensions
    , maybeModal : Maybe ( Modal, Ui.Molecule.Modal.State )
    , addresses : Maybe (List LocalStorage.Address)

    -- PATTERN
    , drag : Drag
    , address : LocalStorage.Address
    , permissions : Github.Permissions
    , sha : String
    , pattern : Pattern BottomLeft
    , stored : Bool
    , name : String
    , resolution : Resolution
    , center : Point2d Meters BottomLeft
    , patternState : Ui.Molecule.Pattern.State

    -- TOOLBAR TOP
    , topToolbarExpanded : Bool
    , createObjectMenuBtn : Ui.Molecule.MenuBtn.State
    , maybeSlideToolbarTop : Maybe Slide

    -- TOOLBAR BOTTOM
    , toolbarBottomExpanded : Bool
    , toolbarBottomHeight : Maybe Float

    -- DATA
    , selectedTab : Tab
    , focusedVariable : Maybe String
    , hoveredVariable : Maybe String

    -- DIALOG
    , maybeDialog : Maybe Dialog
    , preventActionMenuClose : Bool
    }


type Drag
    = NoDrag
    | DragWithOneTouchPoint
        { identifier : Int
        , start : ( Float, Float )
        , current : ( Float, Float )
        }
    | DragWithTwoTouchPoints
        { identifierA : Int
        , startA : ( Float, Float )
        , currentA : ( Float, Float )
        , identifierB : Int
        , startB : ( Float, Float )
        , currentB : ( Float, Float )
        }
    | DragWithMouse
        { start : ( Float, Float )
        , current : ( Float, Float )
        }


type alias Slide =
    { identifier : Int
    , start : ( Float, Float )
    , current : ( Float, Float )
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
init : Session -> Github.Identity -> LocalStorage.Address -> ( Model, Cmd Msg )
init session identity address =
    ( Loading
        { session = session
        , address = address
        , maybePatternData = Nothing
        , maybeMeta = Nothing
        , maybePermissions = Nothing
        }
    , case address of
        LocalStorage.GithubRepo { repo, ref } ->
            Cmd.batch
                [ Github.getPattern identity
                    { repo = repo
                    , ref = ref
                    , onPatternData = ReceivedPatternData
                    }
                , Github.getMeta identity
                    { repo = repo
                    , ref = ref
                    , onMeta = ReceivedMeta
                    }
                , Github.getPermissions identity
                    { repo = repo
                    , onPermissions = ReceivedPermissions
                    }
                ]

        LocalStorage.Browser _ ->
            Cmd.batch
                [ LocalStorage.requestPattern address
                , LocalStorage.requestMeta address
                ]
    )


initLoaded :
    Session
    -> Element.Device
    -> LocalStorage.Address
    -> String
    -> Pattern BottomLeft
    -> Github.Meta
    -> Github.Permissions
    -> ( Model, Cmd Msg )
initLoaded session device address sha pattern meta permissions =
    ( Loaded
        { session = session
        , patternContainerDimensions = Nothing
        , maybeModal = Nothing
        , addresses = Nothing

        -- PATTERN
        , drag = NoDrag
        , address = address
        , permissions = permissions
        , sha = sha
        , pattern = Pattern.regenerateCaches pattern
        , stored = True
        , name = meta.name
        , resolution = Pixels.pixels 1 |> Quantity.per (Length.meters 1)
        , center = Point2d.origin
        , patternState = Ui.Molecule.Pattern.init

        -- TOOLBAR TOP
        , topToolbarExpanded = False
        , createObjectMenuBtn = Ui.Molecule.MenuBtn.init
        , maybeSlideToolbarTop = Nothing

        -- TOOLBAR BOTTOM
        , toolbarBottomExpanded = False
        , toolbarBottomHeight = Nothing

        -- DATA
        , selectedTab = ObjectsTab
        , focusedVariable = Nothing
        , hoveredVariable = Nothing

        -- DIALOG
        , maybeDialog = Nothing
        , preventActionMenuClose = False
        }
    , Cmd.batch
        [ LocalStorage.requestZoom address
        , LocalStorage.requestCenter address
        , LocalStorage.requestAddresses
        , if isCompact device then
            requestViewportOfToolbarBottom

          else
            Cmd.none
        ]
    )


{-| -}
toSession : Model -> Session
toSession model =
    case model of
        Loading { session } ->
            session

        Error session ->
            session

        Loaded { session } ->
            session



---- VIEW


{-| -}
view :
    Element.Device
    -> Github.Identity
    -> Model
    -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view device identity model =
    case model of
        Loading _ ->
            statusMsg "Loading pattern..." "Loading pattern..."

        Error _ ->
            statusMsg "Something went wrong." "Loading pattern.."

        Loaded data ->
            { title = "Sewing Pattern Editor"
            , body = viewEditor device identity data
            , dialog = Maybe.map (viewModal data.pattern) data.maybeModal
            }


statusMsg : String -> String -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
statusMsg title text =
    { title = title
    , body = Element.el [ Element.centerX, Element.centerY ] (Element.text text)
    , dialog = Nothing
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


viewEditor : Element.Device -> Github.Identity -> LoadedData -> Element Msg
viewEditor device identity model =
    if isCompact device then
        viewEditorCompact identity model

    else
        viewEditorFullScreen identity model


viewEditorCompact : Github.Identity -> LoadedData -> Element Msg
viewEditorCompact identity model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clipY
        , Element.inFront (viewToolbarBottomCompact model)
        ]
        (Element.el
            [ Element.htmlAttribute (Html.Attributes.id "pattern-container")
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.inFront (viewToolbarTopCompact identity model)
            ]
            (Element.lazy3 viewPattern model.patternContainerDimensions (actualCenter model) model)
        )


viewEditorFullScreen : Github.Identity -> LoadedData -> Element Msg
viewEditorFullScreen identity model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewToolbarTopFullscreen identity model
        , Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            ]
            [ viewToolbarLeftFullscreen model
            , viewWorkspaceFullscreen model
            , viewToolbarRightFullscreen model
            ]
        ]



---- TOOLBAR TOP COMPACT


viewToolbarTopCompact : Github.Identity -> LoadedData -> Element Msg
viewToolbarTopCompact identity model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level1
        , Background.color Ui.Theme.Color.secondary
        , Element.htmlAttribute <|
            Html.Attributes.style "transition" "transform 0.2s ease-out 0s"
        , Element.moveUp <|
            case model.maybeSlideToolbarTop of
                Nothing ->
                    if model.topToolbarExpanded then
                        0

                    else
                        toolbarTopHeight

                Just slide ->
                    let
                        deltaY =
                            Tuple.second slide.current - Tuple.second slide.start
                    in
                    if model.topToolbarExpanded then
                        clamp 0 toolbarTopHeight (0 - deltaY)

                    else
                        clamp 0 toolbarTopHeight (toolbarTopHeight - deltaY)
        , Element.htmlAttribute <|
            Html.Events.Extra.Touch.onWithOptions "touchstart"
                { stopPropagation = False, preventDefault = False }
                UserStartedTouchOnToolbarTop
        , Element.htmlAttribute <|
            Html.Events.Extra.Touch.onWithOptions "touchmove"
                { stopPropagation = False, preventDefault = False }
                UserMovedTouchOnToolbarTop
        , Element.htmlAttribute <|
            Html.Events.Extra.Touch.onWithOptions "touchend"
                { stopPropagation = False, preventDefault = False }
                UserEndedTouchOnToolbarTop
        , Element.htmlAttribute <|
            Html.Events.Extra.Touch.onWithOptions "touchcancel"
                { stopPropagation = False, preventDefault = False }
                UserCancelledTouchOnToolbarTop
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el [ Element.alignLeft ] patternActions
            , Element.el [ Element.alignRight ] (signInViaGithubBtn identity)
            ]
        , Element.el [ Element.centerX ] (patternAddress model.address)
        , Element.row
            [ Element.width Element.fill ]
            [ Element.el [ Element.padding Ui.Theme.Spacing.level1 ]
                (backToPatternsLink { showLabel = False })
            , Element.row
                [ Element.centerX
                , Element.spacing Ui.Theme.Spacing.level2
                ]
                [ patternName model.name
                , loadingIndicator model.stored
                ]
            ]
        , Element.row
            [ Element.width Element.fill ]
            [ if model.permissions.push then
                viewToolButtons False model

              else
                Element.none
            , Element.el
                [ Element.alignRight ]
                viewZoomControls
            ]
        ]



---- TOOLBAR BOTTOM COMPACT


viewToolbarBottomCompact : LoadedData -> Element Msg
viewToolbarBottomCompact model =
    let
        verticalOffset =
            if model.toolbarBottomExpanded then
                0

            else
                case model.toolbarBottomHeight of
                    Nothing ->
                        0

                    Just height ->
                        height - 38
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.moveDown verticalOffset
        , Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")
        , Element.htmlAttribute (Html.Attributes.style "transition" "transform 0.2s ease-out 0s")
        ]
        [ Element.el
            [ Element.height (Element.fillPortion 1)
            , Element.width Element.fill
            ]
            Element.none
        , Element.el
            [ Element.htmlAttribute (Html.Attributes.id "toolbar-bottom")
            , Element.height
                (Element.fillPortion 1
                    |> Element.maximum 360
                )
            , Element.width Element.fill
            , Border.widthEach
                { top = 1
                , bottom = 0
                , left = 0
                , right = 0
                }
            , Border.color Ui.Theme.Color.secondary
            , Background.color Ui.Theme.Color.white
            , Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")
            , Element.above <|
                Element.el
                    [ Element.width Element.fill
                    , Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")
                    ]
                    (viewSelectionCompact model)
            ]
            (Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.clip
                , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
                , Element.spacing Ui.Theme.Spacing.level1
                , Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")
                ]
                [ Element.row
                    [ Element.width Element.fill
                    , Element.padding Ui.Theme.Spacing.level1
                    ]
                    [ Element.el
                        [ Element.alignRight ]
                        (Element.row []
                            [ Ui.Atom.Input.btnIcon
                                { id = "toggle-btn"
                                , onPress = Just UserPressedLeftToolbarToggleBtn
                                , icon =
                                    if model.toolbarBottomExpanded then
                                        "chevron-down"

                                    else
                                        "chevron-up"
                                }
                            ]
                        )
                    ]
                , case model.maybeDialog of
                    Nothing ->
                        viewData model

                    Just dialog ->
                        Element.el
                            [ Element.height Element.fill
                            , Element.width Element.fill
                            , Element.scrollbarY
                            ]
                            (viewDialog model.pattern dialog)
                ]
            )
        ]



---- TOOLBAR TOP FULLSCREEN


viewToolbarTopFullscreen : Github.Identity -> LoadedData -> Element Msg
viewToolbarTopFullscreen identity model =
    Element.row
        [ Element.width Element.fill
        , Element.padding (Ui.Theme.Spacing.level1 // 2)
        , Background.color Ui.Theme.Color.secondary
        , Element.behindContent <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (patternName model.name)
                )
        ]
        [ Element.el [ Element.paddingXY Ui.Theme.Spacing.level2 0 ] <|
            backToPatternsLink { showLabel = True }
        , Element.row
            [ Element.paddingXY Ui.Theme.Spacing.level4 0
            , Element.spacing Ui.Theme.Spacing.level2
            , Font.color Ui.Theme.Color.grayDark
            , Element.alignRight
            ]
            [ patternAddress model.address
            , loadingIndicator model.stored
            , patternActions
            , signInViaGithubBtn identity
            ]
        ]



---- TOOLBAR TOP PARTS


backToPatternsLink : { showLabel : Bool } -> Element msg
backToPatternsLink { showLabel } =
    Ui.Theme.Focus.outline <|
        Element.link
            [ Font.color Ui.Theme.Color.primary
            , Element.mouseOver
                [ Font.color Ui.Theme.Color.primaryDark ]
            ]
            { url = "/"
            , label =
                Element.row
                    [ Element.spacing Ui.Theme.Spacing.level1 ]
                    [ Ui.Atom.Icon.fa "arrow-left"
                    , if showLabel then
                        Ui.Theme.Typography.body "Back to patterns"

                      else
                        Element.none
                    ]
            }


patternName : String -> Element msg
patternName name =
    Ui.Theme.Typography.bodyBold name


loadingIndicator : Bool -> Element msg
loadingIndicator stored =
    Ui.Atom.Icon.fa <|
        if stored then
            "check-circle"

        else
            "arrow-alt-circle-up"


patternAddress : LocalStorage.Address -> Element msg
patternAddress address =
    Element.row
        [ Element.spacing Ui.Theme.Spacing.level1 ]
        (List.map Ui.Theme.Typography.button <|
            List.intersperse "/" (LocalStorage.addressToPathSegments address)
        )


patternActions : Element Msg
patternActions =
    Element.row
        []
        [ Ui.Atom.Input.btnSecondaryBorderedLeft
            { id = "download-btn"
            , onPress = Nothing
            , label = "Download"
            }
        , Ui.Atom.Input.btnSecondaryBorderedRight
            { id = "project-btn"
            , onPress = Just UserPressedProject
            , label = "Project"
            }
        ]


signInViaGithubBtn : Github.Identity -> Element Msg
signInViaGithubBtn identity =
    case identity of
        Github.Anonymous ->
            Element.el [] <|
                Ui.Atom.Input.btnPrimary
                    { id = "sign-in-btn"
                    , onPress = Just UserPressedSignIn
                    , label = "Sign in via GitHub"
                    }

        Github.OauthToken _ ->
            Element.none



---- TOOLBAR LEFT FULLSCREEN


viewToolbarLeftFullscreen : LoadedData -> Element Msg
viewToolbarLeftFullscreen model =
    Element.column
        [ Element.width (Element.px 400)
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Element.spacing Ui.Theme.Spacing.level1
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.padding Ui.Theme.Spacing.level1
            ]
            [ if model.permissions.push then
                viewToolButtons False model

              else
                Element.none
            , Element.el [ Element.alignRight ] viewZoomControls
            ]
        , viewData model
        ]



---- TOOLBAR RIGHT FULLSCREEN


viewToolbarRightFullscreen : LoadedData -> Element Msg
viewToolbarRightFullscreen model =
    Element.el
        [ Element.width (Element.maximum 500 Element.fill)
        , Element.height Element.fill
        , Element.scrollbarY
        , Element.padding Ui.Theme.Spacing.level1
        , Background.color Ui.Theme.Color.white
        , Border.widthEach
            { top = 1
            , bottom = 4
            , left = 1
            , right = 1
            }
        , Border.color Ui.Theme.Color.secondaryDark
        ]
        (model.maybeDialog
            |> Maybe.map (viewDialog model.pattern)
            |> Maybe.withDefault (viewSelectionFullscreen model)
        )



---- SELECTION


viewSelectionFullscreen : LoadedData -> Element Msg
viewSelectionFullscreen model =
    if List.isEmpty model.patternState.selectedObjects then
        Element.none

    else
        Element.column
            [ Element.width Element.fill ]
            [ Element.Keyed.column
                [ Element.width Element.fill
                , Element.spacing Ui.Theme.Spacing.level1
                , Element.padding Ui.Theme.Spacing.level1
                ]
                (List.indexedMap (viewSelectedObject (List.length model.patternState.selectedObjects))
                    model.patternState.selectedObjects
                )
            , viewShortcutsFullscreen model
            ]


viewSelectionCompact : LoadedData -> Element Msg
viewSelectionCompact model =
    if List.isEmpty model.patternState.selectedObjects then
        Element.none

    else
        Element.column
            [ Element.width Element.fill
            , Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")
            ]
            [ viewShortcutsCompact model
            , Element.Keyed.column
                [ Element.width Element.fill
                , Element.spacing Ui.Theme.Spacing.level1
                , Element.padding Ui.Theme.Spacing.level1
                , Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")
                ]
                (List.indexedMap (viewSelectedObject (List.length model.patternState.selectedObjects))
                    model.patternState.selectedObjects
                )
            ]


viewSelectedObject : Int -> Int -> Object -> ( String, Element Msg )
viewSelectedObject count index object =
    let
        controls =
            Element.column
                [ Element.height Element.fill
                , Background.color Ui.Theme.Color.primary
                , Font.color Ui.Theme.Color.white
                ]
                [ if index == 0 then
                    Element.el
                        [ Element.height (Element.px 27)
                        , Element.width (Element.px 27)
                        ]
                        Element.none

                  else
                    Ui.Atom.Input.btnIcon
                        { id = "selected-object-" ++ String.fromInt index ++ "--move-up-btn"
                        , onPress = Just (UserPressedSelectedObjectMoveUp index)
                        , icon = "chevron-up"
                        }
                , if index == count - 1 then
                    Element.el
                        [ Element.height (Element.px 27)
                        , Element.width (Element.px 27)
                        ]
                        Element.none

                  else
                    Ui.Atom.Input.btnIcon
                        { id = "selected-object-" ++ String.fromInt index ++ "--move-up-down"
                        , onPress = Just (UserPressedSelectedObjectMoveDown index)
                        , icon = "chevron-down"
                        }
                ]

        content =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding Ui.Theme.Spacing.level2
                ]
                [ Element.row
                    [ Element.width Element.fill
                    , Element.spacing Ui.Theme.Spacing.level1
                    ]
                    [ case object of
                        Point _ ->
                            Ui.Atom.Icon.point

                        Axis _ ->
                            Ui.Atom.Icon.axis

                        Circle _ ->
                            Ui.Atom.Icon.circle

                        Curve _ ->
                            Ui.Atom.Icon.curve

                        Detail _ ->
                            Ui.Atom.Icon.detail
                    , Ui.Theme.Typography.bodyBold name
                    ]
                ]

        actions =
            Element.column
                [ Element.height Element.fill
                ]
                [ Element.el
                    [ Element.height (Element.px 27)
                    , Element.width (Element.px 27)
                    ]
                    (Ui.Atom.Input.btnIcon
                        { id = "selected-object-" ++ String.fromInt index ++ "--unselect-btn"
                        , onPress = Just (UserPressedSelectedObjectUnselect index)
                        , icon = "times"
                        }
                    )
                , Element.el
                    [ Element.centerX
                    , Element.alignBottom
                    , Element.padding Ui.Theme.Spacing.level1
                    ]
                    (Ui.Theme.Typography.button (String.fromInt (index + 1)))
                ]

        name =
            case object of
                Point aPoint ->
                    objectName aPoint

                Axis aAxis ->
                    objectName aAxis

                Circle aCircle ->
                    objectName aCircle

                Curve aCurve ->
                    objectName aCurve

                Detail aDetail ->
                    objectName aDetail
    in
    ( name
    , Element.row
        [ Element.width Element.fill
        , Background.color Ui.Theme.Color.white
        , Border.width 1
        , Border.rounded 3
        , Border.color Ui.Theme.Color.primary
        ]
        [ controls
        , content
        , actions
        ]
    )



---- SHORTCUTS


viewShortcutsFullscreen : LoadedData -> Element Msg
viewShortcutsFullscreen model =
    case model.patternState.selectedObjects of
        (Point aPointA) :: (Point aPointB) :: [] ->
            Element.wrappedRow
                [ Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")
                ]
                [ newIntersection 120 model True aPointA aPointB
                , newIntersection 120 model False aPointA aPointB
                ]

        _ ->
            Element.none


viewShortcutsCompact : LoadedData -> Element Msg
viewShortcutsCompact model =
    case model.patternState.selectedObjects of
        (Point aPointA) :: (Point aPointB) :: [] ->
            Element.column
                [ Element.alignRight
                , Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")
                ]
                [ newIntersection 80 model True aPointA aPointB
                , newIntersection 80 model False aPointA aPointB
                ]

        _ ->
            Element.none


newIntersection : Float -> LoadedData -> Bool -> A Point -> A Point -> Element Msg
newIntersection pixelWidth model flipped aPointA aPointB =
    Result.withDefault Element.none <|
        State.finalValue model.pattern <|
            StateResult.map2
                (\point2dA point2dB ->
                    let
                        xA =
                            Point2d.xCoordinate point2dA

                        xB =
                            Point2d.xCoordinate point2dB

                        yA =
                            Point2d.yCoordinate point2dA

                        yB =
                            Point2d.yCoordinate point2dB
                    in
                    if
                        Quantity.equalWithin (Length.millimeters 1) xA xB
                            || Quantity.equalWithin (Length.millimeters 1) yA yB
                    then
                        Element.none

                    else
                        Result.withDefault Element.none <|
                            Result.map2
                                (\axisA axisB ->
                                    case
                                        Pattern.intersection
                                            (IntersectableAxis (Pattern.this axisA))
                                            (IntersectableAxis (Pattern.this axisB))
                                            0
                                            model.pattern
                                    of
                                        Err _ ->
                                            Element.none

                                        Ok pointNew ->
                                            viewPointShortcut pixelWidth
                                                model
                                                [ point2dA, point2dB ]
                                                pointNew
                                )
                                (Pattern.throughOnePoint aPointA
                                    (if flipped then
                                        Horizontal

                                     else
                                        Vertical
                                    )
                                    model.pattern
                                )
                                (Pattern.throughOnePoint aPointB
                                    (if flipped then
                                        Vertical

                                     else
                                        Horizontal
                                    )
                                    model.pattern
                                )
                )
                (Pattern.point2d aPointA)
                (Pattern.point2d aPointB)


viewPointShortcut : Float -> LoadedData -> List (Point2d Meters BottomLeft) -> Point -> Element Msg
viewPointShortcut pixelWidth model point2ds pointNew =
    case Pattern.insertAndReturnPoint "" pointNew model.pattern of
        Err _ ->
            Element.none

        Ok ( aPoint, newPattern ) ->
            case State.finalValue newPattern (Pattern.point2d aPoint) of
                Err _ ->
                    Element.none

                Ok point2d ->
                    let
                        { resolution, center } =
                            Pattern.Viewport.idealForPoints dimensions point2d point2ds

                        dimensions =
                            { width = Pixels.pixels (pixelWidth - 20)
                            , height = Pixels.pixels (pixelHeight - 20)
                            }

                        pixelHeight =
                            pixelWidth
                    in
                    Ui.Theme.Focus.outline <|
                        Input.button
                            [ Element.htmlAttribute (Html.Attributes.id "new-point-shortcut")
                            , Border.color Ui.Theme.Color.primaryLight
                            , Border.width 1
                            , Border.rounded 3
                            , Background.color Ui.Theme.Color.white
                            , Element.mouseOver
                                [ Border.color Ui.Theme.Color.primary
                                , Background.color Ui.Theme.Color.secondary
                                ]
                            ]
                            { onPress = Just (UserPressedPointShortcut pointNew)
                            , label =
                                Ui.Molecule.Pattern.viewStatic
                                    { id = "pattern" }
                                    { width = pixelWidth
                                    , height = pixelHeight
                                    , resolution = resolution
                                    , center = center
                                    }
                                    newPattern
                                    { hoveredObject = Nothing
                                    , focusedObject = Nothing
                                    , selectedObjects = [ Point aPoint ]
                                    }
                            }



---- WORKSPACE FULLSCREEN


viewWorkspaceFullscreen : LoadedData -> Element Msg
viewWorkspaceFullscreen model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
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
            (Element.lazy3 viewPattern model.patternContainerDimensions (actualCenter model) model)
        )


viewPattern : Maybe Pattern.Viewport.Dimensions -> Point2d Meters BottomLeft -> LoadedData -> Element Msg
viewPattern maybeDimensions center model =
    case maybeDimensions of
        Nothing ->
            Element.none

        Just { width, height } ->
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.htmlAttribute <|
                    Html.Events.preventDefaultOn "dragstart" (Decode.succeed ( NoOp, True ))
                , Element.htmlAttribute <|
                    Html.Events.Extra.Touch.onWithOptions "touchstart"
                        { stopPropagation = False, preventDefault = False }
                        UserStartedTouchOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Touch.onWithOptions "touchmove"
                        { stopPropagation = False, preventDefault = False }
                        UserMovedTouchOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Touch.onWithOptions "touchend"
                        { stopPropagation = False, preventDefault = False }
                        UserEndedTouchOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Touch.onWithOptions "touchcancel"
                        { stopPropagation = False, preventDefault = False }
                        UserCancelledTouchOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Mouse.onWithOptions "mousedown"
                        { stopPropagation = False, preventDefault = False }
                        UserDownedMouseOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Mouse.onWithOptions "mousemove"
                        { stopPropagation = False, preventDefault = False }
                        UserMovedMouseOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Mouse.onWithOptions "mouseup"
                        { stopPropagation = False, preventDefault = False }
                        UserUppedMouseOnPattern
                , Element.htmlAttribute <|
                    Html.Events.Extra.Mouse.onWithOptions "mouseleave"
                        { stopPropagation = False, preventDefault = False }
                        UserLeftMouseOnPattern
                ]
                (Element.map PatternMsg <|
                    Ui.Molecule.Pattern.view
                        { id = "pattern" }
                        { width = Pixels.inPixels width
                        , height = Pixels.inPixels height
                        , resolution = model.resolution
                        , center = center
                        }
                        model.pattern
                        model.patternState
                )


viewToolButtons : Bool -> LoadedData -> Element Msg
viewToolButtons openUpwards model =
    Element.row
        []
        [ viewCreateMenuBtn openUpwards model
        , viewCreateVariableBtn
        ]



---- CREATE MENU BTN


viewCreateMenuBtn : Bool -> LoadedData -> Element Msg
viewCreateMenuBtn openUpwards model =
    Ui.Molecule.MenuBtn.viewPrimary
        { id = "create-object"
        , onMsg = CreateObjectMenuBtnMsg
        , openUpwards = openUpwards
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



---- VIEW CREATE VARIABLE BTN


viewCreateVariableBtn : Element Msg
viewCreateVariableBtn =
    Element.el [] <|
        Ui.Theme.Focus.outline <|
            Input.button
                [ Element.htmlAttribute (Html.Attributes.id "create-variable-btn")
                , Element.height (Element.px 38)
                , Element.width (Element.px 38)
                , Element.centerX
                , Element.centerY
                , Border.rounded 3
                , Font.color Ui.Theme.Color.white
                , Background.color Ui.Theme.Color.primaryLight
                , Element.mouseOver [ Background.color Ui.Theme.Color.primary ]
                ]
                { onPress = Just UserPressedCreateVariable
                , label =
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Ui.Atom.Icon.fa "ruler-horizontal")
                }



---- ZOOM CONTROLS


viewZoomControls : Element Msg
viewZoomControls =
    Element.row []
        [ Ui.Theme.Focus.outlineLeft <|
            Input.button
                [ Element.htmlAttribute (Html.Attributes.id "zoom-plus-btn")
                , Element.height (Element.px 38)
                , Element.width (Element.px 38)
                , Element.centerX
                , Element.centerY
                , Border.roundEach
                    { topLeft = 3
                    , topRight = 0
                    , bottomLeft = 3
                    , bottomRight = 0
                    }
                , Background.color Ui.Theme.Color.secondary
                , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
                ]
                { onPress = Just UserPressedZoomPlus
                , label =
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Ui.Atom.Icon.fa "search-plus")
                }
        , Ui.Theme.Focus.outlineTopBottom <|
            Input.button
                [ Element.htmlAttribute (Html.Attributes.id "zoom-fit-btn")
                , Element.height (Element.px 38)
                , Element.width (Element.px 38)
                , Element.centerX
                , Element.centerY
                , Background.color Ui.Theme.Color.secondary
                , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
                ]
                { onPress = Just UserPressedZoomFit
                , label =
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Ui.Atom.Icon.fa "search")
                }
        , Ui.Theme.Focus.outlineRight <|
            Input.button
                [ Element.htmlAttribute (Html.Attributes.id "zoom-minus-btn")
                , Element.height (Element.px 38)
                , Element.width (Element.px 38)
                , Element.centerX
                , Element.centerY
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 3
                    , bottomLeft = 0
                    , bottomRight = 3
                    }
                , Background.color Ui.Theme.Color.secondary
                , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
                ]
                { onPress = Just UserPressedZoomMinus
                , label =
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Ui.Atom.Icon.fa "search-minus")
                }
        ]



---- DATA


viewData : LoadedData -> Element Msg
viewData model =
    Ui.Atom.Tabs.view
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
                            , editable = model.permissions.push
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
                            , editable = model.permissions.push
                            }
                            model.pattern
                            model.focusedVariable
                            model.hoveredVariable
        }



---- DIALOG


viewDialog : Pattern BottomLeft -> Dialog -> Element Msg
viewDialog pattern dialog =
    case dialog of
        CreateVariable stuff ->
            viewVariable stuff.name stuff.value

        EditVariable stuff ->
            viewEditVariable stuff.name stuff.value

        CreateObject stuff ->
            Element.map DialogCreateMsg <|
                Ui.Organism.Dialog.createView
                    { pattern = pattern
                    , hoveredInCanvas = Nothing
                    }
                    stuff

        EditObject name stuff ->
            Element.map DialogEditMsg <|
                Ui.Organism.Dialog.editView
                    { pattern = pattern
                    , name = name
                    , hoveredInCanvas = Nothing
                    }
                    stuff



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
    | ReceivedPatternData (Result Http.Error (Github.PatternData BottomLeft))
    | ReceivedSha (Result Http.Error String)
    | ReceivedMeta (Result Http.Error Github.Meta)
    | ReceivedPermissions (Result Http.Error Github.Permissions)
    | ReceivedPatternUpdate (Result Http.Error ())
      -- LOCAL STORAGE
    | ChangedZoom LocalStorage.Address Float
    | ChangedCenter LocalStorage.Address (Point2d Meters BottomLeft)
    | ChangedAddresses (List LocalStorage.Address)
    | ChangedPattern LocalStorage.Address (Pattern BottomLeft)
    | ChangedMeta LocalStorage.Address Github.Meta
    | ChangedWhatever
      -- TOP TOOLBAR
    | CreateObjectMenuBtnMsg (Ui.Molecule.MenuBtn.Msg CreateAction)
    | UserPressedProject
    | UserPressedSignIn
    | UserStartedTouchOnToolbarTop Html.Events.Extra.Touch.Event
    | UserMovedTouchOnToolbarTop Html.Events.Extra.Touch.Event
    | UserEndedTouchOnToolbarTop Html.Events.Extra.Touch.Event
    | UserCancelledTouchOnToolbarTop Html.Events.Extra.Touch.Event
      -- LEFT TOOLBAR
    | UserPressedLeftToolbarToggleBtn
    | UserSelectedTab Tab String
    | ReceivedToolbarBottomViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
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
    | UserPressedZoomFit
    | UserPressedZoomMinus
    | UserStartedTouchOnPattern Html.Events.Extra.Touch.Event
    | UserMovedTouchOnPattern Html.Events.Extra.Touch.Event
    | UserEndedTouchOnPattern Html.Events.Extra.Touch.Event
    | UserCancelledTouchOnPattern Html.Events.Extra.Touch.Event
    | UserDownedMouseOnPattern Html.Events.Extra.Mouse.Event
    | UserMovedMouseOnPattern Html.Events.Extra.Mouse.Event
    | UserUppedMouseOnPattern Html.Events.Extra.Mouse.Event
    | UserLeftMouseOnPattern Html.Events.Extra.Mouse.Event
      -- RIGHT TOOLBAR
    | DialogCreateMsg Ui.Organism.Dialog.CreateMsg
    | DialogEditMsg Ui.Organism.Dialog.EditMsg
    | UserPressedSelectedObjectMoveUp Int
    | UserPressedSelectedObjectMoveDown Int
    | UserPressedSelectedObjectUnselect Int
    | UserPressedPointShortcut Point
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
update : String -> Element.Device -> Github.Identity -> Msg -> Model -> ( Model, Cmd Msg )
update clientId device identity msg model =
    case model of
        Loading data ->
            case updateLoading msg data of
                (Loading newData) as newModel ->
                    case ( newData.maybePatternData, newData.maybeMeta, newData.maybePermissions ) of
                        ( Just patternData, Just meta, Just permissions ) ->
                            initLoaded newData.session
                                device
                                newData.address
                                patternData.sha
                                patternData.pattern
                                meta
                                permissions

                        _ ->
                            ( newModel, Cmd.none )

                newModel ->
                    ( newModel, Cmd.none )

        Error _ ->
            ( model, Cmd.none )

        Loaded data ->
            updateLoaded clientId device identity msg data
                |> Tuple.mapFirst Loaded


updateLoading : Msg -> LoadingData -> Model
updateLoading msg data =
    case msg of
        ReceivedPatternData result ->
            case result of
                Err _ ->
                    Error data.session

                Ok patternData ->
                    Loading { data | maybePatternData = Just patternData }

        ReceivedMeta result ->
            case result of
                Err _ ->
                    Error data.session

                Ok meta ->
                    Loading { data | maybeMeta = Just meta }

        ReceivedPermissions result ->
            case result of
                Err _ ->
                    Error data.session

                Ok permissions ->
                    Loading { data | maybePermissions = Just permissions }

        ChangedPattern address pattern ->
            if data.address == address then
                Loading
                    { data
                        | maybePatternData =
                            Just
                                { pattern = Pattern.regenerateCaches pattern
                                , sha = ""
                                }
                    }

            else
                Loading data

        ChangedMeta address meta ->
            if data.address == address then
                Loading
                    { data
                        | maybeMeta = Just meta
                        , maybePermissions =
                            Just
                                { admin = True
                                , push = True
                                , pull = True
                                }
                    }

            else
                Loading data

        _ ->
            Loading data


updateLoaded : String -> Element.Device -> Github.Identity -> Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateLoaded clientId device identity msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedGithubAccessToken _ ->
            ( model, Cmd.none )

        ReceivedPatternData result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok patternData ->
                    ( { model
                        | sha = patternData.sha
                        , pattern = Pattern.regenerateCaches patternData.pattern
                      }
                    , Cmd.none
                    )

        ReceivedSha result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok newSha ->
                    ( { model
                        | sha = newSha
                        , stored = True
                      }
                    , Cmd.none
                    )

        ReceivedPatternUpdate _ ->
            ( model
            , Cmd.none
            )

        ReceivedMeta result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok meta ->
                    ( { model | name = meta.name }
                    , Cmd.none
                    )

        ReceivedPermissions result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok newPermissions ->
                    ( { model | permissions = newPermissions }
                    , Cmd.none
                    )

        -- LOCAL STORAGE
        ChangedZoom address zoom ->
            if model.address == address then
                ( { model | resolution = Pixels.pixels zoom |> Quantity.per (Length.meters 1) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ChangedCenter address center ->
            ( if model.address == address then
                { model | center = center }

              else
                model
            , Cmd.none
            )

        ChangedAddresses addresses ->
            let
                newAddresses =
                    List.uniqueBy addressToHash (model.address :: addresses)

                addressToHash address =
                    case address of
                        LocalStorage.GithubRepo { repo } ->
                            "github/" ++ repo.owner ++ "/" ++ repo.name

                        LocalStorage.Browser { slug } ->
                            "browser/" ++ slug
            in
            ( { model | addresses = Just newAddresses }
            , if addresses /= newAddresses then
                LocalStorage.updateAddresses newAddresses

              else
                Cmd.none
            )

        ChangedPattern _ _ ->
            ( model, Cmd.none )

        ChangedMeta _ _ ->
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
                , toolbarBottomExpanded = maybeAction /= Nothing
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
            , Cmd.batch
                [ Cmd.map CreateObjectMenuBtnMsg menuBtnCmd
                , if maybeAction /= Nothing && isCompact device then
                    requestViewportOfToolbarBottom

                  else
                    Cmd.none
                ]
            )

        UserPressedProject ->
            ( model
            , Browser.Navigation.load (Route.absolute (Route.Details model.address) [])
            )

        UserPressedSignIn ->
            ( model
            , Github.requestAuthorization clientId <|
                Route.crossOrigin (Session.domain model.session) (Route.Pattern model.address) []
            )

        UserStartedTouchOnToolbarTop event ->
            ( startSlideToolbarTop event model
            , Cmd.none
            )

        UserMovedTouchOnToolbarTop event ->
            ( model.maybeSlideToolbarTop
                |> Maybe.map (updateSlideToolbarTop event model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        UserEndedTouchOnToolbarTop event ->
            ( model.maybeSlideToolbarTop
                |> Maybe.map (endSlideToolbarTop event model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        UserCancelledTouchOnToolbarTop event ->
            ( model.maybeSlideToolbarTop
                |> Maybe.map (endSlideToolbarTop event model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        -- LEFT TOOLBAR
        UserPressedLeftToolbarToggleBtn ->
            ( { model | toolbarBottomExpanded = not model.toolbarBottomExpanded }
            , Cmd.none
            )

        UserSelectedTab tab id ->
            ( { model | selectedTab = tab }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)
            )

        ReceivedToolbarBottomViewport result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok viewport ->
                    ( { model | toolbarBottomHeight = Just viewport.viewport.height }
                    , Cmd.none
                    )

        -- LEFT TOOLBAR OBJECTS
        ObjectListMsg objectListMsg ->
            ( { model
                | patternState =
                    Ui.Molecule.ObjectList.update objectListMsg model.pattern model.patternState
              }
            , Cmd.none
            )

        UserPressedHideObject _ ->
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
            , if isCompact device then
                requestViewportOfToolbarBottom

              else
                Cmd.none
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
            , focusNameInput
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
                , toolbarBottomExpanded = True
              }
            , if isCompact device then
                Cmd.batch
                    [ requestViewportOfToolbarBottom
                    , focusNameInput
                    ]

              else
                focusNameInput
            )

        -- PATTERN
        PatternMsg patternMsg ->
            ( { model
                | patternState =
                    Ui.Molecule.Pattern.update patternMsg
                        model.pattern
                        (dragging model)
                        model.patternState
              }
            , Cmd.none
            )

        UserResizedWindow ->
            ( model
            , if isCompact device then
                Cmd.batch
                    [ requestViewportOfToolbarBottom
                    , requestViewportOfPattern 0
                    ]

              else
                requestViewportOfPattern 0
            )

        AnimationFrameRequestedPatternContainerViewport ->
            ( model
            , requestViewportOfPattern 500
            )

        UpdateRequestedPatternContainerViewport result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok viewport ->
                    ( { model
                        | patternContainerDimensions =
                            Just
                                { width = Pixels.pixels viewport.viewport.width
                                , height = Pixels.pixels viewport.viewport.height
                                }
                      }
                    , Cmd.none
                    )

        UserPressedZoomPlus ->
            ( model
            , updateZoom model.address (Quantity.multiplyBy 1.1 model.resolution)
            )

        UserPressedZoomFit ->
            case model.patternContainerDimensions of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just dimensions ->
                    ( model
                    , updateToIdealViewport model.address model.pattern dimensions
                    )

        UserPressedZoomMinus ->
            ( model
            , updateZoom model.address (Quantity.divideBy 1.1 model.resolution)
            )

        UserStartedTouchOnPattern event ->
            ( case event.touches of
                touch :: [] ->
                    { model
                        | drag =
                            DragWithOneTouchPoint
                                { identifier = touch.identifier
                                , start = touch.screenPos
                                , current = touch.screenPos
                                }
                    }

                _ ->
                    model
            , Cmd.none
            )

        UserMovedTouchOnPattern event ->
            ( case model.drag of
                NoDrag ->
                    model

                DragWithOneTouchPoint stuff ->
                    case event.touches of
                        touch :: [] ->
                            if touch.identifier == stuff.identifier then
                                { model
                                    | drag =
                                        DragWithOneTouchPoint
                                            { stuff | current = touch.screenPos }
                                }

                            else
                                model

                        _ ->
                            model

                DragWithTwoTouchPoints _ ->
                    -- TODO implement properly
                    model

                DragWithMouse _ ->
                    model
            , Cmd.none
            )

        UserEndedTouchOnPattern event ->
            endSlidePattern event model

        UserCancelledTouchOnPattern event ->
            endSlidePattern event model

        UserDownedMouseOnPattern event ->
            ( { model
                | drag =
                    DragWithMouse
                        { start = event.screenPos
                        , current = event.screenPos
                        }
              }
            , Cmd.none
            )

        UserMovedMouseOnPattern event ->
            ( case model.drag of
                NoDrag ->
                    model

                DragWithOneTouchPoint _ ->
                    model

                DragWithTwoTouchPoints _ ->
                    model

                DragWithMouse stuff ->
                    { model | drag = DragWithMouse { stuff | current = event.screenPos } }
            , Cmd.none
            )

        UserUppedMouseOnPattern event ->
            endDragWithMouse event model

        UserLeftMouseOnPattern event ->
            endDragWithMouse event model

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
                                , stored = False
                              }
                            , if isCompact device then
                                Cmd.batch
                                    [ requestViewportOfToolbarBottom
                                    , putPattern identity
                                        model.address
                                        model.sha
                                        "create object"
                                        newPattern
                                    ]

                              else
                                putPattern identity model.address model.sha "create object" newPattern
                            )

                        Ui.Organism.Dialog.CreateCanceled ->
                            ( { model | maybeDialog = Nothing }
                            , if isCompact device then
                                requestViewportOfToolbarBottom

                              else
                                Cmd.none
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

                Just (CreateObject _) ->
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
                                , stored = False
                              }
                            , if isCompact device then
                                Cmd.batch
                                    [ requestViewportOfToolbarBottom
                                    , putPattern identity model.address model.sha "edit object" newPattern
                                    ]

                              else
                                putPattern identity model.address model.sha "edit object" newPattern
                            )

                        Ui.Organism.Dialog.EditCanceled ->
                            ( { model | maybeDialog = Nothing }
                            , if isCompact device then
                                requestViewportOfToolbarBottom

                              else
                                Cmd.none
                            )

                Just (CreateVariable _) ->
                    ( model, Cmd.none )

                Just (EditVariable _) ->
                    ( model, Cmd.none )

        UserPressedSelectedObjectMoveUp index ->
            let
                { patternState } =
                    model
            in
            ( { model
                | patternState =
                    { patternState | selectedObjects = List.swapAt index (index - 1) patternState.selectedObjects }
              }
            , ("selected-object-" ++ String.fromInt (index - 1) ++ "--move-up-btn")
                |> Browser.Dom.focus
                |> Task.attempt (\_ -> NoOp)
            )

        UserPressedSelectedObjectMoveDown index ->
            let
                { patternState } =
                    model
            in
            ( { model
                | patternState =
                    { patternState | selectedObjects = List.swapAt index (index + 1) patternState.selectedObjects }
              }
            , ("selected-object-" ++ String.fromInt (index + 1) ++ "--move-down-btn")
                |> Browser.Dom.focus
                |> Task.attempt (\_ -> NoOp)
            )

        UserPressedSelectedObjectUnselect index ->
            let
                { patternState } =
                    model
            in
            ( { model
                | patternState =
                    { patternState | selectedObjects = List.removeAt index patternState.selectedObjects }
              }
            , Cmd.none
            )

        UserPressedPointShortcut newPoint ->
            let
                maybeDialog =
                    Ui.Organism.Dialog.createPointWith model.pattern newPoint

                { patternState } =
                    model
            in
            ( { model
                | toolbarBottomExpanded = maybeDialog /= Nothing
                , maybeDialog = Maybe.map CreateObject maybeDialog
                , patternState = { patternState | selectedObjects = [] }
              }
            , if maybeDialog /= Nothing && isCompact device then
                requestViewportOfToolbarBottom

              else
                Cmd.none
            )

        -- VARIABLE DIALOG
        UserChangedVariableName newName ->
            case model.maybeDialog of
                Just (CreateVariable data) ->
                    ( { model | maybeDialog = Just (CreateVariable { data | name = newName }) }
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
                    ( { model | maybeDialog = Just (CreateVariable { data | value = newValue }) }
                    , Cmd.none
                    )

                Just (EditVariable data) ->
                    ( { model | maybeDialog = Just (EditVariable { data | value = newValue }) }
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
                                , stored = False
                              }
                            , if isCompact device then
                                Cmd.batch
                                    [ requestViewportOfToolbarBottom
                                    , putPattern identity
                                        model.address
                                        model.sha
                                        "create variable"
                                        newPattern
                                    ]

                              else
                                putPattern identity model.address model.sha "create variable" newPattern
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
                                , stored = False
                              }
                            , if isCompact device then
                                Cmd.batch
                                    [ requestViewportOfToolbarBottom
                                    , putPattern identity
                                        model.address
                                        model.sha
                                        "edit variable"
                                        newPattern
                                    ]

                              else
                                putPattern identity model.address model.sha "edit variable" newPattern
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
                Just ( PointDeleteConfirm aPoint, _ ) ->
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
                        , stored = False
                      }
                    , putPattern identity model.address model.sha "delete point" newPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedAxisDeleteModalDelete ->
            case model.maybeModal of
                Just ( AxisDeleteConfirm aAxis, _ ) ->
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
                        , stored = False
                      }
                    , putPattern identity model.address model.sha "delete axis" newPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedCircleDeleteModalDelete ->
            case model.maybeModal of
                Just ( CircleDeleteConfirm aCircle, _ ) ->
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
                        , stored = False
                      }
                    , putPattern identity model.address model.sha "delete circle" newPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedCurveDeleteModalDelete ->
            case model.maybeModal of
                Just ( CurveDeleteConfirm aCurve, _ ) ->
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
                        , stored = False
                      }
                    , putPattern identity model.address model.sha "delete curve" newPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedDetailDeleteModalDelete ->
            case model.maybeModal of
                Just ( DetailDeleteConfirm aDetail, _ ) ->
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
                        , stored = False
                      }
                    , putPattern identity model.address model.sha "delete detail" newPattern
                    )

                _ ->
                    ( model, Cmd.none )

        UserPressedVariableDeleteModalDelete ->
            case model.maybeModal of
                Just ( VariableDeleteConfirm variable, _ ) ->
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
                        , stored = False
                      }
                    , putPattern identity model.address model.sha "delete variable" newPattern
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

                Just ( modal, _ ) ->
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
        Loading _ ->
            LocalStorage.changedStore
                { changedZoom = ChangedZoom
                , changedCenter = ChangedCenter
                , changedAddresses = ChangedAddresses
                , changedPattern = ChangedPattern
                , changedMeta = ChangedMeta
                , changedWhatever = ChangedWhatever
                }

        Error _ ->
            Sub.none

        Loaded data ->
            loadedSubscriptions data


loadedSubscriptions : LoadedData -> Sub Msg
loadedSubscriptions data =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> UserResizedWindow)
        , LocalStorage.changedStore
            { changedZoom = ChangedZoom
            , changedCenter = ChangedCenter
            , changedAddresses = ChangedAddresses
            , changedPattern = ChangedPattern
            , changedMeta = ChangedMeta
            , changedWhatever = ChangedWhatever
            }

        -- ANIMATIONS
        , case data.maybeModal of
            Nothing ->
                Sub.none

            Just ( _, state ) ->
                Sub.map ChangedModalState (Ui.Molecule.Modal.subscriptions state)

        -- VIEWPORT MEASURMENTS
        , case data.patternContainerDimensions of
            Just _ ->
                Sub.none

            Nothing ->
                Browser.Events.onAnimationFrame
                    (\_ -> AnimationFrameRequestedPatternContainerViewport)
        ]



---- SLIDE PATTERN


endSlidePattern : Html.Events.Extra.Touch.Event -> LoadedData -> ( LoadedData, Cmd Msg )
endSlidePattern event model =
    let
        newCenter start current =
            case event.touches of
                touch :: [] ->
                    model.center
                        |> Point2d.translateBy
                            (Vector2d.at_ model.resolution <|
                                Vector2d.fromPixels
                                    { x = Tuple.first start - Tuple.first touch.screenPos
                                    , y = Tuple.second start - Tuple.second touch.screenPos
                                    }
                            )

                _ ->
                    model.center
                        |> Point2d.translateBy
                            (Vector2d.at_ model.resolution <|
                                Vector2d.fromPixels
                                    { x = Tuple.first start - Tuple.first current
                                    , y = Tuple.second start - Tuple.second current
                                    }
                            )
    in
    case model.drag of
        NoDrag ->
            ( model, Cmd.none )

        DragWithOneTouchPoint { start, current } ->
            ( { model
                | center = newCenter start current
                , drag = NoDrag
              }
            , LocalStorage.updateCenter model.address (newCenter start current)
            )

        DragWithTwoTouchPoints _ ->
            -- TODO implement properly
            ( model, Cmd.none )

        DragWithMouse _ ->
            ( model, Cmd.none )


endDragWithMouse : Html.Events.Extra.Mouse.Event -> LoadedData -> ( LoadedData, Cmd Msg )
endDragWithMouse event model =
    case model.drag of
        NoDrag ->
            ( model, Cmd.none )

        DragWithOneTouchPoint _ ->
            ( model, Cmd.none )

        DragWithTwoTouchPoints _ ->
            ( model, Cmd.none )

        DragWithMouse { start } ->
            let
                newCenter =
                    model.center
                        |> Point2d.translateBy
                            (Vector2d.at_ model.resolution <|
                                Vector2d.fromPixels
                                    { x = Tuple.first start - Tuple.first event.screenPos
                                    , y = Tuple.second start - Tuple.second event.screenPos
                                    }
                            )
            in
            ( { model
                | center = newCenter
                , drag = NoDrag
              }
            , LocalStorage.updateCenter model.address newCenter
            )



---- SLIDE TOOLBAR TOP


startSlideToolbarTop : Html.Events.Extra.Touch.Event -> LoadedData -> LoadedData
startSlideToolbarTop event model =
    case event.touches of
        touch :: [] ->
            { model
                | maybeSlideToolbarTop =
                    Just
                        { identifier = touch.identifier
                        , start = touch.screenPos
                        , current = touch.screenPos
                        }
            }

        _ ->
            model


updateSlideToolbarTop : Html.Events.Extra.Touch.Event -> LoadedData -> Slide -> LoadedData
updateSlideToolbarTop event model slide =
    case event.touches of
        touch :: [] ->
            if slide.identifier == touch.identifier then
                { model | maybeSlideToolbarTop = Just { slide | current = touch.screenPos } }

            else
                model

        _ ->
            model


endSlideToolbarTop : Html.Events.Extra.Touch.Event -> LoadedData -> Slide -> LoadedData
endSlideToolbarTop event model slide =
    let
        deltaY =
            case event.touches of
                touch :: [] ->
                    Tuple.second touch.screenPos - Tuple.second slide.start

                _ ->
                    Tuple.second slide.current - Tuple.second slide.start
    in
    if deltaY >= toolbarTopHeight / 2 then
        { model
            | topToolbarExpanded = True
            , maybeSlideToolbarTop = Nothing
        }

    else if deltaY <= toolbarTopHeight / -2 then
        { model
            | topToolbarExpanded = False
            , maybeSlideToolbarTop = Nothing
        }

    else
        { model | maybeSlideToolbarTop = Nothing }



---- HELPER


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<no name>"


putPattern : Github.Identity -> LocalStorage.Address -> String -> String -> Pattern BottomLeft -> Cmd Msg
putPattern identity address sha message newPattern =
    case address of
        LocalStorage.GithubRepo { repo } ->
            Github.putPattern identity
                { repo = repo
                , message = message
                , pattern = newPattern
                , sha = sha
                , onSha = ReceivedSha
                }

        LocalStorage.Browser _ ->
            LocalStorage.updatePattern address newPattern


isCompact : Element.Device -> Bool
isCompact device =
    case ( device.class, device.orientation ) of
        ( Element.Phone, _ ) ->
            True

        ( Element.Tablet, Element.Portrait ) ->
            True

        _ ->
            False


requestViewportOfToolbarBottom : Cmd Msg
requestViewportOfToolbarBottom =
    Task.attempt ReceivedToolbarBottomViewport
        (Browser.Dom.getViewportOf "toolbar-bottom")


requestViewportOfPattern : Float -> Cmd Msg
requestViewportOfPattern delay =
    Process.sleep delay
        |> Task.andThen (\_ -> Browser.Dom.getViewportOf "pattern-container")
        |> Task.attempt UpdateRequestedPatternContainerViewport


focusNameInput : Cmd Msg
focusNameInput =
    Browser.Dom.focus "name-input"
        |> Task.attempt (\_ -> NoOp)


updateZoom : LocalStorage.Address -> Resolution -> Cmd Msg
updateZoom address resolution =
    LocalStorage.updateZoom address
        (Pixels.inPixels (Quantity.at resolution (Length.meters 1)))


updateToIdealViewport :
    LocalStorage.Address
    -> Pattern BottomLeft
    -> Pattern.Viewport.Dimensions
    -> Cmd Msg
updateToIdealViewport address pattern dimensions =
    case Pattern.Viewport.idealForPattern dimensions pattern of
        Nothing ->
            Cmd.none

        Just { resolution, center } ->
            Cmd.batch
                [ LocalStorage.updateZoom address
                    (Length.meters 1
                        |> Quantity.at resolution
                        |> Pixels.inPixels
                    )
                , LocalStorage.updateCenter address center
                ]


toolbarTopHeight : Float
toolbarTopHeight =
    124


actualCenter : LoadedData -> Point2d Meters BottomLeft
actualCenter model =
    case model.drag of
        NoDrag ->
            model.center

        DragWithOneTouchPoint { start, current } ->
            model.center
                |> Point2d.translateBy
                    (Vector2d.at_ model.resolution <|
                        Vector2d.fromPixels
                            { x = Tuple.first start - Tuple.first current
                            , y = Tuple.second start - Tuple.second current
                            }
                    )

        DragWithTwoTouchPoints { startA, currentA } ->
            -- TODO implement this properly
            model.center
                |> Point2d.translateBy
                    (Vector2d.at_ model.resolution <|
                        Vector2d.fromPixels
                            { x = Tuple.first startA - Tuple.first currentA
                            , y = Tuple.second startA - Tuple.second currentA
                            }
                    )

        DragWithMouse { start, current } ->
            model.center
                |> Point2d.translateBy
                    (Vector2d.at_ model.resolution <|
                        Vector2d.fromPixels
                            { x = Tuple.first start - Tuple.first current
                            , y = Tuple.second start - Tuple.second current
                            }
                    )


dragging : LoadedData -> Bool
dragging model =
    case model.drag of
        NoDrag ->
            False

        DragWithOneTouchPoint { start, current } ->
            start /= current

        DragWithTwoTouchPoints { startA, currentA } ->
            -- TODO implement this properly
            startA /= currentA

        DragWithMouse { start, current } ->
            start /= current
