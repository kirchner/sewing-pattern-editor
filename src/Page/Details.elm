module Page.Details exposing
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

import Angle
import Axis3d
import BoundingBox2d
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Camera3d
import Detail2d exposing (Detail2d)
import Detail3d
import Direction3d
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg as Svg
import Geometry.Svg.Extra as Svg
import Git
import Html exposing (Html)
import Html.Attributes
import Http
import Length
import LineSegment2d
import LineSegment3d
import LineSegment3d.Projection
import List.Extra as List
import LocalStorage
import Pattern exposing (Pattern)
import Pixels
import Point2d
import Point3d
import Polygon2d
import Ports
import Quantity
import Rectangle2d
import Route
import SketchPlane3d
import State
import Svg
import Svg.Attributes
import Task
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Vector2d
import Vector3d
import Viewpoint3d



---- MODEL


{-| -}
type Model
    = Loading LoadingData
    | Error
    | Loaded LoadedData


type alias LoadingData =
    { address : LocalStorage.Address
    , maybePatternData : Maybe (Git.PatternData BottomLeft)
    , maybeMeta : Maybe Git.Meta
    , maybePermissions : Maybe Git.Permissions
    }


type BottomLeft
    = BottomLeft BottomLeft


type alias LoadedData =
    { address : LocalStorage.Address
    , permissions : Git.Permissions
    , sha : String
    , pattern : Pattern BottomLeft
    , name : String

    -- DETAIL
    , width : Float
    , height : Float
    , cameraWidth : Int
    , cameraHeight : Int
    , pose : Maybe Pose
    , selectedDetail : Int
    }


type alias Pose =
    { rotation : List Float
    , translation :
        { x : Float
        , y : Float
        , z : Float
        }
    }


{-| -}
init : Git.Identity -> LocalStorage.Address -> ( Model, Cmd Msg )
init identity address =
    ( Loading
        { address = address
        , maybePatternData = Nothing
        , maybeMeta = Nothing
        , maybePermissions = Nothing
        }
    , case address of
        LocalStorage.GitRepo { repo, ref } ->
            Cmd.batch
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

        LocalStorage.Browser _ ->
            Cmd.batch
                [ LocalStorage.requestPattern address
                , LocalStorage.requestMeta address
                ]
    )


initLoaded :
    Element.Device
    -> LocalStorage.Address
    -> String
    -> Pattern BottomLeft
    -> Git.Meta
    -> Git.Permissions
    -> ( Model, Cmd Msg )
initLoaded device address sha pattern meta permissions =
    ( Loaded
        { address = address
        , permissions = permissions
        , sha = sha
        , pattern = Pattern.regenerateCaches pattern
        , name = meta.name

        -- DETAIL
        , width = 0
        , height = 0
        , cameraWidth = 0
        , cameraHeight = 0
        , pose = Nothing
        , selectedDetail = 0
        }
    , Task.attempt ReceivedViewport Browser.Dom.getViewport
    )



---- VIEW


{-| -}
view :
    Element.Device
    -> Git.Identity
    -> Model
    -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view device identity model =
    case model of
        Loading _ ->
            statusMsg "Loading pattern..." "Loading pattern..."

        Error ->
            statusMsg "Something went wrong." "Loading pattern.."

        Loaded data ->
            { title = "Sewing Pattern Editor - Details"
            , body = viewDetails device identity data
            , dialog = Nothing
            }


statusMsg : String -> String -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
statusMsg title text =
    { title = title
    , body = Element.el [ Element.centerX, Element.centerY ] (Element.text text)
    , dialog = Nothing
    }



---- EDITOR


viewDetails : Element.Device -> Git.Identity -> LoadedData -> Element Msg
viewDetails device identity model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.inFront <|
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        [ Element.row
                            [ Element.width Element.fill
                            , Background.color Ui.Theme.Color.secondary
                            ]
                            [ Element.el [ Element.padding Ui.Theme.Spacing.level1 ]
                                (backToPatternLink model.address)
                            ]
                        , Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            Element.none
                        , Element.el
                            [ Element.width Element.fill
                            , Border.widthEach
                                { top = 1
                                , bottom = 0
                                , left = 0
                                , right = 0
                                }
                            , Border.color Ui.Theme.Color.secondary
                            , Background.color Ui.Theme.Color.white
                            ]
                            (viewDetailSegmentControl model)
                        ]
                ]
                (viewMarker model)
        ]
        (Element.html <|
            Html.div
                [ Html.Attributes.style "overflow" "hidden" ]
                [ Html.video
                    [ Html.Attributes.style "display" "none" ]
                    []
                , Html.canvas
                    [ Html.Attributes.id "for-computation"
                    , Html.Attributes.style "display" "none"
                    ]
                    []
                , Html.canvas
                    [ Html.Attributes.id "for-display" ]
                    []
                ]
        )


viewMarker : LoadedData -> Element msg
viewMarker model =
    let
        maybeDetail2d =
            Pattern.details model.pattern
                |> List.getAt model.selectedDetail
                |> Maybe.andThen (Result.toMaybe << State.finalValue model.pattern << Pattern.detail2d)
    in
    case ( maybeDetail2d, model.pose ) of
        ( Just detail2d, Just pose ) ->
            let
                camera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { focalPoint = Point3d.pixels 0 0 1000
                                , eyePoint = Point3d.origin
                                , upDirection = Direction3d.positiveY
                                }
                        , verticalFieldOfView = Angle.degrees 40
                        , clipDepth = Pixels.pixels 10
                        }

                window =
                    Rectangle2d.from Point2d.origin
                        (Point2d.pixels
                            (toFloat model.cameraWidth)
                            (toFloat model.cameraHeight)
                        )

                rotate rotateAround =
                    case pose.rotation of
                        _ :: _ :: a13 :: a21 :: a22 :: a23 :: _ :: _ :: a33 :: [] ->
                            let
                                xRotation =
                                    asin a23

                                yRotation =
                                    -1 * atan2 a13 a33

                                zRotation =
                                    atan2 a21 a22
                            in
                            rotateAround Axis3d.x (Angle.radians xRotation)
                                >> rotateAround Axis3d.y (Angle.radians yRotation)
                                >> rotateAround Axis3d.z (Angle.radians zRotation)

                        _ ->
                            identity

                translation =
                    Vector3d.pixels
                        (-1 * pose.translation.x)
                        (-1 * pose.translation.y)
                        pose.translation.z

                scaledDetail2d =
                    detail2d
                        |> Detail2d.at
                            (Pixels.pixels 1
                                |> Quantity.per (Length.millimeters 1)
                            )

                { viewBoxMinX, viewBoxMinY, viewBoxWidth, viewBoxHeight } =
                    if
                        (toFloat model.cameraHeight / toFloat model.cameraWidth)
                            < (model.height / model.height)
                    then
                        { viewBoxMinX =
                            (toFloat model.cameraWidth
                                * model.height
                                / toFloat model.cameraHeight
                                - model.width
                            )
                                / 2
                        , viewBoxMinY = 0
                        , viewBoxWidth = model.width * toFloat model.cameraHeight / model.height
                        , viewBoxHeight = toFloat model.cameraHeight
                        }

                    else
                        { viewBoxMinX = 0
                        , viewBoxMinY =
                            (toFloat model.cameraHeight
                                * model.width
                                / toFloat model.cameraWidth
                                - model.height
                            )
                                / 2
                        , viewBoxWidth = toFloat model.cameraWidth
                        , viewBoxHeight = model.height * toFloat model.cameraWidth / model.width
                        }
            in
            Element.html <|
                Svg.svg
                    [ Svg.Attributes.viewBox <|
                        String.join " "
                            [ String.fromFloat viewBoxMinX
                            , String.fromFloat viewBoxMinY
                            , String.fromFloat viewBoxWidth
                            , String.fromFloat viewBoxHeight
                            ]
                    , Html.Attributes.style "user-select" "none"
                    , Html.Attributes.style "width" (String.fromFloat model.width ++ "px")
                    , Html.Attributes.style "height" (String.fromFloat model.height ++ "px")
                    ]
                    [ scaledDetail2d
                        |> Detail2d.translateBy
                            (case Detail2d.centerPoint scaledDetail2d of
                                Nothing ->
                                    Vector2d.zero

                                Just centerPoint2d ->
                                    Vector2d.from centerPoint2d Point2d.origin
                            )
                        |> Detail3d.on SketchPlane3d.xy
                        |> rotate Detail3d.rotateAround
                        |> Detail3d.translateBy translation
                        |> Detail3d.toScreenSpace camera window
                        |> Maybe.map
                            (Svg.detail2d
                                [ Svg.Attributes.fill "none"
                                , Svg.Attributes.strokeDasharray "20 20"
                                , Svg.Attributes.stroke (toColor Ui.Theme.Color.primary)
                                , Svg.Attributes.strokeWidth "3"
                                , Svg.Attributes.strokeLinecap "round"
                                ]
                            )
                        |> Maybe.withDefault (Svg.text "")

                    --, LineSegment3d.from Point3d.origin (Point3d.pixels 81 0 0)
                    --    |> rotate LineSegment3d.rotateAround
                    --    |> LineSegment3d.translateBy translation
                    --    |> LineSegment3d.Projection.toScreenSpace camera window
                    --    |> Maybe.map
                    --        (Svg.lineSegment2d
                    --            [ Svg.Attributes.fill "none"
                    --            , Svg.Attributes.stroke "red"
                    --            , Svg.Attributes.strokeWidth "4"
                    --            ]
                    --        )
                    --    |> Maybe.withDefault (Svg.text "")
                    --, LineSegment3d.from Point3d.origin (Point3d.pixels 0 81 0)
                    --    |> rotate LineSegment3d.rotateAround
                    --    |> LineSegment3d.translateBy translation
                    --    |> LineSegment3d.Projection.toScreenSpace camera window
                    --    |> Maybe.map
                    --        (Svg.lineSegment2d
                    --            [ Svg.Attributes.fill "none"
                    --            , Svg.Attributes.stroke "green"
                    --            , Svg.Attributes.strokeWidth "4"
                    --            ]
                    --        )
                    --    |> Maybe.withDefault (Svg.text "")
                    --, LineSegment3d.from Point3d.origin (Point3d.pixels 0 0 81)
                    --    |> rotate LineSegment3d.rotateAround
                    --    |> LineSegment3d.translateBy translation
                    --    |> LineSegment3d.Projection.toScreenSpace camera window
                    --    |> Maybe.map
                    --        (Svg.lineSegment2d
                    --            [ Svg.Attributes.fill "none"
                    --            , Svg.Attributes.stroke "blue"
                    --            , Svg.Attributes.strokeWidth "4"
                    --            ]
                    --        )
                    --    |> Maybe.withDefault (Svg.text "")
                    ]

        _ ->
            Element.none


backToPatternLink : LocalStorage.Address -> Element msg
backToPatternLink address =
    Ui.Theme.Focus.outline <|
        Element.link
            [ Font.color Ui.Theme.Color.primary
            , Element.mouseOver
                [ Font.color Ui.Theme.Color.primaryDark ]
            ]
            { url = Route.absolute (Route.Pattern address) []
            , label =
                Element.row
                    [ Element.spacing Ui.Theme.Spacing.level1 ]
                    [ Ui.Atom.Icon.fa "arrow-left"
                    , Ui.Theme.Typography.body "Back to pattern"
                    ]
            }


viewDetailSegmentControl : LoadedData -> Element Msg
viewDetailSegmentControl model =
    let
        viewDetail index aDetail =
            case State.finalValue model.pattern (Pattern.detail2d aDetail) of
                Err _ ->
                    Nothing

                Ok detail2d ->
                    Just <|
                        ( index
                        , viewDetailLabel detail2d
                        )
    in
    Ui.Atom.Input.segmentControl
        { id = "details"
        , label = Nothing
        , help = Nothing
        , onChange = UserSelectedDetail
        , options =
            Pattern.details model.pattern
                |> List.indexedMap viewDetail
                |> List.filterMap identity
        , selected = model.selectedDetail
        , child = Nothing
        }


viewDetailLabel : Detail2d Length.Meters BottomLeft -> Element msg
viewDetailLabel detail2d =
    case Detail2d.boundingBox detail2d of
        Nothing ->
            Element.none

        Just boundingBox ->
            let
                { minX, maxX, minY, maxY } =
                    BoundingBox2d.extrema boundingBox

                viewportWidth =
                    pixelWidth

                viewportHeight =
                    pixelWidth

                pixelWidth =
                    64

                pixelHeight =
                    pixelWidth

                width =
                    Length.inMeters maxX - Length.inMeters minX

                height =
                    Length.inMeters maxY - Length.inMeters minY

                idealHorizontalResolution =
                    Pixels.pixels pixelWidth
                        |> Quantity.per (Length.meters width)

                idealVerticalResolution =
                    Pixels.pixels pixelHeight
                        |> Quantity.per (Length.meters height)

                center =
                    BoundingBox2d.centerPoint boundingBox

                idealResolution =
                    Quantity.min idealHorizontalResolution idealVerticalResolution
            in
            Element.html <|
                Svg.svg
                    [ Svg.Attributes.viewBox <|
                        String.join " "
                            [ String.fromFloat (viewportWidth / -2)
                            , String.fromFloat (viewportHeight / -2)
                            , String.fromFloat viewportWidth
                            , String.fromFloat viewportHeight
                            ]
                    , Html.Attributes.style "user-select" "none"
                    , Html.Attributes.style "width"
                        (String.fromFloat viewportWidth ++ "px")
                    , Html.Attributes.style "height"
                        (String.fromFloat viewportHeight ++ "px")
                    ]
                    [ Svg.translateBy
                        (Vector2d.from
                            (Point2d.at idealResolution center)
                            Point2d.origin
                        )
                        (Svg.detail2d
                            [ Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke "currentColor"
                            , Svg.Attributes.strokeWidth "1"
                            ]
                            (Detail2d.at idealResolution detail2d)
                        )
                    ]



---- UPDATE


type Msg
    = ReceivedPatternData (Result Http.Error (Git.PatternData BottomLeft))
    | ReceivedSha (Result Http.Error String)
    | ReceivedMeta (Result Http.Error Git.Meta)
    | ReceivedPermissions (Result Http.Error Git.Permissions)
      -- LOCAL STORAGE
    | ChangedPattern LocalStorage.Address (Pattern BottomLeft)
    | ChangedMeta LocalStorage.Address Git.Meta
    | ChangedWhatever
      -- VIDEO
    | ReceivedViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ResizedBrowser Int Int
      -- DETAIL
    | ChangedCamera Int Int
    | ChangedPose Pose
    | UserSelectedDetail Int


{-| -}
update :
    Browser.Navigation.Key
    -> String
    -> String
    -> Element.Device
    -> Git.Identity
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update key domain clientId device identity msg model =
    case model of
        Loading data ->
            case updateLoading msg data of
                (Loading newData) as newModel ->
                    case ( newData.maybePatternData, newData.maybeMeta, newData.maybePermissions ) of
                        ( Just patternData, Just meta, Just permissions ) ->
                            initLoaded device
                                newData.address
                                patternData.sha
                                patternData.pattern
                                meta
                                permissions

                        _ ->
                            ( newModel, Cmd.none )

                newModel ->
                    ( newModel, Cmd.none )

        Error ->
            ( model, Cmd.none )

        Loaded data ->
            updateLoaded key domain clientId device identity msg data
                |> Tuple.mapFirst Loaded


updateLoading : Msg -> LoadingData -> Model
updateLoading msg data =
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


updateLoaded :
    Browser.Navigation.Key
    -> String
    -> String
    -> Element.Device
    -> Git.Identity
    -> Msg
    -> LoadedData
    -> ( LoadedData, Cmd Msg )
updateLoaded key domain clientId device identity msg model =
    case msg of
        ReceivedPatternData result ->
            case result of
                Err error ->
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
                Err error ->
                    ( model, Cmd.none )

                Ok newSha ->
                    ( { model | sha = newSha }
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
        ChangedPattern _ _ ->
            ( model, Cmd.none )

        ChangedMeta _ _ ->
            ( model, Cmd.none )

        ChangedWhatever ->
            ( model, Cmd.none )

        -- VIDEO
        ReceivedViewport result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok { scene } ->
                    ( { model
                        | width = scene.width
                        , height = scene.height
                      }
                    , Ports.startVideo
                        { width = toFloat (floor scene.width)
                        , height = toFloat (floor scene.height)
                        }
                    )

        ResizedBrowser width height ->
            ( { model
                | width = toFloat width
                , height = toFloat height
              }
            , Ports.resizeVideo
                { width = toFloat width
                , height = toFloat height
                }
            )

        -- DETAIL
        ChangedCamera width height ->
            ( { model
                | cameraWidth = width
                , cameraHeight = height
              }
            , Cmd.none
            )

        ChangedPose pose ->
            ( { model | pose = Just pose }
            , Cmd.none
            )

        UserSelectedDetail selectedDetail ->
            ( { model | selectedDetail = selectedDetail }
            , Cmd.none
            )



---- SUBSCRIPTIONS


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading _ ->
            LocalStorage.changedStore
                { changedZoom = \_ _ -> ChangedWhatever
                , changedCenter = \_ _ -> ChangedWhatever
                , changedAddresses = \_ -> ChangedWhatever
                , changedPattern = ChangedPattern
                , changedMeta = ChangedMeta
                , changedWhatever = ChangedWhatever
                }

        Error ->
            Sub.none

        Loaded _ ->
            Sub.batch
                [ LocalStorage.changedStore
                    { changedZoom = \_ _ -> ChangedWhatever
                    , changedCenter = \_ _ -> ChangedWhatever
                    , changedAddresses = \_ -> ChangedWhatever
                    , changedPattern = ChangedPattern
                    , changedMeta = ChangedMeta
                    , changedWhatever = ChangedWhatever
                    }
                , Ports.changedPose (.pose >> ChangedPose)
                , Ports.changedCamera (\{ camera } -> ChangedCamera camera.width camera.height)
                , Browser.Events.onResize ResizedBrowser
                ]


toColor : Color -> String
toColor color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color
    in
    String.concat
        [ "rgba("
        , String.fromInt (floor (255 * red))
        , ","
        , String.fromInt (floor (255 * green))
        , ","
        , String.fromInt (floor (255 * blue))
        , ","
        , String.fromFloat alpha
        , ")"
        ]
