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

import BoundingBox2d
import Browser.Navigation
import Detail2d exposing (Detail2d)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg as Svg
import Geometry.Svg.Extra as Svg
import Git
import Html.Attributes
import Http
import Length
import LocalStorage
import Pattern exposing (Pattern)
import Pixels
import Point2d
import Quantity
import Route
import State
import Svg
import Svg.Attributes
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Vector2d



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
    , selectedDetail : Int
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
        , selectedDetail = 0
        }
    , Cmd.none
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
            ]
            (viewDetailSegmentControl model)
        ]


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
                    Length.inMeters maxX - Length.inMeters minX + pixelWidth

                height =
                    Length.inMeters maxY - Length.inMeters minY + pixelWidth

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
      -- DETAIL
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

        -- DETAIL
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
            LocalStorage.changedStore
                { changedZoom = \_ _ -> ChangedWhatever
                , changedCenter = \_ _ -> ChangedWhatever
                , changedAddresses = \_ -> ChangedWhatever
                , changedPattern = ChangedPattern
                , changedMeta = ChangedMeta
                , changedWhatever = ChangedWhatever
                }
