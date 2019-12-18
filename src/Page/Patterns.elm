module Page.Patterns exposing
    ( Model, init
    , view
    , Msg, update, subscriptions
    )

{-|

@docs Model, init
@docs view
@docs Msg, update, subscriptions

-}

import Browser.Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Git
import Http
import LocalStorage
import Route
import Time exposing (Posix)
import Ui.Atom.Input
import Ui.Molecule.PatternList
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- MODEL


{-| -}
type Model
    = Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { addresses : List LocalStorage.Address
    , unrequestedAddresses : List LocalStorage.Address
    , patterns : List ProcessedPattern
    }


type alias LoadedData =
    { addresses : List LocalStorage.Address
    , patterns : List ProcessedPattern
    , search : String
    }


type alias ProcessedPattern =
    { address : LocalStorage.Address
    , name : String
    , description : String
    , storage : Ui.Molecule.PatternList.Storage
    , updatedAt : Posix
    }


{-| -}
init : ( Model, Cmd Msg )
init =
    ( Loading
        { addresses = []
        , unrequestedAddresses = []
        , patterns = []
        }
    , LocalStorage.requestAddresses
    )



---- VIEW


{-| -}
view :
    Git.Identity
    -> Model
    -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view identity model =
    { title = "Patterns"
    , body =
        case model of
            Loading _ ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Loading patterns...")

            Loaded data ->
                viewPatterns identity data
    , dialog = Nothing
    }


viewPatterns : Git.Identity -> LoadedData -> Element Msg
viewPatterns identity model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level4
        ]
        [ viewTopBar identity
        , viewContent model
        ]


viewTopBar : Git.Identity -> Element Msg
viewTopBar identity =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px (2 * Ui.Theme.Spacing.level7))
        , Background.color Ui.Theme.Color.secondary
        , Element.inFront <|
            Element.el
                [ Element.centerX
                , Element.width
                    (Element.fill
                        |> Element.maximum 780
                    )
                , Element.height Element.fill
                , Element.padding (7 + Ui.Theme.Spacing.level1)
                ]
                (Element.el
                    [ Element.centerY ]
                    (Ui.Theme.Typography.headingOne "Patterns")
                )
        ]
        [ case identity of
            Git.Anonymous ->
                Element.el
                    [ Element.paddingXY Ui.Theme.Spacing.level1 0
                    , Element.alignRight
                    ]
                    (Ui.Atom.Input.btnPrimary
                        { id = "sign-in-btn"
                        , onPress = Just UserPressedSignIn
                        , label = "Sign in via GitHub"
                        }
                    )

            Git.OauthToken _ ->
                Element.none
        ]


viewContent : LoadedData -> Element Msg
viewContent model =
    let
        toPatternInfo processedPattern =
            { name = processedPattern.name
            , description = processedPattern.description
            , storage = processedPattern.storage
            , updatedAt = processedPattern.updatedAt
            , onClone = UserPressedClone processedPattern.address
            }
    in
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.centerX
        , Element.width
            (Element.fill
                |> Element.maximum 780
            )
        ]
        [ Ui.Molecule.PatternList.view
            { search = model.search
            , onSearchChange = UserChangedSearch
            , onImport = UserPressedImport
            , onCreate = UserPressedCreate
            , patternInfos = List.map toPatternInfo model.patterns
            , now = Time.millisToPosix 0
            }
        ]



---- UPDATE


{-| -}
type Msg
    = UserChangedSearch String
    | UserPressedImport
    | UserPressedCreate
    | ChangedAddresses (List LocalStorage.Address)
    | ChangedMeta LocalStorage.Address Git.Meta
    | ReceivedMeta LocalStorage.Address (Result Http.Error Git.Meta)
    | ChangedWhatever
    | UserPressedClone LocalStorage.Address
    | UserPressedSignIn


{-| -}
update :
    Browser.Navigation.Key
    -> String
    -> String
    -> Git.Identity
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update key domain clientId identity msg model =
    case model of
        Loading data ->
            updateLoading identity msg data

        Loaded data ->
            updateLoaded key domain clientId msg data
                |> Tuple.mapFirst Loaded


updateLoading : Git.Identity -> Msg -> LoadingData -> ( Model, Cmd Msg )
updateLoading identity msg model =
    case msg of
        ChangedAddresses newAddresses ->
            { model | unrequestedAddresses = newAddresses }
                |> requestNextMeta identity

        ChangedMeta address meta ->
            model
                |> addMeta address meta
                |> requestNextMeta identity

        ReceivedMeta _ (Err httpError) ->
            model
                |> requestNextMeta identity

        ReceivedMeta address (Ok meta) ->
            model
                |> addMeta address meta
                |> requestNextMeta identity

        ChangedWhatever ->
            ( Loading model, Cmd.none )

        _ ->
            ( Loading model, Cmd.none )


updateLoaded :
    Browser.Navigation.Key
    -> String
    -> String
    -> Msg
    -> LoadedData
    -> ( LoadedData, Cmd Msg )
updateLoaded key domain clientId msg model =
    case msg of
        UserChangedSearch newSearch ->
            ( { model | search = newSearch }
            , Cmd.none
            )

        UserPressedImport ->
            ( model, Cmd.none )

        UserPressedCreate ->
            ( model
            , Browser.Navigation.pushUrl key "/new"
            )

        UserPressedClone address ->
            ( model, Cmd.none )

        UserPressedSignIn ->
            ( model
            , Git.requestAuthorization clientId (Route.crossOrigin domain Route.Patterns [])
            )

        _ ->
            ( model, Cmd.none )


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    LocalStorage.changedStore
        { changedZoom = \_ _ -> ChangedWhatever
        , changedCenter = \_ _ -> ChangedWhatever
        , changedAddresses = ChangedAddresses
        , changedPattern = \_ _ -> ChangedWhatever
        , changedMeta = ChangedMeta
        , changedWhatever = ChangedWhatever
        }


addMeta : LocalStorage.Address -> Git.Meta -> LoadingData -> LoadingData
addMeta address meta data =
    let
        newPatterns =
            { address = address
            , name = meta.name
            , description = meta.description
            , storage =
                case address of
                    LocalStorage.GitRepo { repo } ->
                        Ui.Molecule.PatternList.Github repo.owner repo.name

                    LocalStorage.Browser { slug } ->
                        Ui.Molecule.PatternList.LocalStorage slug
            , updatedAt = Time.millisToPosix 0
            }
                :: data.patterns
    in
    { data | patterns = newPatterns }


requestNextMeta : Git.Identity -> LoadingData -> ( Model, Cmd Msg )
requestNextMeta identity data =
    case data.unrequestedAddresses of
        [] ->
            ( Loaded
                { addresses = data.addresses
                , patterns = data.patterns
                , search = ""
                }
            , Cmd.none
            )

        next :: rest ->
            ( Loading { data | unrequestedAddresses = rest }
            , case next of
                LocalStorage.GitRepo { repo, ref } ->
                    Git.getMeta identity
                        { repo = repo
                        , ref = ref
                        , onMeta = ReceivedMeta next
                        }

                LocalStorage.Browser _ ->
                    LocalStorage.requestMeta next
            )
