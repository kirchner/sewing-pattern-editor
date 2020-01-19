module Page.Patterns exposing
    ( Model, init, toSession
    , view
    , Msg, update, subscriptions
    )

{-|

@docs Model, init, toSession
@docs view
@docs Msg, update, subscriptions

-}

import Browser.Navigation
import Element exposing (Element)
import Github
import Http
import List.Extra as List
import LocalStorage
import Route
import Session exposing (Session)
import Time exposing (Posix)
import Ui.Molecule.PatternList
import Ui.Molecule.TopBar
import Ui.Theme.Spacing



---- MODEL


{-| -}
type Model
    = Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { session : Session
    , addresses : List LocalStorage.Address
    , unrequestedAddresses : Maybe (List LocalStorage.Address)
    , patterns : List ProcessedPattern
    }


type alias LoadedData =
    { session : Session
    , addresses : List LocalStorage.Address
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
init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading
        { session = session
        , addresses = []
        , unrequestedAddresses = Nothing
        , patterns = []
        }
    , LocalStorage.requestAddresses
    )


{-| -}
toSession : Model -> Session
toSession model =
    case model of
        Loading { session } ->
            session

        Loaded { session } ->
            session



---- VIEW


{-| -}
view : Element.Device -> Model -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view device model =
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
                viewPatterns device data
    , dialog = Nothing
    }


viewPatterns : Element.Device -> LoadedData -> Element Msg
viewPatterns device model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level4
        ]
        [ Ui.Molecule.TopBar.view
            { userPressedSignIn = UserPressedSignIn
            , cred = Session.githubCred model.session
            , device = device
            , heading = "Patterns"
            , backToLabel = Nothing
            }
        , viewContent model
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
    | ChangedMeta LocalStorage.Address Github.Meta
    | ReceivedMeta LocalStorage.Address (Result Http.Error Github.Meta)
    | ChangedWhatever
    | UserPressedClone LocalStorage.Address
    | UserPressedSignIn


{-| -}
update : String -> Msg -> Model -> ( Model, Cmd Msg )
update clientId msg model =
    case model of
        Loading data ->
            updateLoading msg data

        Loaded data ->
            updateLoaded clientId msg data
                |> Tuple.mapFirst Loaded


updateLoading : Msg -> LoadingData -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        ChangedAddresses newAddresses ->
            case model.unrequestedAddresses of
                Nothing ->
                    { model | unrequestedAddresses = Just newAddresses }
                        |> requestNextMeta

                Just _ ->
                    ( Loading model, Cmd.none )

        ChangedMeta address meta ->
            model
                |> addMeta address meta
                |> requestNextMeta

        ReceivedMeta _ (Err _) ->
            model
                |> requestNextMeta

        ReceivedMeta address (Ok meta) ->
            model
                |> addMeta address meta
                |> requestNextMeta

        ChangedWhatever ->
            ( Loading model, Cmd.none )

        _ ->
            ( Loading model, Cmd.none )


updateLoaded : String -> Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateLoaded clientId msg model =
    case msg of
        UserChangedSearch newSearch ->
            ( { model | search = newSearch }
            , Cmd.none
            )

        UserPressedImport ->
            ( model, Cmd.none )

        UserPressedCreate ->
            ( model
            , Browser.Navigation.pushUrl (Session.navKey model.session) "/new"
            )

        UserPressedClone _ ->
            ( model, Cmd.none )

        UserPressedSignIn ->
            ( model
            , Github.requestAuthorization clientId <|
                Route.crossOrigin (Session.domain model.session) Route.Patterns []
            )

        _ ->
            ( model, Cmd.none )


{-| -}
subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.changedStore
        { changedZoom = \_ _ -> ChangedWhatever
        , changedCenter = \_ _ -> ChangedWhatever
        , changedAddresses = ChangedAddresses
        , changedPattern = \_ _ -> ChangedWhatever
        , changedMeta = ChangedMeta
        , changedWhatever = ChangedWhatever
        }


addMeta : LocalStorage.Address -> Github.Meta -> LoadingData -> LoadingData
addMeta address meta data =
    let
        newPatterns =
            { address = address
            , name = meta.name
            , description = meta.description
            , storage =
                case address of
                    LocalStorage.GithubRepo { repo } ->
                        Ui.Molecule.PatternList.Github repo.owner repo.name

                    LocalStorage.Browser { slug } ->
                        Ui.Molecule.PatternList.LocalStorage slug
            , updatedAt = Time.millisToPosix 0
            }
                :: data.patterns
                |> List.uniqueBy (.address >> addressToHash)

        addressToHash address_ =
            case address_ of
                LocalStorage.GithubRepo { repo } ->
                    "github/" ++ repo.owner ++ "/" ++ repo.name

                LocalStorage.Browser { slug } ->
                    "browser/" ++ slug
    in
    { data | patterns = newPatterns }


requestNextMeta : LoadingData -> ( Model, Cmd Msg )
requestNextMeta data =
    case data.unrequestedAddresses of
        Nothing ->
            ( Loading data
            , Cmd.none
            )

        Just [] ->
            ( Loaded
                { session = data.session
                , addresses = data.addresses
                , patterns = data.patterns
                , search = ""
                }
            , Cmd.none
            )

        Just (next :: rest) ->
            ( Loading { data | unrequestedAddresses = Just rest }
            , case next of
                LocalStorage.GithubRepo { repo, ref } ->
                    Github.getMeta (Session.githubCred data.session)
                        { repo = repo
                        , ref = ref
                        , onMeta = ReceivedMeta next
                        }

                LocalStorage.Browser _ ->
                    LocalStorage.requestMeta next
            )
