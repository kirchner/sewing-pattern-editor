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

import Auth
import Browser.Navigation
import Element exposing (Element)
import Github
import Http
import List.Extra as List
import LocalStorage
import Session exposing (Session)
import Storage.Address as Address exposing (Address)
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
    , addresses : List Address
    , unrequestedAddresses : Maybe (List Address)
    , patterns : List ProcessedPattern
    }


type alias LoadedData =
    { session : Session
    , addresses : List Address
    , patterns : List ProcessedPattern
    , search : String
    }


type alias ProcessedPattern =
    { address : Address
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
            { cred = Github.noCred
            , device = device
            , heading = "Patterns"
            , backToLabel = Nothing
            , userPressedLogout = Just UserPressedLogout
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
                |> Element.maximum 860
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
    | ChangedAddresses (List Address)
    | ChangedMeta Address Github.Meta
    | ReceivedMeta Address (Result Http.Error Github.Meta)
    | ChangedWhatever
    | UserPressedClone Address
    | UserPressedLogout
    | ReceivedLogout (Result Http.Error ())


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading data ->
            updateLoading msg data

        Loaded data ->
            updateLoaded msg data
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


updateLoaded : Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateLoaded msg model =
    case msg of
        UserChangedSearch newSearch ->
            ( { model | search = newSearch }
            , Cmd.none
            )

        UserPressedImport ->
            ( model, Cmd.none )

        UserPressedCreate ->
            ( model
            , Browser.Navigation.pushUrl (Session.key model.session) "/new"
            )

        UserPressedClone _ ->
            ( model, Cmd.none )

        UserPressedLogout ->
            ( model
            , Auth.logout model.session ReceivedLogout
            )

        ReceivedLogout result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( model
                    , Browser.Navigation.load "/"
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


addMeta : Address -> Github.Meta -> LoadingData -> LoadingData
addMeta address meta data =
    let
        newPatterns =
            { address = address
            , name = meta.name
            , description = meta.description
            , storage =
                case address of
                    Address.GithubRepo { repo } ->
                        Ui.Molecule.PatternList.Github repo.owner repo.name

                    Address.Browser { slug } ->
                        Ui.Molecule.PatternList.LocalStorage slug
            , updatedAt = Time.millisToPosix 0
            }
                :: data.patterns
                |> List.uniqueBy (.address >> addressToHash)

        addressToHash address_ =
            case address_ of
                Address.GithubRepo { repo } ->
                    "github/" ++ repo.owner ++ "/" ++ repo.name

                Address.Browser { slug } ->
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
                Address.GithubRepo { repo, ref } ->
                    Github.getMeta Github.noCred
                        { repo = repo
                        , ref = ref
                        , onMeta = ReceivedMeta next
                        }

                Address.Browser _ ->
                    LocalStorage.requestMeta next
            )
