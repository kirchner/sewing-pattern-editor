module Page.NewPatterns exposing (Model, Msg, init, subscriptions, update, view)

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


type Model
    = Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { addresses : List LocalStorage.Address
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


init : ( Model, Cmd Msg )
init =
    ( Loading
        { addresses = []
        , patterns = []
        }
    , LocalStorage.requestAddresses
    )



---- VIEW


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
        , Element.padding (Ui.Theme.Spacing.level1 // 2)
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
                , Element.padding 7
                ]
                (Element.el
                    [ Element.centerY ]
                    (Ui.Theme.Typography.headingOne "Patterns")
                )
        ]
        [ case identity of
            Git.Anonymous ->
                Element.el
                    [ Element.paddingXY Ui.Theme.Spacing.level4 0
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


type Msg
    = UserChangedSearch String
    | UserPressedImport
    | UserPressedCreate
    | ChangedAddresses (List LocalStorage.Address)
    | ReceivedMeta LocalStorage.Address (List LocalStorage.Address) (Result Http.Error Git.Meta)
    | ChangedWhatever
    | UserPressedClone LocalStorage.Address
    | UserPressedSignIn


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
            ( Loading { model | addresses = newAddresses }
            , case newAddresses of
                [] ->
                    Cmd.none

                first :: rest ->
                    Git.getMeta identity
                        { repo = first.repo
                        , ref = first.ref
                        , onMeta = ReceivedMeta first rest
                        }
            )

        ReceivedMeta _ addresses (Err httpError) ->
            ( if List.isEmpty addresses then
                Loaded
                    { addresses = model.addresses
                    , patterns = model.patterns
                    , search = ""
                    }

              else
                Loading model
            , case addresses of
                [] ->
                    Cmd.none

                first :: rest ->
                    Git.getMeta identity
                        { repo = first.repo
                        , ref = first.ref
                        , onMeta = ReceivedMeta first rest
                        }
            )

        ReceivedMeta address addresses (Ok meta) ->
            let
                newPatterns =
                    { address = address
                    , name = meta.name
                    , description = meta.description
                    , storage =
                        Ui.Molecule.PatternList.Github address.repo.owner address.repo.name
                    , updatedAt = Time.millisToPosix 0
                    }
                        :: model.patterns
            in
            ( if List.isEmpty addresses then
                Loaded
                    { addresses = model.addresses
                    , patterns = newPatterns
                    , search = ""
                    }

              else
                Loading
                    { model | patterns = newPatterns }
            , case addresses of
                [] ->
                    Cmd.none

                first :: rest ->
                    Git.getMeta identity
                        { repo = first.repo
                        , ref = first.ref
                        , onMeta = ReceivedMeta first rest
                        }
            )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    LocalStorage.changedStore
        { changedZoom = \_ _ -> ChangedWhatever
        , changedCenter = \_ _ -> ChangedWhatever
        , changedAddresses = ChangedAddresses
        , changedWhatever = ChangedWhatever
        }
