module Page.PatternNew exposing
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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy as Element
import Github
import Http
import LocalStorage
import Pattern exposing (Pattern)
import RemoteData exposing (WebData)
import Route
import Session exposing (Session)
import String.Extra as String
import Ui.Atom.Input
import Ui.Molecule.TopBar
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- MODEL


{-| -}
type Model
    = Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { session : Session
    , parameters : Route.NewParameters
    , user : WebData Github.User
    }


type alias LoadedData =
    { session : Session
    , owner : Maybe String

    -- FORM
    , name : String
    , description : String
    , storageSolution : StorageSolutionTag
    , slug : Generated
    , repositoryName : String
    , visibility : Visibility

    -- NEW ADDRESS
    , newAddress : Maybe LocalStorage.Address
    }


type Generated
    = Generated String
    | Modified String


generatedToString : Generated -> String
generatedToString generated =
    case generated of
        Generated text ->
            text

        Modified text ->
            text


type Visibility
    = Public
    | Private


visibilityFromString : String -> Maybe Visibility
visibilityFromString string =
    case string of
        "public" ->
            Just Public

        "private" ->
            Just Private

        _ ->
            Nothing


visibilityToString : Visibility -> String
visibilityToString visibility =
    case visibility of
        Public ->
            "public"

        Private ->
            "private"


{-| -}
init : Session -> Github.Cred -> Route.NewParameters -> ( Model, Cmd Msg )
init session cred newParameters =
    case cred of
        Github.Anonymous ->
            initLoaded session newParameters Nothing

        Github.OauthToken _ ->
            ( Loading
                { session = session
                , parameters = newParameters
                , user = RemoteData.Loading
                }
            , Github.getAuthenticatedUser cred
                { onUser = RemoteData.fromResult >> ReceivedAuthenticatedUser }
            )


initLoaded : Session -> Route.NewParameters -> Maybe String -> ( Model, Cmd Msg )
initLoaded session newParameters owner =
    ( Loaded
        { session = session
        , owner = owner
        , name = Maybe.withDefault "" newParameters.name
        , description = Maybe.withDefault "" newParameters.description
        , storageSolution =
            newParameters.storageSolution
                |> Maybe.andThen storageSolutionFromString
                |> Maybe.withDefault BrowserTag
        , slug = Maybe.withDefault (Generated "") (Maybe.map Modified newParameters.slug)
        , repositoryName = Maybe.withDefault "" newParameters.repositoryName
        , visibility =
            newParameters.visibility
                |> Maybe.andThen visibilityFromString
                |> Maybe.withDefault Public
        , newAddress = Nothing
        }
    , Cmd.none
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
view :
    Element.Device
    -> Github.Cred
    -> Model
    -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view device cred model =
    { title = "Create a new pattern"
    , body =
        case model of
            Loading _ ->
                Element.none

            Loaded data ->
                viewNew device cred data
    , dialog = Nothing
    }


viewNew : Element.Device -> Github.Cred -> LoadedData -> Element Msg
viewNew device cred model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level4
        ]
        [ Ui.Molecule.TopBar.view
            { userPressedSignIn = UserPressedSignIn
            , cred = cred
            , device = device
            , heading = "Create a new pattern"
            , backToLabel = Just "Back to patterns"
            }
        , viewContent cred model
        ]


viewContent : Github.Cred -> LoadedData -> Element Msg
viewContent cred model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.centerX
        , Element.width
            (Element.fill
                |> Element.maximum 780
            )
        , Element.paddingEach
            { top = Ui.Theme.Spacing.level1
            , bottom = Ui.Theme.Spacing.level8
            , left = Ui.Theme.Spacing.level1
            , right = Ui.Theme.Spacing.level1
            }
        ]
        [ Element.el [ Element.padding 7 ] <|
            Ui.Theme.Typography.paragraphBody
                [ Element.text
                    """A pattern contains all the information on how all
                    details needed for a certain piece of clothing can be
                    constructed from a set of body measurements. If you already
                    have a pattern you can also """
                , Element.el
                    [ Element.inFront <|
                        Element.link
                            [ Border.width 2
                            , Border.rounded 3
                            , Element.moveLeft 2
                            , Element.moveUp 2
                            , Element.paddingEach
                                { top = 2
                                , bottom = 2
                                , left = 0
                                , right = 0
                                }
                            , Border.color Ui.Theme.Color.transparent
                            , Font.color Ui.Theme.Color.transparent
                            , Element.focused
                                [ Border.color Ui.Theme.Color.complementary ]
                            ]
                            { url = "/import"
                            , label = Element.text "import it"
                            }
                    , Font.color Ui.Theme.Color.primary
                    , Element.mouseOver
                        [ Font.color Ui.Theme.Color.primaryDark ]
                    ]
                    (Element.text "import it")
                , Element.text "."
                ]
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.width Element.fill
            ]
            [ Ui.Atom.Input.text
                { id = "pattern-name-input"
                , text = model.name
                , onChange = UserChangedName
                , label = "Name"
                , help = Nothing
                }
            , Ui.Atom.Input.text
                { id = "pattern-decription-input"
                , text = model.description
                , onChange = UserChangedDescription
                , label = "Description (optional)"
                , help = Nothing
                }
            ]
        , horizontalRule
        , Ui.Atom.Input.radioColumn
            { id = "storage-radio-group"
            , onChange = UserChangedStorageSolution
            , selected = Just model.storageSolution
            , label = "Storage solution"
            , options =
                [ Ui.Atom.Input.option
                    { value = BrowserTag
                    , label = "Browser cache"
                    , child =
                        Ui.Theme.Typography.paragraphButton
                            [ Element.text
                                """Store all data locally in the cache of the
                                browser you are currently using. Note, that you
                                will not have access to this pattern from
                                a different browser or machine. You will loose
                                all data when you clear the browser cache."""
                            ]
                    }
                , Ui.Atom.Input.option
                    { value = GithubTag
                    , label = "GitHub repository"
                    , child =
                        Ui.Theme.Typography.paragraphButton
                            [ Element.text
                                """Store all data within a GitHub repository.
                                You will need to sign in with your GitHub
                                account and authorize the sewing pattern editor
                                to create and modify repositories within your
                                GitHub account."""
                            ]
                    }
                ]
            }
        , case model.storageSolution of
            BrowserTag ->
                Ui.Atom.Input.text
                    { id = "localstorage-slug-input"
                    , text = generatedToString model.slug
                    , onChange = UserChangedSlug
                    , label = "Slug"
                    , help = Nothing
                    }

            GithubTag ->
                case cred of
                    Github.Anonymous ->
                        Element.el [] <|
                            Ui.Atom.Input.btnPrimary
                                { id = "log-in-btn"
                                , onPress = Just UserPressedSignIn
                                , label = "Sign in via GitHub"
                                }

                    Github.OauthToken _ ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Theme.Spacing.level2
                            ]
                            [ Element.row
                                [ Element.width Element.fill
                                , Element.spacing Ui.Theme.Spacing.level1
                                ]
                                [ Element.row
                                    [ Element.alignBottom
                                    , Element.spacing Ui.Theme.Spacing.level2
                                    , Element.paddingEach
                                        { top = 17
                                        , bottom = 17
                                        , left = 7
                                        , right = 2
                                        }
                                    ]
                                    [ Ui.Theme.Typography.body <|
                                        Maybe.withDefault "" model.owner
                                    , Ui.Theme.Typography.body "/"
                                    ]
                                , Ui.Atom.Input.text
                                    { id = "github-repository-name-input"
                                    , text = model.repositoryName
                                    , onChange = UserChangedRepositoryName
                                    , label = "Repository name"
                                    , help = Nothing
                                    }
                                ]
                            , Ui.Atom.Input.radioColumn
                                { id = "visibility-radio-group"
                                , onChange = UserChangedVisibility
                                , selected = Just model.visibility
                                , label = "Visibility"
                                , options =
                                    [ Ui.Atom.Input.option
                                        { value = Public
                                        , label = "Public"
                                        , child = Element.none
                                        }
                                    , Ui.Atom.Input.option
                                        { value = Private
                                        , label = "Private"
                                        , child = Element.none
                                        }
                                    ]
                                }
                            ]
        , horizontalRule
        , Element.el [ Element.width Element.shrink ] <|
            Ui.Atom.Input.btnPrimary
                { id = "create-pattern-btn"
                , onPress = Just UserPressedCreate
                , label = "Create pattern"
                }
        ]


horizontalRule : Element msg
horizontalRule =
    Element.el
        [ Element.width Element.fill
        , Element.paddingXY 7 0
        ]
        (Element.el
            [ Element.width Element.fill
            , Element.height (Element.px 1)
            , Background.color Ui.Theme.Color.secondary
            ]
            Element.none
        )



---- UPDATE


{-| -}
type Msg
    = ReceivedAuthenticatedUser (WebData Github.User)
    | UserChangedName String
    | UserChangedDescription String
    | UserChangedStorageSolution StorageSolutionTag
    | UserChangedSlug String
    | UserPressedSignIn
    | UserChangedRepositoryName String
    | UserChangedVisibility Visibility
    | UserPressedCreate
    | ReceivedRepository String String Github.Repo (Result Http.Error Github.Repository)
    | ReceivedShaOfMeta Github.Repo (Result Http.Error String)
    | ReceivedShaOfPattern Github.Repo (Result Http.Error String)
    | ChangedWhatever
    | ChangedPattern LocalStorage.Address (Pattern ())
    | ChangedMeta LocalStorage.Address Github.Meta
    | ChangedAddresses (List LocalStorage.Address)


type StorageSolutionTag
    = BrowserTag
    | GithubTag


storageSolutionToString : StorageSolutionTag -> String
storageSolutionToString storageSolution =
    case storageSolution of
        BrowserTag ->
            "browser"

        GithubTag ->
            "github"


storageSolutionFromString : String -> Maybe StorageSolutionTag
storageSolutionFromString string =
    case string of
        "browser" ->
            Just BrowserTag

        "github" ->
            Just GithubTag

        _ ->
            Nothing


{-| -}
update : String -> Github.Cred -> Msg -> Model -> ( Model, Cmd Msg )
update clientId cred msg model =
    case model of
        Loading data ->
            case msg of
                ReceivedAuthenticatedUser user ->
                    { data | user = user }
                        |> checkLoaded

                _ ->
                    ( model, Cmd.none )

        Loaded data ->
            updateLoaded clientId cred msg data
                |> Tuple.mapFirst Loaded


checkLoaded : LoadingData -> ( Model, Cmd Msg )
checkLoaded data =
    case data.user of
        RemoteData.Success user ->
            initLoaded data.session data.parameters (Just user.login)

        RemoteData.NotAsked ->
            initLoaded data.session data.parameters Nothing

        _ ->
            ( Loading data
            , Cmd.none
            )


updateLoaded : String -> Github.Cred -> Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateLoaded clientId cred msg model =
    case msg of
        ReceivedAuthenticatedUser _ ->
            ( model, Cmd.none )

        UserChangedName newName ->
            ( { model
                | name = newName
                , slug =
                    case model.slug of
                        Generated _ ->
                            Generated (nameToSlug newName)

                        Modified _ ->
                            model.slug
              }
            , Cmd.none
            )

        UserChangedDescription newDescription ->
            ( { model | description = newDescription }
            , Cmd.none
            )

        UserChangedStorageSolution newStorageSolution ->
            ( { model | storageSolution = newStorageSolution }
            , Cmd.none
            )

        UserPressedSignIn ->
            ( model
            , Github.requestAuthorization clientId <|
                Route.crossOrigin (Session.domain model.session)
                    (Route.PatternNew
                        { name = Just model.name
                        , description = Just model.description
                        , storageSolution =
                            Just (storageSolutionToString model.storageSolution)
                        , slug = Just (generatedToString model.slug)
                        , repositoryName = Just model.repositoryName
                        , visibility = Just (visibilityToString model.visibility)
                        }
                    )
                    []
            )

        UserChangedSlug newSlug ->
            ( { model | slug = Modified newSlug }
            , Cmd.none
            )

        UserChangedRepositoryName newRepositoryName ->
            ( { model | repositoryName = newRepositoryName }
            , Cmd.none
            )

        UserChangedVisibility newVisibility ->
            ( { model | visibility = newVisibility }
            , Cmd.none
            )

        UserPressedCreate ->
            ( model
            , case model.storageSolution of
                BrowserTag ->
                    LocalStorage.updatePattern
                        (LocalStorage.Browser { slug = generatedToString model.slug })
                        newPattern

                GithubTag ->
                    case model.owner of
                        Nothing ->
                            Cmd.none

                        Just owner ->
                            let
                                repo =
                                    { owner = owner
                                    , name = model.repositoryName
                                    }

                                route =
                                    Route.Pattern
                                        (LocalStorage.GithubRepo
                                            { repo = repo
                                            , ref = Github.defaultRef
                                            }
                                        )
                            in
                            Github.createRepository cred
                                { repository =
                                    { name = model.repositoryName
                                    , description = model.description
                                    , homepage = Route.crossOrigin (Session.domain model.session) route []
                                    , private =
                                        case model.visibility of
                                            Public ->
                                                False

                                            Private ->
                                                True
                                    }
                                , onRepository =
                                    ReceivedRepository model.name model.description repo
                                }
            )

        ReceivedRepository _ _ _ (Err _) ->
            ( model, Cmd.none )

        ReceivedRepository name description repo (Ok _) ->
            ( model
            , Github.putMeta cred
                { repo = repo
                , message = "create meta.json"
                , meta =
                    { name = name
                    , description = description
                    }
                , sha = ""
                , onSha = ReceivedShaOfMeta repo
                }
            )

        ReceivedShaOfMeta _ (Err _) ->
            ( model, Cmd.none )

        ReceivedShaOfMeta repo (Ok _) ->
            ( model
            , Github.putPattern cred
                { repo = repo
                , message = "create pattern.json"
                , pattern = newPattern
                , sha = ""
                , onSha = ReceivedShaOfPattern repo
                }
            )

        ReceivedShaOfPattern _ (Err _) ->
            ( model, Cmd.none )

        ReceivedShaOfPattern repo (Ok _) ->
            ( { model
                | newAddress =
                    Just <|
                        LocalStorage.GithubRepo
                            { repo = repo
                            , ref = Github.defaultRef
                            }
              }
            , LocalStorage.requestAddresses
            )

        ChangedPattern address _ ->
            ( model
            , LocalStorage.updateMeta address
                { name = model.name
                , description = model.description
                }
            )

        ChangedMeta address _ ->
            ( { model | newAddress = Just address }
            , LocalStorage.requestAddresses
            )

        ChangedAddresses addresses ->
            case model.newAddress of
                Nothing ->
                    ( model, Cmd.none )

                Just address ->
                    ( model
                    , Cmd.batch
                        [ Route.pushUrl (Session.navKey model.session) (Route.Pattern address)
                        , LocalStorage.updateAddresses (address :: addresses)
                        ]
                    )

        ChangedWhatever ->
            ( model, Cmd.none )


{-| -}
subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.changedStore
        { changedZoom = \_ _ -> ChangedWhatever
        , changedCenter = \_ _ -> ChangedWhatever
        , changedAddresses = ChangedAddresses
        , changedPattern = ChangedPattern
        , changedMeta = ChangedMeta
        , changedWhatever = ChangedWhatever
        }



---- HELPER


nameToSlug : String -> String
nameToSlug =
    String.toLower >> String.dasherize


newPattern : Pattern coordinates
newPattern =
    case Pattern.insertPoint "Origin" (Pattern.origin 0 0) Pattern.empty of
        Err _ ->
            Pattern.empty

        Ok pattern ->
            pattern
