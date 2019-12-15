module Page.New exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Git
import Http
import Pattern
import Route exposing (Route)
import String.Extra as String
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Url.Builder



---- MODEL


type Model
    = Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { parameters : Route.NewParameters
    }


type alias LoadedData =
    { owner : Maybe String

    -- FORM
    , name : String
    , description : String
    , storageSolution : StorageSolutionTag
    , slug : Generated
    , repositoryName : String
    , visibility : Visibility
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


init : Git.Identity -> Route.NewParameters -> ( Model, Cmd Msg )
init identity newParameters =
    case identity of
        Git.Anonymous ->
            initLoaded newParameters Nothing

        Git.OauthToken _ ->
            ( Loading
                { parameters = newParameters }
            , Git.getAuthenticatedUser identity
                { onUser = ReceivedAuthenticatedUser }
            )


initLoaded : Route.NewParameters -> Maybe String -> ( Model, Cmd Msg )
initLoaded newParameters owner =
    ( Loaded
        { owner = owner
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
        }
    , Cmd.none
    )



---- VIEW


view :
    Git.Identity
    -> Model
    -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view identity model =
    { title = "Create a new pattern"
    , body =
        case model of
            Loading _ ->
                Element.none

            Loaded data ->
                viewNew identity data
    , dialog = Nothing
    }


viewNew : Git.Identity -> LoadedData -> Element Msg
viewNew identity model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level4
        ]
        [ viewTopBar
        , viewContent identity model
        ]


viewTopBar : Element Msg
viewTopBar =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px (2 * Ui.Theme.Spacing.level6))
        , Background.color Ui.Theme.Color.secondary
        ]
        [ Element.el [ Element.padding Ui.Theme.Spacing.level4 ] <|
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
                            , Ui.Theme.Typography.body "Back to patterns"
                            ]
                    }
        ]


viewContent : Git.Identity -> LoadedData -> Element Msg
viewContent identity model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.centerX
        , Element.width
            (Element.fill
                |> Element.maximum 780
            )
        ]
        [ Element.el [ Element.padding 7 ] <|
            Ui.Theme.Typography.headingOne "Create a new pattern"
        , Element.el [ Element.padding 7 ] <|
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
                                You will need to log in with your GitHub
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
                case identity of
                    Git.Anonymous ->
                        Element.el [] <|
                            Ui.Atom.Input.btnPrimary
                                { id = "log-in-btn"
                                , onPress = Just UserPressedLogIn
                                , label = "Log in to GitHub"
                                }

                    Git.OauthToken _ ->
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
        , Element.el [ Element.width Element.shrink ] <|
            Ui.Atom.Input.btnPrimary
                { id = "create-pattern-btn"
                , onPress = Just UserPressedCreate
                , label = "Create pattern"
                }
        ]



---- UPDATE


type Msg
    = ReceivedAuthenticatedUser (Result Http.Error Git.User)
    | UserChangedName String
    | UserChangedDescription String
    | UserChangedStorageSolution StorageSolutionTag
    | UserChangedSlug String
    | UserPressedLogIn
    | UserChangedRepositoryName String
    | UserChangedVisibility Visibility
    | UserPressedCreate
    | ReceivedRepository String String Git.Repo (Result Http.Error Git.Repository)
    | ReceivedShaOfMeta Git.Repo (Result Http.Error String)
    | ReceivedShaOfPattern Git.Repo (Result Http.Error String)


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
            case msg of
                ReceivedAuthenticatedUser (Err httpError) ->
                    ( model, Cmd.none )

                ReceivedAuthenticatedUser (Ok user) ->
                    initLoaded data.parameters (Just user.login)

                _ ->
                    ( model, Cmd.none )

        Loaded data ->
            updateLoaded key domain clientId identity msg data
                |> Tuple.mapFirst Loaded


updateLoaded :
    Browser.Navigation.Key
    -> String
    -> String
    -> Git.Identity
    -> Msg
    -> LoadedData
    -> ( LoadedData, Cmd Msg )
updateLoaded key domain clientId identity msg model =
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

        UserPressedLogIn ->
            ( model
            , Git.requestAuthorization clientId <|
                Route.crossOrigin domain
                    (Route.New
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
                    Cmd.none

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
                                    Route.GitHub repo Git.defaultRef
                            in
                            Git.createRepository identity
                                { repository =
                                    { name = model.repositoryName
                                    , description = model.description
                                    , homepage = Route.crossOrigin domain route []
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

        ReceivedRepository _ _ _ (Err httpError) ->
            ( model, Cmd.none )

        ReceivedRepository name description repo (Ok repository) ->
            ( model
            , Git.putMeta identity
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

        ReceivedShaOfMeta _ (Err httpError) ->
            ( model, Cmd.none )

        ReceivedShaOfMeta repo (Ok _) ->
            ( model
            , Git.putPattern identity
                { repo = repo
                , message = "create pattern.json"
                , pattern =
                    case Pattern.insertPoint "Origin" (Pattern.origin 0 0) Pattern.empty of
                        Err _ ->
                            Pattern.empty

                        Ok pattern ->
                            pattern
                , sha = ""
                , onSha = ReceivedShaOfPattern repo
                }
            )

        ReceivedShaOfPattern _ (Err httpError) ->
            ( model, Cmd.none )

        ReceivedShaOfPattern repo (Ok _) ->
            ( model
            , Route.pushUrl key (Route.GitHub repo Git.defaultRef)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- HELPER


nameToSlug : String -> String
nameToSlug =
    String.toLower >> String.dasherize
