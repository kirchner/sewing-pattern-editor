module Page.NewPatterns exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import LocalStorage
import Time
import Ui.Molecule.PatternList
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- MODEL


type alias Model =
    { addresses : List LocalStorage.Address
    , search : String
    }


init : ( Model, Cmd Msg )
init =
    ( { addresses = []
      , search = ""
      }
    , LocalStorage.requestAddresses
    )



---- VIEW


view : Model -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view model =
    { title = "Patterns"
    , body = viewPatterns model
    , dialog = Nothing
    }


viewPatterns : Model -> Element Msg
viewPatterns model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level4
        ]
        [ viewTopBar
        , viewContent model
        ]


viewTopBar : Element Msg
viewTopBar =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px (2 * Ui.Theme.Spacing.level6))
        , Background.color Ui.Theme.Color.secondary
        ]
        []


viewContent : Model -> Element Msg
viewContent model =
    let
        toPatternInfo address =
            { name = "TODO"
            , description = "TODO"
            , storage = Ui.Molecule.PatternList.Github address.repo.owner address.repo.name
            , updatedAt = Time.millisToPosix 0
            , onClone = UserPressedClone address
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
        [ Element.el [ Element.padding 7 ] <|
            Ui.Theme.Typography.headingOne "Patterns"
        , Ui.Molecule.PatternList.view
            { search = model.search
            , onSearchChange = UserChangedSearch
            , onImport = UserPressedImport
            , onCreate = UserPressedCreate
            , patternInfos = List.map toPatternInfo model.addresses
            , now = Time.millisToPosix 0
            }
        ]



---- UPDATE


type Msg
    = UserChangedSearch String
    | UserPressedImport
    | UserPressedCreate
    | ChangedAddresses (List LocalStorage.Address)
    | ChangedWhatever
    | UserPressedClone LocalStorage.Address


update : Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
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

        ChangedAddresses newAddresses ->
            ( { model | addresses = newAddresses }
            , Cmd.none
            )

        ChangedWhatever ->
            ( model, Cmd.none )

        UserPressedClone address ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    LocalStorage.changedStore
        { changedZoom = \_ _ -> ChangedWhatever
        , changedCenter = \_ _ -> ChangedWhatever
        , changedAddresses = ChangedAddresses
        , changedWhatever = ChangedWhatever
        }
