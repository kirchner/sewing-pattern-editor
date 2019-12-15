module Page.NewPatterns exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Time
import Ui.Molecule.PatternList
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



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
            { search = ""
            , onSearchChange = \_ -> NoOp
            , onImport = NoOp
            , onCreate = UserPressedCreate
            , patternInfos = []
            , now = Time.millisToPosix 0
            }
        ]



---- UPDATE


type Msg
    = NoOp
    | UserPressedCreate


update : Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UserPressedCreate ->
            ( model
            , Browser.Navigation.pushUrl key "/new"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
