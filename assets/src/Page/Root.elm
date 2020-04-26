module Page.Root exposing
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
import Http
import Json.Encode as Encode
import Session exposing (Session)
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- MODEL


{-| -}
type Model
    = Loaded LoadedData


type alias LoadedData =
    { session : Session
    }


{-| -}
init : Session -> ( Model, Cmd Msg )
init session =
    ( Loaded { session = session }
    , Cmd.none
    )


{-| -}
toSession : Model -> Session
toSession model =
    case model of
        Loaded { session } ->
            session



---- VIEW


{-| -}
view : Model -> { title : String, body : Element Msg, dialog : Maybe (Element Msg) }
view (Loaded data) =
    { title = "SewingLab"
    , body = viewBody data
    , dialog = Nothing
    }


viewBody : LoadedData -> Element Msg
viewBody data =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing Ui.Theme.Spacing.level6
        , Background.color Ui.Theme.Color.secondary
        ]
        [ Element.el
            [ Element.width Element.fill
            , Background.color Ui.Theme.Color.complementaryLight
            ]
            (Element.el
                [ Element.centerX
                , Element.width (Element.fill |> Element.maximum 860)
                , Element.paddingXY 0 Ui.Theme.Spacing.level7
                ]
                (Ui.Theme.Typography.headingOne "SewingLab")
            )
        , Element.column
            [ Element.width (Element.fill |> Element.maximum 860)
            , Element.height Element.fill
            , Element.centerX
            , Element.spacing Ui.Theme.Spacing.level8
            ]
            [ Element.row
                [ Element.spacing Ui.Theme.Spacing.level3 ]
                [ infoBlock
                , loginBlock
                ]
            ]
        ]


infoBlock : Element msg
infoBlock =
    Element.column
        [ Element.width (Element.fillPortion 1)
        , Element.spacing Ui.Theme.Spacing.level3
        ]
        [ Ui.Theme.Typography.headingTwo "A place for sewing patterns"
        , Ui.Theme.Typography.paragraphBody
            [ Element.text "SewingLab is a platform for creating customizable sewing patterns and sharing them with other people. Create bespoke clothing with patterns which are dynamically generated from body measurements."
            ]
        ]


loginBlock : Element Msg
loginBlock =
    Element.column
        [ Element.width (Element.fillPortion 1)
        , Background.color Ui.Theme.Color.white
        , Element.padding Ui.Theme.Spacing.level3
        , Element.spacing Ui.Theme.Spacing.level2
        , Border.rounded 6
        , Border.width 1
        , Border.color Ui.Theme.Color.black
        ]
        [ Element.el [ Element.centerX ] (Ui.Theme.Typography.bodyBold "Join SewingLab")
        , Element.column
            [ Element.width Element.fill ]
            [ Ui.Atom.Input.btnProviderFill
                { id = "login-with-github"
                , onPress = Just UserPressedLoginWithGithub
                , icon = "github"
                , label = "Log in with GitHub"
                }
            , Ui.Atom.Input.btnProviderFill
                { id = "login-with-twitter"
                , onPress = Just UserPressedLoginWithTwitter
                , icon = "twitter"
                , label = "Log in with Twitter"
                }
            ]
        , Element.el [ Element.centerX ] <|
            Ui.Theme.Typography.paragraphBody
                [ Element.text "We require social login to prevent abuse." ]
        ]



---- UPDATE


{-| -}
type Msg
    = UserPressedLoginWithGithub
    | UserPressedLoginWithTwitter


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loaded data ->
            Tuple.mapFirst Loaded (updateLoaded msg data)


updateLoaded : Msg -> LoadedData -> ( LoadedData, Cmd Msg )
updateLoaded msg data =
    case msg of
        UserPressedLoginWithGithub ->
            ( data
            , Browser.Navigation.load "/auth/github"
            )

        UserPressedLoginWithTwitter ->
            ( data
            , Browser.Navigation.load "/auth/twitter"
            )


{-| -}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
