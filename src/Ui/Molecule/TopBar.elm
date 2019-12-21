module Ui.Molecule.TopBar exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Git
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography


type alias Config msg =
    { userPressedSignIn : msg
    , identity : Git.Identity
    , device : Element.Device
    , backToLabel : Maybe String
    , heading : String
    }


view : Config msg -> Element msg
view cfg =
    let
        backToPatternsLink =
            case cfg.backToLabel of
                Nothing ->
                    Element.none

                Just backToLabel ->
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
                                    , Ui.Theme.Typography.body backToLabel
                                    ]
                            }

        signInViaGithubBtn =
            case cfg.identity of
                Git.Anonymous ->
                    Ui.Atom.Input.btnPrimary
                        { id = "sign-in-btn"
                        , onPress = Just cfg.userPressedSignIn
                        , label = "Sign in via GitHub"
                        }

                Git.OauthToken _ ->
                    Element.none

        heading =
            Element.el
                [ Element.centerY ]
                (Ui.Theme.Typography.headingOne cfg.heading)

        -- COMPACT
        compact =
            Element.column
                [ Element.width Element.fill
                , Element.padding Ui.Theme.Spacing.level1
                , Element.spacing Ui.Theme.Spacing.level1
                , Background.color Ui.Theme.Color.secondary
                ]
                [ Element.wrappedRow
                    [ Element.width Element.fill ]
                    [ Element.el []
                        backToPatternsLink
                    , Element.el [ Element.alignRight ]
                        signInViaGithubBtn
                    ]
                , Element.el
                    [ Element.paddingEach
                        { top = 0
                        , bottom = Ui.Theme.Spacing.level2
                        , left = 0
                        , right = 0
                        }
                    ]
                    heading
                ]

        -- FULLSCREEN
        fullscreen =
            Element.row
                [ Element.width Element.fill
                , Element.padding Ui.Theme.Spacing.level2
                , Background.color Ui.Theme.Color.secondary
                ]
                [ Element.el
                    [ Element.width Element.fill ]
                    backToPatternsLink
                , Element.el
                    [ Element.centerX ]
                    (Element.el
                        [ Element.width
                            (Element.fill
                                |> Element.maximum 780
                            )
                        ]
                        heading
                    )
                , Element.el
                    [ Element.width Element.fill ]
                    (Element.el
                        [ Element.alignRight ]
                        signInViaGithubBtn
                    )
                ]
    in
    case ( cfg.device.class, cfg.device.orientation ) of
        ( Element.Phone, _ ) ->
            compact

        ( Element.Tablet, Element.Portrait ) ->
            compact

        _ ->
            fullscreen