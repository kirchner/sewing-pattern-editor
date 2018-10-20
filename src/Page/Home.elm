module Page.Home exposing (Msg, update, view)

import Browser.Navigation as Navigation
import Design exposing (Grey(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Pattern exposing (Pattern)
import View.Icon


type Msg
    = DownloadPatternPressed String
    | OpenPatternPressed String
    | DeletePatternPressed String


update : Navigation.Key -> Msg -> Cmd msg
update key msg =
    case msg of
        DownloadPatternPressed patternSlug ->
            Cmd.none

        OpenPatternPressed patternSlug ->
            Navigation.pushUrl key ("/pattern/" ++ patternSlug)

        DeletePatternPressed patternSlug ->
            Cmd.none


view : List ( String, Pattern ) -> Html Msg
view namedPatterns =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Just (Design.toColor Brightest)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.family
            [ Font.external
                { name = "Roboto"
                , url = "https://fonts.googleapis.com/css?family=Roboto"
                }
            , Font.sansSerif
            ]
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.padding Design.large
                , Design.backgroundColor Darkest
                , Design.fontColor Darkest
                , Element.inFront <|
                    Element.el
                        [ Element.alignBottom
                        , Element.alignRight
                        , Element.htmlAttribute <|
                            Html.Attributes.style "z-index" "1000"
                        ]
                        (Input.button
                            [ Design.backgroundColor Brightish
                            , Design.fontColor Brightish
                            , Element.moveDown 30
                            , Element.moveLeft (toFloat Design.large)
                            , Element.width (Element.px 60)
                            , Element.height (Element.px 60)
                            , Border.rounded 30
                            , Element.mouseOver
                                [ Design.backgroundColor Bright
                                , Design.fontColor Bright
                                ]
                            ]
                            { onPress = Nothing
                            , label =
                                Element.el
                                    [ Element.centerX
                                    , Element.centerY
                                    ]
                                    (View.Icon.faLarge "plus")
                            }
                        )
                ]
                [ Element.el
                    [ Font.size Design.normal
                    ]
                    (Element.text "Patterns")
                ]
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Design.backgroundColor Brightest
                , Design.fontColor Brightest
                ]
                (Element.row
                    [ Element.width Element.fill
                    , Element.padding Design.large
                    ]
                    (List.map viewPattern namedPatterns)
                )
            , Element.row
                [ Element.width Element.fill
                , Element.height (Element.px 40)
                , Element.paddingXY 10 5
                , Element.spacing 5
                , Design.backgroundColor Darkest
                , Design.fontColor Darkest
                ]
                [ Element.newTabLink
                    [ Element.alignRight
                    , Element.paddingXY 5 5
                    ]
                    { url = "https://github.com/kirchner/sewing-pattern-editor"
                    , label = View.Icon.dev "github-plain"
                    }
                ]
            ]
        )


viewPattern : ( String, Pattern ) -> Element Msg
viewPattern ( patternSlug, pattern ) =
    Element.column
        [ Element.width (Element.px 300)
        , Element.height (Element.px 400)
        , Border.color (Design.toColor Bright)
        , Border.width 1
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.padding Design.small
            , Design.backgroundColor Darkest
            , Design.fontColor Darkest
            , Border.color (Design.toColor Darkest)
            , Border.widthEach
                { left = 0
                , right = 0
                , top = 0
                , bottom = 1
                }
            ]
            (Element.el [ Element.centerX ]
                (Element.text patternSlug)
            )
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color (Element.rgb 255 255 255)
            ]
            Element.none
        , Element.row
            [ Element.width Element.fill
            , Element.padding Design.xSmall
            , Element.spacing Design.xSmall
            , Design.backgroundColor Darkest
            , Design.fontColor Darkest
            , Border.color (Design.toColor Bright)
            , Border.widthEach
                { left = 0
                , right = 0
                , top = 1
                , bottom = 0
                }
            ]
            [ Element.el [ Element.alignLeft ] <|
                button "Export" (DownloadPatternPressed patternSlug)
            , Element.el [ Element.alignRight ] <|
                button "Open" (OpenPatternPressed patternSlug)
            , Element.el [ Element.alignRight ] <|
                button "Delete" (DeletePatternPressed patternSlug)
            ]
        ]


button label msg =
    Input.button
        [ Element.padding Design.xSmall
        , Design.backgroundColor Dark
        , Design.fontColor Dark
        , Border.width 1
        , Border.color (Design.toColor Dark)
        , Element.mouseOver
            [ Design.backgroundColor Darkish
            , Design.fontColor Darkish
            ]
        ]
        { onPress = Just msg
        , label =
            Element.el
                [ Font.size Design.small ]
                (Element.text label)
        }
