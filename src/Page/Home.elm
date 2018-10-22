module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Design exposing (Grey(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Page.Pattern exposing (ViewedPattern)
import Pattern exposing (Pattern)
import Task
import View.Icon


type alias Model =
    { dialog : Dialog }


type Dialog
    = NoDialog
    | CreateProject String


init : Model
init =
    { dialog = NoDialog }


type Msg
    = NoOp
    | DownloadPatternPressed String
    | OpenPatternPressed String
    | DeletePatternPressed String
    | AddProjectClicked
    | NewProjectNameChanged String
    | NewProjectCreateClicked
    | NewProjectCancelClicked


update :
    Navigation.Key
    -> Dict String ViewedPattern
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, Maybe (Dict String ViewedPattern) )
update key cache msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Nothing )

        DownloadPatternPressed patternSlug ->
            ( model
            , Cmd.none
            , Nothing
            )

        OpenPatternPressed patternSlug ->
            ( model
            , Navigation.pushUrl key ("/pattern/" ++ patternSlug)
            , Nothing
            )

        DeletePatternPressed patternSlug ->
            ( model
            , Cmd.none
            , Nothing
            )

        AddProjectClicked ->
            ( { model | dialog = CreateProject "" }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        NewProjectNameChanged newName ->
            case model.dialog of
                NoDialog ->
                    ( model, Cmd.none, Nothing )

                CreateProject _ ->
                    ( { model | dialog = CreateProject newName }
                    , Cmd.none
                    , Nothing
                    )

        NewProjectCreateClicked ->
            case model.dialog of
                NoDialog ->
                    ( model, Cmd.none, Nothing )

                CreateProject name ->
                    if Dict.member name cache then
                        ( model
                        , Cmd.none
                        , Nothing
                        )

                    else
                        ( { model | dialog = NoDialog }
                        , Cmd.none
                        , Just (Dict.insert name Page.Pattern.defaultViewedPattern cache)
                        )

        NewProjectCancelClicked ->
            ( { model | dialog = NoDialog }
            , Cmd.none
            , Nothing
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dialog of
        NoDialog ->
            Sub.none

        _ ->
            Browser.Events.onKeyDown
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            case key of
                                "Escape" ->
                                    Decode.succeed NewProjectCancelClicked

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )


view : List ( String, Pattern ) -> Model -> Html Msg
view namedPatterns model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
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
                , Element.below (viewDialog model.dialog)
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
                            { onPress = Just AddProjectClicked
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
                    , Element.spacing Design.normal
                    ]
                    (List.map viewPattern namedPatterns)
                )
            , Element.row
                [ Element.width Element.fill
                , Element.paddingXY 10 5
                , Element.spacing 5
                , Design.backgroundColor Darkest
                , Design.fontColor Darkest
                ]
                [ Element.newTabLink
                    [ Element.alignRight
                    , Element.padding 5
                    ]
                    { url = "https://github.com/kirchner/sewing-pattern-editor"
                    , label = View.Icon.dev "github-plain"
                    }
                ]
            ]
        )


viewDialog : Dialog -> Element Msg
viewDialog dialog =
    case dialog of
        NoDialog ->
            Element.none

        CreateProject name ->
            Element.column
                [ Element.centerX
                , Element.width (Element.px 350)
                , Element.padding Design.small
                , Element.spacing Design.small
                , Design.backgroundColor Darkest
                , Design.fontColor Darkest
                ]
                [ Element.text "Create a new project"
                , Input.text
                    [ Element.width Element.fill
                    , Element.padding 5
                    , Font.size 16
                    , Design.fontColor Darkish
                    , Design.backgroundColor Darkish
                    , Border.width 1
                    , Border.color (Design.toColor Brightish)
                    , Element.htmlAttribute <|
                        Html.Attributes.id "name-input"
                    ]
                    { onChange = NewProjectNameChanged
                    , text = name
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove
                            [ Font.size 12
                            , Font.variant Font.smallCaps
                            , Design.fontColor Darkest
                            ]
                            (Element.text "choose a name")
                    }
                , Element.row
                    [ Element.width Element.fill ]
                    [ Element.el
                        [ Element.alignLeft ]
                        (button "Create" NewProjectCreateClicked)
                    , Element.el
                        [ Element.alignRight ]
                        (button "Cancel" NewProjectCancelClicked)
                    ]
                ]


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
