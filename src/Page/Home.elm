module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import BoundingBox2d
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Design exposing (Grey(..))
import Dict exposing (Dict)
import Draw.Pattern as Pattern
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Pattern exposing (Pattern)
import StoredPattern exposing (StoredPattern)
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Those
import View.Icon


type alias Model =
    { dialog : Dialog }


type Dialog
    = NoDialog
    | CreatePattern String


init : Model
init =
    { dialog = NoDialog }


type Msg
    = NoOp
    | DownloadPatternPressed String
    | DeletePatternPressed String
    | AddPatternClicked
    | NewPatternNameChanged String
    | NewPatternCreateClicked
    | NewPatternCancelClicked


update :
    Navigation.Key
    -> Dict String StoredPattern
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, Maybe (Dict String StoredPattern) )
update key cache msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Nothing )

        DownloadPatternPressed patternSlug ->
            ( model
            , Cmd.none
            , Nothing
            )

        DeletePatternPressed patternSlug ->
            ( model
            , Cmd.none
            , Nothing
            )

        AddPatternClicked ->
            ( { model | dialog = CreatePattern "" }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        NewPatternNameChanged newName ->
            case model.dialog of
                NoDialog ->
                    ( model, Cmd.none, Nothing )

                CreatePattern _ ->
                    ( { model | dialog = CreatePattern newName }
                    , Cmd.none
                    , Nothing
                    )

        NewPatternCreateClicked ->
            case model.dialog of
                NoDialog ->
                    ( model, Cmd.none, Nothing )

                CreatePattern name ->
                    let
                        slug =
                            name
                                |> String.map replaceNonLetter
                                |> String.toLower

                        replaceNonLetter c =
                            if Char.isAlpha c then
                                c

                            else
                                '-'
                    in
                    if Dict.member slug cache then
                        ( model
                        , Cmd.none
                        , Nothing
                        )

                    else
                        ( { model | dialog = NoDialog }
                        , Cmd.none
                        , Just (Dict.insert slug (StoredPattern.init slug name) cache)
                        )

        NewPatternCancelClicked ->
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
                                    Decode.succeed NewPatternCancelClicked

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )


view : List StoredPattern -> Model -> Html Msg
view storedPatterns model =
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
                            { onPress = Just AddPatternClicked
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
                    (List.map viewPattern storedPatterns)
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
                    , Font.size Design.small
                    , Element.mouseOver
                        [ Font.color (Design.toColor Darkish) ]
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

        CreatePattern name ->
            Element.column
                [ Element.centerX
                , Element.width (Element.px 350)
                , Element.padding Design.small
                , Element.spacing Design.small
                , Design.backgroundColor Darkest
                , Design.fontColor Darkest
                ]
                [ Element.text "Create a new pattern"
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
                    { onChange = NewPatternNameChanged
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
                        (button "Create" NewPatternCreateClicked)
                    , Element.el
                        [ Element.alignRight ]
                        (button "Cancel" NewPatternCancelClicked)
                    ]
                ]


viewPattern : StoredPattern -> Element Msg
viewPattern ({ pattern } as storedPattern) =
    let
        selections =
            { points = Those.fromList []
            , lines = Those.fromList []
            , lineSegments = Those.fromList []
            , details = Those.fromList []
            }

        viewBox =
            String.join " "
                [ String.fromFloat (BoundingBox2d.minX boundingBox)
                , String.fromFloat (BoundingBox2d.minY boundingBox)
                , String.fromFloat width
                , String.fromFloat height
                ]

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        boundingBox =
            geometry.points
                |> List.map (\( _, _, p2d ) -> p2d)
                |> BoundingBox2d.containingPoints
                |> Maybe.map scale
                |> Maybe.withDefault
                    (BoundingBox2d.fromExtrema
                        { minX = 0
                        , maxX = 300
                        , minY = 0
                        , maxY = 300
                        }
                    )

        scale box =
            let
                centerPoint =
                    BoundingBox2d.centerPoint box
            in
            BoundingBox2d.scaleAbout centerPoint 2 box

        zoom =
            (max width height) / 300

        ( geometry, _ ) =
            Pattern.geometry pattern
    in
    Element.column
        [ Border.color (Design.toColor Bright)
        , Border.width 1
        ]
        [ Element.row
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
            [ Element.link [ Element.centerX ]
                { url = "/pattern/" ++ storedPattern.slug
                , label =
                    Element.el
                        [ Font.size Design.small
                        , Font.underline
                        , Element.mouseOver
                            [ Font.color (Design.toColor Darkish) ]
                        ]
                        (Element.text storedPattern.name)
                }
            , Element.downloadAs [ Element.alignLeft ]
                { url =
                    "data:application/json;charset=utf-8,"
                        ++ Encode.encode 0 (StoredPattern.encode storedPattern)
                , label =
                    Element.el
                        [ Font.size Design.small
                        , Element.mouseOver
                            [ Font.color (Design.toColor Darkish) ]
                        ]
                        (View.Icon.fa "download")
                , filename = storedPattern.slug ++ ".json"
                }
            ]
        , Element.el
            [ Element.width (Element.px 300)
            , Element.height (Element.px 300)
            , Background.color (Element.rgb 255 255 255)
            ]
            (Element.html <|
                Svg.svg
                    [ Svg.Attributes.viewBox viewBox
                    , Html.Attributes.style "user-select" "none"
                    , Html.Events.preventDefaultOn "dragstart" <|
                        Decode.succeed ( NoOp, True )
                    ]
                    [ Pattern.draw selections zoom Nothing pattern ]
            )
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
            [ Element.el [ Element.alignRight ] <|
                button "Delete" (DeletePatternPressed storedPattern.slug)
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
