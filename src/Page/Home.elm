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
import Color
import Design exposing (Grey(..))
import Dict exposing (Dict)
import Draw.Pattern as Pattern
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Frame2d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Pattern exposing (Pattern)
import Route
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
    | PatternCardClicked String
    | PatternCardMenuClicked String
    | DownloadPatternPressed String
    | DeletePatternPressed String
    | AddPatternClicked
    | NewPatternNameChanged String
    | NewPatternCreateClicked
    | NewPatternCancelClicked


update :
    String
    -> Navigation.Key
    -> Dict String StoredPattern
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, Maybe (Dict String StoredPattern) )
update prefix key cache msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Nothing )

        PatternCardClicked slug ->
            ( model
            , Navigation.pushUrl key <|
                Route.toString prefix (Route.Editor slug Nothing)
            , Nothing
            )

        PatternCardMenuClicked slug ->
            ( model
            , Cmd.none
            , Nothing
            )

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


view : String -> List StoredPattern -> Model -> Html Msg
view prefix storedPatterns model =
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
                , Element.paddingXY Design.xLarge Design.normal
                , Design.backgroundColor Darkest
                , Design.fontColor Darkest
                , Element.below (viewDialog model.dialog)
                ]
                [ Element.el
                    [ Font.size Design.normal
                    , Element.alignLeft
                    ]
                    (Element.text "Your patterns")
                , Element.row
                    [ Element.spacing Design.xxxSmall
                    , Element.alignRight
                    ]
                    [ Input.button
                        [ Element.paddingXY Design.small Design.xSmall
                        , Font.size 18
                        , Background.color gray800
                        , Font.color white
                        , Element.mouseOver
                            [ Background.color gray700 ]
                        ]
                        { onPress = Just AddPatternClicked
                        , label = Element.text "Create new pattern"
                        }
                    , Input.button
                        [ Element.padding Design.xSmall
                        , Background.color gray800
                        , Font.color white
                        , Element.mouseOver
                            [ Background.color gray700 ]
                        ]
                        { onPress = Nothing
                        , label = View.Icon.faMedium "ellipsis-h"
                        }
                    ]
                ]
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Design.backgroundColor Brightest
                , Design.fontColor Brightest
                ]
                (Element.wrappedRow
                    [ Element.width Element.fill
                    , Element.paddingXY Design.xLarge Design.large
                    , Element.spacing Design.normal
                    ]
                    (List.map (viewPattern prefix) storedPatterns)
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


viewPattern : String -> StoredPattern -> Element Msg
viewPattern prefix ({ pattern } as storedPattern) =
    let
        selections =
            { points = Those.fromList []
            , lines = Those.fromList []
            , lineSegments = Those.fromList []
            , details = Those.fromList []
            }

        viewBox =
            String.join " "
                [ String.fromFloat
                    (BoundingBox2d.minX boundingBox
                        - max 0 ((max width height - width) / 2)
                    )
                , String.fromFloat
                    (BoundingBox2d.minY boundingBox
                        - max 0 ((max width height - height) / 2)
                    )
                , String.fromFloat (max width height)
                , String.fromFloat (max width height)
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
            BoundingBox2d.scaleAbout centerPoint 1.1 box

        zoom =
            max width height / 40

        ( geometry, _ ) =
            Pattern.geometry pattern
    in
    --Element.column
    --    [ Border.color (Design.toColor Bright)
    --    , Border.width 1
    --    ]
    --    [ Element.row
    --        [ Element.width Element.fill
    --        , Element.padding Design.small
    --        , Design.backgroundColor Darkest
    --        , Design.fontColor Darkest
    --        , Border.color (Design.toColor Darkest)
    --        , Border.widthEach
    --            { left = 0
    --            , right = 0
    --            , top = 0
    --            , bottom = 1
    --            }
    --        ]
    --        [ Element.link [ Element.centerX ]
    --            { url = Route.toString prefix (Route.Editor storedPattern.slug Nothing)
    --            , label =
    --                Element.el
    --                    [ Font.size Design.small
    --                    , Font.underline
    --                    , Element.mouseOver
    --                        [ Font.color (Design.toColor Darkish) ]
    --                    ]
    --                    (Element.text storedPattern.name)
    --            }
    --        , Element.downloadAs [ Element.alignLeft ]
    --            { url =
    --                "data:application/json;charset=utf-8,"
    --                    ++ Encode.encode 0 (StoredPattern.encode storedPattern)
    --            , label =
    --                Element.el
    --                    [ Font.size Design.small
    --                    , Element.mouseOver
    --                        [ Font.color (Design.toColor Darkish) ]
    --                    ]
    --                    (View.Icon.fa "download")
    --            , filename = storedPattern.slug ++ ".json"
    --            }
    --        ]
    --    , Element.el
    --        [ Element.width (Element.px 300)
    --        , Element.height (Element.px 300)
    --        , Background.color (Element.rgb 255 255 255)
    --        ]
    --        (Element.html <|
    --            Svg.svg
    --                [ Svg.Attributes.viewBox viewBox
    --                , Html.Attributes.style "user-select" "none"
    --                , Html.Events.preventDefaultOn "dragstart" <|
    --                    Decode.succeed ( NoOp, True )
    --                ]
    --                [ Pattern.draw selections False zoom Nothing pattern ]
    --        )
    --    , Element.row
    --        [ Element.width Element.fill
    --        , Element.padding Design.xSmall
    --        , Element.spacing Design.xSmall
    --        , Design.backgroundColor Darkest
    --        , Design.fontColor Darkest
    --        , Border.color (Design.toColor Bright)
    --        , Border.widthEach
    --            { left = 0
    --            , right = 0
    --            , top = 1
    --            , bottom = 0
    --            }
    --        ]
    --        [ Element.el [ Element.alignRight ] <|
    --            button "Delete" (DeletePatternPressed storedPattern.slug)
    --        ]
    --    ]
    Element.column
        [ Design.fontColor Dark
        , Border.width 1
        , Design.borderColor Dark
        , Element.htmlAttribute <|
            Html.Attributes.class "pattern-card"
        , Element.pointer
        , Border.shadow
            { offset = ( 2, 2 )
            , size = 0
            , blur = 0
            , color = Element.fromRgb (Color.toRgba (Color.hsla 240 0.07 0.5 0.25))
            }
        , Background.color <|
            Element.fromRgb (Color.toRgba (Color.hsla 240 0.07 0.9 1))
        , Element.mouseOver
            [ Background.color <|
                Element.fromRgb (Color.toRgba (Color.hsla 240 0.07 0.99 1))
            ]
        , Events.onClick (PatternCardClicked storedPattern.slug)
        ]
        [ Element.el
            [ Element.width (Element.px 300)
            , Element.height (Element.px 300)
            ]
            (Element.html <|
                Svg.svg
                    [ Svg.Attributes.viewBox viewBox
                    , Html.Attributes.style "user-select" "none"
                    , Html.Events.preventDefaultOn "dragstart" <|
                        Decode.succeed ( NoOp, True )
                    ]
                    [ Pattern.draw selections False zoom Nothing pattern ]
            )
        , Element.row
            [ Element.width Element.fill
            , Element.padding Design.xSmall
            , Design.backgroundColor Dark
            ]
            [ Element.el
                [ Region.heading 2
                , Element.alignLeft
                ]
                (Element.link []
                    { url = Route.toString prefix (Route.Editor storedPattern.slug Nothing)
                    , label =
                        Element.el
                            [ Font.size 20
                            , Element.padding Design.small
                            ]
                            (Element.text storedPattern.name)
                    }
                )
            , Input.button
                [ Element.alignRight
                , Element.padding Design.small
                , Font.color white
                , Border.width 1
                , Border.color gray900
                , Border.rounded 4
                , Element.mouseOver
                    [ Background.color gray700 ]
                , Element.htmlAttribute <|
                    Html.Events.stopPropagationOn "click" <|
                        Decode.succeed ( PatternCardMenuClicked storedPattern.slug, True )
                ]
                { onPress = Nothing
                , label = View.Icon.faLarge "ellipsis-h"
                }
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


color =
    Element.fromRgb << Color.toRgba


white =
    color (Color.rgb255 229 223 197)


green900 =
    color (Color.rgb255 27 94 32)


gray700 =
    color (Color.rgb255 97 97 97)


gray750 =
    color (Color.rgb255 82 82 82)


gray800 =
    color (Color.rgb255 66 66 66)


gray900 =
    color (Color.rgb255 33 33 33)


gray950 =
    color (Color.rgb255 22 22 22)
