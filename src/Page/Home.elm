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
import Design
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
import View.Input
import View.Modal


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
        , Element.inFront <|
            viewDialog model.dialog
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.paddingXY (2 * Design.xLarge) Design.large
            , Element.spacing Design.normal
            ]
            [ Element.row
                [ Element.width Element.fill
                ]
                [ Element.el
                    [ Design.fontXXLarge
                    , Font.color Design.primary
                    , Font.bold
                    , Element.alignLeft
                    ]
                    (Element.text "Patterns")
                , Element.row
                    [ Element.spacing Design.xSmall
                    , Element.alignRight
                    ]
                    [ View.Input.btnSecondary "create-new-pattern"
                        { onPress = Just AddPatternClicked
                        , label = "Create new pattern"
                        }
                    , View.Input.btnSecondary "upload-pattern"
                        { onPress = Nothing
                        , label = "Upload pattern"
                        }
                    ]
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing Design.normal
                ]
                (storedPatterns
                    |> slice 3
                    |> List.map
                        (List.map (viewPattern prefix)
                            >> List.intersperse
                                (Element.el [ Element.width Element.fill ] Element.none)
                            >> Element.row [ Element.width Element.fill ]
                        )
                )
            , Element.row
                [ Element.width Element.fill
                , Element.paddingXY 0 Design.large
                , Element.spacing 5
                ]
                [ Element.newTabLink
                    [ Element.alignRight
                    , Font.color Design.black
                    , Font.size Design.small
                    , Element.mouseOver
                        [ Font.color Design.primaryDark ]
                    ]
                    { url = "https://github.com/kirchner/sewing-pattern-editor"
                    , label =
                        Element.row
                            [ Element.spacing Design.xSmall ]
                            [ Element.el [ Font.underline ]
                                (Element.text "Check out the source code")
                            , View.Icon.dev "github-plain"
                            ]
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
            View.Modal.small
                { onCancelPress = NewPatternCancelClicked
                , title = "Create new pattern"
                , content =
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Design.small
                        ]
                        [ Element.text "Create a new pattern"
                        , View.Input.text "name-input"
                            { onChange = NewPatternNameChanged
                            , text = name
                            , label = "Pick a name"
                            , help = Nothing
                            }
                        ]
                , actions =
                    [ View.Input.btnPrimary
                        { onPress = Just NewPatternCreateClicked
                        , label = "Create"
                        }
                    , Element.el [ Element.alignRight ] <|
                        View.Input.btnCancel
                            { onPress = Just NewPatternCancelClicked
                            , label = "Cancel"
                            }
                    ]
                }


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
                        , maxX = 330
                        , minY = 0
                        , maxY = 280
                        }
                    )

        scale box =
            let
                centerPoint =
                    BoundingBox2d.centerPoint box
            in
            BoundingBox2d.scaleAbout centerPoint 1.1 box

        zoom =
            350 / max width height

        ( geometry, _ ) =
            Pattern.geometry pattern
    in
    Element.column
        [ Border.width 1
        , Border.rounded 4
        , Border.color Design.primary
        , Element.padding Design.small
        , Element.spacing Design.normal
        , Element.mouseOver
            [ Border.color Design.primaryDark
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 2
                , blur = 8
                , color = Design.primary
                }
            ]
        ]
        [ Element.column
            [ Element.spacing Design.normal
            , Element.pointer
            , Events.onClick (PatternCardClicked storedPattern.slug)
            ]
            [ Element.el
                [ Border.rounded 4
                , Element.width (Element.px 340)
                , Element.height (Element.px 280)
                , Background.color Design.secondary
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
            , Element.el
                [ Region.heading 2
                , Element.alignLeft
                ]
                (Element.link []
                    { url = Route.toString prefix (Route.Editor storedPattern.slug Nothing)
                    , label =
                        Element.el
                            [ Design.fontXLarge
                            , Font.bold
                            , Font.color Design.primary
                            , Font.underline
                            , Element.mouseOver
                                [ Font.color Design.primaryDark ]
                            ]
                            (Element.text storedPattern.name)
                    }
                )
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing Design.xSmall
            ]
            [ View.Input.btnSecondary (storedPattern.slug ++ "-rename")
                { onPress = Nothing
                , label = "Rename"
                }
            , Element.downloadAs
                [ Element.alignLeft
                , Element.paddingXY 18 10
                , Background.color Design.secondary
                , Font.color Design.black
                , Design.fontSmall
                , Element.mouseOver
                    [ Background.color Design.secondaryDark ]
                , Element.htmlAttribute <|
                    Html.Attributes.style "transition"
                        "background-color 0.2s ease-in-out 0s"
                ]
                { url =
                    "data:application/json;charset=utf-8,"
                        ++ Encode.encode 0 (StoredPattern.encode storedPattern)
                , label = Element.text "Download"
                , filename = storedPattern.slug ++ ".json"
                }
            , Element.el [ Element.alignRight ] <|
                View.Input.btnDanger
                    { onPress = Nothing
                    , label = "Delete"
                    }
            ]
        ]



---- HELPER


slice : Int -> List a -> List (List a)
slice size list =
    sliceHelp size [] list


sliceHelp : Int -> List (List a) -> List a -> List (List a)
sliceHelp size result list =
    case List.take size list of
        [] ->
            List.reverse result

        next ->
            sliceHelp size (next :: result) (List.drop size list)
