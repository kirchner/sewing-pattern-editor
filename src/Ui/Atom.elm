module Ui.Atom exposing
    ( btnPrimary, btnSecondary, btnDanger, btnCancel
    , btnCallToAction
    , btnIcon, btnIconDanger, btnIconLarge
    , checkbox
    , radioRow, radioColumn, option
    , segmentControl
    , inputText, inputTextAppended, inputFormula, inputFormulaAppended
    , fa, faBody, faLarge
    , withFocusOutline, withFocusOutlineTop, withFocusOutlineBottom
    )

{-|


# Buttons

@docs btnPrimary, btnSecondary, btnDanger, btnCancel
@docs btnCallToAction
@docs btnIcon, btnIconDanger, btnIconLarge


# Form Elements

@docs checkbox
@docs radioRow, radioColumn, option
@docs segmentControl
@docs inputText, inputTextAppended, inputFormula, inputFormulaAppended


# Icons

@docs fa, faBody, faLarge

@docs withFocusOutline, withFocusOutlineTop, withFocusOutlineBottom

-}

import Element exposing (Decoration, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Ui.Color
import Ui.Space
import Ui.Typography



---- STANDARD


btnPrimary : { onPress : Maybe msg, label : String } -> Element msg
btnPrimary { onPress, label } =
    withFocusOutline <|
        Input.button
            [ Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Font.color Ui.Color.white
            , Background.color Ui.Color.primaryLight
            , Element.mouseOver [ Background.color Ui.Color.primary ]
            , Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }


btnSecondary : String -> { onPress : Maybe msg, label : String } -> Element msg
btnSecondary =
    btnSecondaryHelp Element.shrink


btnSecondaryHelp : Element.Length -> String -> { onPress : Maybe msg, label : String } -> Element msg
btnSecondaryHelp width id { onPress, label } =
    withFocusOutline <|
        Input.button
            [ Element.htmlAttribute (Html.Attributes.id id)
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Element.width width
            , Background.color Ui.Color.secondary
            , Element.mouseOver [ Background.color Ui.Color.secondaryDark ]
            , Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }


btnDanger : { onPress : Maybe msg, label : String } -> Element msg
btnDanger { onPress, label } =
    withFocusOutline <|
        Input.button
            [ Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Font.color Ui.Color.white
            , Background.color Ui.Color.danger
            , Element.mouseOver [ Background.color Ui.Color.dangerDark ]
            , Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }


btnCancel : { onPress : Maybe msg, label : String } -> Element msg
btnCancel { onPress, label } =
    withFocusOutline <|
        Input.button
            [ Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Font.color Ui.Color.primary
            , Font.underline
            , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }



---- CALL TO ACTION


btnCallToAction : String -> { onPress : Maybe msg, label : String } -> Element msg
btnCallToAction id { onPress, label } =
    withFocusOutline <|
        Input.button
            [ Element.htmlAttribute (Html.Attributes.id id)
            , Element.width Element.fill
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Border.rounded Ui.Space.level1
            , Background.color Ui.Color.secondary
            , Element.mouseOver [ Background.color Ui.Color.secondaryDark ]
            , Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
            ]
            { onPress = onPress
            , label =
                Element.row
                    [ Element.width Element.fill
                    , Element.spacing Ui.Space.level4
                    ]
                    [ Ui.Typography.button label
                    , Element.el [ Element.alignRight ] (fa "chevron-right")
                    ]
            }



---- ICON BUTTONS


btnIcon : { onPress : Maybe msg, icon : String } -> Element msg
btnIcon { onPress, icon } =
    withFocusOutline <|
        Input.button
            [ Element.mouseOver [ Font.color Ui.Color.primaryDark ] ]
            { onPress = onPress
            , label =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (fa icon)
            }


btnIconDanger : { onPress : Maybe msg, icon : String } -> Element msg
btnIconDanger { onPress, icon } =
    withFocusOutline <|
        Input.button
            [ Font.color Ui.Color.danger
            , Element.mouseOver [ Font.color Ui.Color.dangerDark ]
            ]
            { onPress = onPress
            , label =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (fa icon)
            }


btnIconLarge : { onPress : Maybe msg, icon : String } -> Element msg
btnIconLarge { onPress, icon } =
    withFocusOutline <|
        Input.button
            [ Element.mouseOver [ Font.color Ui.Color.primaryDark ] ]
            { onPress = onPress
            , label =
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (faLarge icon)
            }



---- CHECKBOX


checkbox :
    { onChange : Bool -> msg
    , checked : Bool
    , label : String
    }
    -> Element msg
checkbox { onChange, checked, label } =
    withFocusOutline <|
        Input.checkbox
            [ Element.width Element.fill ]
            { onChange = onChange
            , icon = checkboxIcon
            , checked = checked
            , label =
                Input.labelRight
                    [ Element.centerY
                    , Element.paddingXY Ui.Space.level1 0
                    ]
                    (Ui.Typography.body label)
            }


checkboxIcon : Bool -> Element msg
checkboxIcon checked =
    Element.el
        [ Element.width (Element.px 26)
        , Element.height (Element.px 26)
        , Font.color Ui.Color.white
        , Element.centerY
        , Font.size 20
        , Font.center
        , Border.rounded 3
        , Border.color <|
            if checked then
                Ui.Color.primary

            else
                Ui.Color.black
        , Border.width 1
        , Background.color <|
            if checked then
                Ui.Color.primary

            else
                Ui.Color.white
        , Element.focused
            [ Border.color Ui.Color.primary
            , focusShadow
            ]
        ]
        (if checked then
            Element.el
                [ Border.color Ui.Color.white
                , Element.height (Element.px 9)
                , Element.width (Element.px 14)
                , Element.rotate (degrees -50)
                , Element.centerX
                , Element.centerY
                , Element.moveUp 3
                , Border.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Element.none

         else
            Element.none
        )



---- RADIOS


radioRow :
    String
    ->
        { onChange : value -> msg
        , options : List (Input.Option value msg)
        , selected : Maybe value
        , label : String
        }
    -> Element msg
radioRow id { onChange, options, selected, label } =
    withFocusOutline <|
        Input.radioRow
            [ Element.htmlAttribute (Html.Attributes.id id)
            , Element.width Element.fill
            , Element.spacing Ui.Space.level4
            ]
            { onChange = onChange
            , options = options
            , selected = selected
            , label =
                Input.labelAbove
                    [ Element.paddingEach
                        { top = 0
                        , bottom = Ui.Space.level2
                        , left = 0
                        , right = 0
                        }
                    ]
                    (Ui.Typography.bodyBold label)
            }


radioColumn :
    String
    ->
        { onChange : value -> msg
        , options : List (Input.Option value msg)
        , selected : Maybe value
        , label : String
        }
    -> Element msg
radioColumn id { onChange, options, selected, label } =
    withFocusOutline <|
        Input.radio
            [ Element.htmlAttribute (Html.Attributes.id id)
            , Element.width Element.fill
            , Element.spacing Ui.Space.level2
            ]
            { onChange = onChange
            , options = options
            , selected = selected
            , label =
                Input.labelAbove
                    [ Element.paddingEach
                        { top = 0
                        , bottom = Ui.Space.level2
                        , left = 0
                        , right = 0
                        }
                    ]
                    (Ui.Typography.bodyBold label)
            }


option : value -> String -> Input.Option value msg
option value label =
    Input.optionWith value (radioOption label)


optionCustom : value -> Element msg -> Input.Option value msg
optionCustom value label =
    Input.optionWith value (radioOptionCustom label)


radioOption : String -> Input.OptionState -> Element msg
radioOption label status =
    radioOptionCustom (Element.text label) status


radioOptionCustom : Element msg -> Input.OptionState -> Element msg
radioOptionCustom label status =
    let
        setClass attrs =
            case status of
                Input.Selected ->
                    Element.htmlAttribute (Html.Attributes.class "focusable")
                        :: attrs

                _ ->
                    attrs
    in
    Element.row
        [ Element.spacing Ui.Space.level2
        , Element.alignLeft
        , Element.width Element.fill
        ]
        [ Element.el
            ([ Element.width (Element.px 26)
             , Element.height (Element.px 26)
             , Element.alignTop
             , Background.color Ui.Color.white
             , Border.rounded 13
             , Border.width <|
                case status of
                    Input.Idle ->
                        1

                    Input.Focused ->
                        1

                    Input.Selected ->
                        8
             , Border.color <|
                case status of
                    Input.Idle ->
                        Ui.Color.black

                    Input.Focused ->
                        Ui.Color.black

                    Input.Selected ->
                        Ui.Color.primary
             ]
                |> setClass
            )
            Element.none
        , Element.el
            [ Element.width Element.fill
            , Element.htmlAttribute <|
                Html.Attributes.class "unfocusable"
            ]
            label
        ]



---- SEGMENT CONTROL


segmentControl :
    String
    ->
        { label : Maybe String
        , onChange : tag -> msg
        , options : List ( tag, String )
        , selected : tag
        , elementAppended : Bool
        }
    -> Element msg
segmentControl id { label, onChange, options, selected, elementAppended } =
    (if elementAppended then
        withFocusOutlineTop

     else
        withFocusOutline
    )
    <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing Ui.Space.level2
            ]
            [ case label of
                Nothing ->
                    Element.none

                Just labelText ->
                    Element.el
                        [ Element.htmlAttribute (Html.Attributes.id (id ++ "-label"))
                        , Element.width Element.fill
                        ]
                        (Ui.Typography.bodyBold labelText)
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , if elementAppended then
                    Border.roundEach
                        { topLeft = 3
                        , topRight = 3
                        , bottomLeft = 0
                        , bottomRight = 0
                        }

                  else
                    Border.rounded 3
                , Border.width 1
                , Border.color Ui.Color.primary
                , Background.color Ui.Color.secondary
                ]
                (Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.htmlAttribute (Html.Attributes.attribute "role" "radiogroup")
                    , Element.htmlAttribute (Html.Attributes.tabindex 0)
                    , Element.htmlAttribute (Html.Attributes.class "segment-control")
                    , Element.htmlAttribute (Html.Attributes.id (id ++ "-label"))
                    , onKeyDown onChange (List.map Tuple.first options) selected
                    ]
                    (List.map (Element.map onChange) (segments options selected))
                )
            ]


type Position
    = First
    | Middle
    | Last


segments : List ( tag, String ) -> tag -> List (Element tag)
segments tags selectedTag =
    tags
        |> List.indexedMap
            (\index ( tag, label ) ->
                if index == 0 then
                    segment selectedTag tag First label

                else if index == List.length tags - 1 then
                    segment selectedTag tag Last label

                else
                    segment selectedTag tag Middle label
            )
        |> List.intersperse
            (Element.el
                [ Element.width (Element.px 1)
                , Element.height Element.fill
                , Background.color Ui.Color.primary
                ]
                Element.none
            )


segment : tag -> tag -> Position -> String -> Element tag
segment selectedTag thisTag position label =
    let
        selected =
            selectedTag == thisTag
    in
    Element.el
        [ Element.htmlAttribute (Html.Attributes.attribute "role" "radio")
        , Element.htmlAttribute <|
            Html.Attributes.attribute "aria-checked" <|
                if selected then
                    "true"

                else
                    "false"
        , Events.onClick thisTag
        , Element.pointer
        , Element.width Element.fill
        , Element.paddingXY 0 7
        , Background.color <|
            if selected then
                Ui.Color.primary

            else
                Ui.Color.transparent
        , Element.htmlAttribute <|
            Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s"
        , Font.color <|
            if selected then
                Ui.Color.white

            else
                Ui.Color.black
        , Element.mouseOver <|
            if selected then
                []

            else
                [ Background.color Ui.Color.secondaryDark ]
        , case position of
            First ->
                Border.widthEach
                    { top = 1
                    , bottom = 1
                    , left = 1
                    , right = 0
                    }

            Middle ->
                Border.widthEach
                    { top = 1
                    , bottom = 1
                    , left = 0
                    , right = 0
                    }

            Last ->
                Border.widthEach
                    { top = 1
                    , bottom = 1
                    , left = 0
                    , right = 1
                    }
        , Border.color Ui.Color.transparent
        , Element.focused
            [ Border.color Ui.Color.primary ]
        ]
        (Element.el
            ([ Element.centerX ] ++ userSelectNone)
            (Ui.Typography.body label)
        )


onKeyDown : (tag -> msg) -> List tag -> tag -> Element.Attribute msg
onKeyDown toMsg tags selectedTag =
    let
        ( prevTag, nextTag ) =
            case List.splitWhen (\thisTag -> thisTag == selectedTag) tags of
                Nothing ->
                    unchanged

                Just ( start, end ) ->
                    case ( List.last start, end ) of
                        ( Nothing, _ :: next :: _ ) ->
                            case List.last tags of
                                Nothing ->
                                    unchanged

                                Just last ->
                                    ( last, next )

                        ( Just prev, _ :: next :: _ ) ->
                            ( prev, next )

                        ( Just prev, _ :: [] ) ->
                            case List.head tags of
                                Nothing ->
                                    unchanged

                                Just first ->
                                    ( prev, first )

                        _ ->
                            unchanged

        unchanged =
            ( selectedTag, selectedTag )
    in
    Element.htmlAttribute <|
        Html.Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                Decode.succeed (toMsg prevTag)

                            "ArrowUp" ->
                                Decode.succeed (toMsg prevTag)

                            "ArrowRight" ->
                                Decode.succeed (toMsg nextTag)

                            "ArrowDown" ->
                                Decode.succeed (toMsg nextTag)

                            _ ->
                                Decode.fail "not handling that key here"
                    )
            )


userSelectNone : List (Element.Attribute msg)
userSelectNone =
    [ Element.htmlAttribute <|
        Html.Attributes.style "-moz-user-select" "none"
    , Element.htmlAttribute <|
        Html.Attributes.style "-webkit-user-select" "none"
    , Element.htmlAttribute <|
        Html.Attributes.style "-ms-user-select" "none"
    , Element.htmlAttribute <|
        Html.Attributes.style "user-select" "none"
    ]



---- TEXT


inputText :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        , help : Maybe String
        }
    -> Element msg
inputText id data =
    let
        withShadow attrs =
            if data.help == Nothing then
                [ Element.focused
                    [ Border.color Ui.Color.primary
                    , focusShadow
                    ]
                , Border.color Ui.Color.black
                ]
                    ++ attrs

            else
                [ dangerShadow
                , Border.color Ui.Color.danger
                ]
                    ++ attrs
    in
    withFocusOutline <|
        Input.text
            (withShadow
                [ Element.htmlAttribute <|
                    Html.Attributes.id id
                , Element.width Element.fill
                , Element.padding 10
                , Font.size 16
                , Background.color Ui.Color.white
                , Border.rounded 3
                , Border.width 1
                ]
            )
            { onChange = data.onChange
            , text = data.text
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (case data.help of
                        Nothing ->
                            Element.el
                                [ Element.paddingEach
                                    { top = 0
                                    , bottom = Ui.Space.level1
                                    , left = 0
                                    , right = 0
                                    }
                                ]
                                (Ui.Typography.bodyBold data.label)

                        Just helpText ->
                            Element.column
                                [ Element.spacing Ui.Space.level2
                                , Element.paddingEach
                                    { top = 0
                                    , bottom = Ui.Space.level1
                                    , left = 0
                                    , right = 0
                                    }
                                ]
                                [ Ui.Typography.bodyBold data.label
                                , Element.row
                                    [ Element.spacing Ui.Space.level1
                                    , Font.color Ui.Color.danger
                                    ]
                                    [ fa "exclamation-circle"
                                    , Ui.Typography.bodyBold helpText
                                    ]
                                ]
                    )
            }


inputTextAppended :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        }
    -> Element msg
inputTextAppended id data =
    withFocusOutlineBottom <|
        Input.text
            [ Element.htmlAttribute <|
                Html.Attributes.id id
            , Element.width Element.fill
            , Element.padding 10
            , Font.size 16
            , Background.color Ui.Color.white
            , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = 3
                , bottomRight = 3
                }
            , Border.widthEach
                { top = 0
                , bottom = 1
                , left = 1
                , right = 1
                }
            , Element.focused
                [ Border.color Ui.Color.primary
                , focusShadow
                ]
            , Border.color Ui.Color.black
            ]
            { onChange = data.onChange
            , text = data.text
            , placeholder = Nothing
            , label = Input.labelHidden data.label
            }


inputFormula :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        , help : Maybe String
        }
    -> Element msg
inputFormula id data =
    let
        lineCount =
            List.length (String.split "\n" data.text)

        padding =
            if lineCount == 1 then
                Element.padding 10

            else
                Element.paddingEach
                    { left =
                        if lineCount < 10 then
                            30

                        else
                            40
                    , right = 10
                    , top = 10
                    , bottom = 10
                    }

        sansSerif =
            Font.family
                [ Font.external
                    { name = "Rubik"
                    , url = "https://fonts.googleapis.com/css?family=Rubik:300"
                    }
                , Font.sansSerif
                ]

        withShadow attrs =
            if data.help == Nothing then
                [ Element.focused
                    [ Border.color Ui.Color.primary
                    , focusShadow
                    ]
                , Border.color Ui.Color.black
                ]
                    ++ attrs

            else
                [ dangerShadow
                , Border.color Ui.Color.danger
                ]
                    ++ attrs
    in
    withFocusOutline <|
        Input.multiline
            (withShadow
                [ Element.htmlAttribute (Html.Attributes.id id)
                , Element.width Element.fill
                , Element.inFront (lineNumbers lineCount)
                , padding
                , Element.spacing Ui.Space.level1
                , Font.size 16
                , Font.family [ Font.monospace ]
                , Background.color Ui.Color.white
                , Border.width 1
                , Border.rounded 3
                , Element.htmlAttribute (Html.Attributes.rows lineCount)
                , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                , Element.clip
                ]
            )
            { onChange = data.onChange
            , text = data.text
            , placeholder = Nothing
            , spellcheck = False
            , label =
                Input.labelAbove []
                    (case data.help of
                        Nothing ->
                            Element.el
                                [ sansSerif
                                , Element.paddingEach
                                    { top = 0
                                    , bottom = Ui.Space.level1
                                    , left = 0
                                    , right = 0
                                    }
                                ]
                                (Ui.Typography.bodyBold data.label)

                        Just helpText ->
                            Element.column
                                [ Element.spacing Ui.Space.level1
                                , Element.paddingEach
                                    { top = 0
                                    , bottom = Ui.Space.level1
                                    , left = 0
                                    , right = 0
                                    }
                                ]
                                [ Element.el [ sansSerif ]
                                    (Ui.Typography.bodyBold data.label)
                                , Element.row
                                    [ Element.spacing Ui.Space.level1
                                    , Element.paddingEach
                                        { left = 0
                                        , right = 0
                                        , top = Ui.Space.level1
                                        , bottom = 0
                                        }
                                    , sansSerif
                                    , Font.color Ui.Color.danger
                                    ]
                                    [ fa "exclamation-circle"
                                    , Ui.Typography.bodyBold helpText
                                    ]
                                ]
                    )
            }


inputFormulaAppended :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        }
    -> Element msg
inputFormulaAppended id data =
    let
        lineCount =
            List.length (String.split "\n" data.text)

        padding =
            if lineCount == 1 then
                Element.padding 10

            else
                Element.paddingEach
                    { left =
                        if lineCount < 10 then
                            30

                        else
                            40
                    , right = 10
                    , top = 10
                    , bottom = 10
                    }

        sansSerif =
            Font.family
                [ Font.external
                    { name = "Rubik"
                    , url = "https://fonts.googleapis.com/css?family=Rubik:300"
                    }
                , Font.sansSerif
                ]
    in
    withFocusOutlineBottom <|
        Input.multiline
            [ Element.htmlAttribute (Html.Attributes.id id)
            , Element.width Element.fill
            , Element.inFront (lineNumbers lineCount)
            , padding
            , Element.spacing Ui.Space.level1
            , Font.size 16
            , Font.family [ Font.monospace ]
            , Background.color Ui.Color.white
            , Border.widthEach
                { top = 0
                , bottom = 1
                , left = 1
                , right = 1
                }
            , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = 3
                , bottomRight = 3
                }
            , Element.focused
                [ Border.color Ui.Color.primary
                , focusShadow
                ]
            , Border.color Ui.Color.black
            , Element.htmlAttribute (Html.Attributes.rows lineCount)
            , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
            , Element.clip
            ]
            { onChange = data.onChange
            , text = data.text
            , placeholder = Nothing
            , spellcheck = False
            , label = Input.labelHidden data.label
            }


lineNumbers : Int -> Element msg
lineNumbers lineCount =
    if lineCount == 1 then
        Element.none

    else
        Element.row
            [ Element.height Element.fill
            , Element.paddingXY 5 0
            , Element.spacing 5
            ]
            [ Element.column
                [ Font.size 16
                , Font.family
                    [ Font.monospace ]
                , Element.spacing Ui.Space.level1
                ]
                (List.range 1 lineCount
                    |> List.map
                        (\lineNumber ->
                            Element.el
                                [ Element.alignRight ]
                                (Element.text (String.fromInt lineNumber))
                        )
                )
            , Element.el
                [ Element.paddingXY 0 5
                , Element.height Element.fill
                ]
                (Element.el
                    [ Element.height Element.fill
                    , Element.width (Element.px 1)
                    , Background.color Ui.Color.black
                    ]
                    Element.none
                )
            ]



---- ICONS


fa : String -> Element msg
fa name =
    faHelp 14 name


faBody : String -> Element msg
faBody name =
    faHelp 16 name


faLarge : String -> Element msg
faLarge name =
    faHelp 24 name


faHelp : Int -> String -> Element msg
faHelp size name =
    let
        sizePx =
            String.fromInt size ++ "px"
    in
    Element.el
        [ Element.centerX
        , Element.centerY
        ]
        (Element.el [] <|
            Element.html <|
                Html.i
                    [ Html.Attributes.class "fas"
                    , Html.Attributes.class ("fa-" ++ name)
                    , Html.Attributes.style "font-size" sizePx
                    , Html.Attributes.style "width" sizePx
                    , Html.Attributes.style "height" sizePx
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "color" "inherit"
                    ]
                    []
        )



---- WITH FOCUS OUTLINE


withFocusOutline : Element msg -> Element msg
withFocusOutline element =
    Element.el
        [ Border.width 3
        , Border.dotted
        , Border.color Ui.Color.transparent
        , Element.focused
            [ Border.color Ui.Color.primary ]
        , Element.width Element.fill
        ]
        (Element.el
            [ Element.padding 4
            , Element.width Element.fill
            ]
            element
        )


withFocusOutlineTop : Element msg -> Element msg
withFocusOutlineTop element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 0
            , left = 3
            , right = 3
            }
        , Border.dotted
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.primary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 0
                , left = 4
                , right = 4
                }
            , Element.width Element.fill
            ]
            element
        )


withFocusOutlineBottom : Element msg -> Element msg
withFocusOutlineBottom element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 0
            , bottom = 3
            , left = 3
            , right = 3
            }
        , Border.dotted
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.primary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 0
                , bottom = 4
                , left = 4
                , right = 4
                }
            , Element.width Element.fill
            ]
            element
        )


focusShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Color.primary
        }


dangerShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Color.danger
        }
