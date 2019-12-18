module Ui.Atom.Input exposing
    ( BtnConfig, btnPrimary, btnSecondary, btnDanger, btnCancel
    , btnSecondaryBorderedLeft, btnSecondaryBorderedRight
    , btnCallToAction
    , IconBtnConfig, btnIcon, btnIconDanger, btnIconLarge
    , CheckboxConfig, checkbox
    , TextConfig, text, textAppended, formula, formulaAppended
    , RadioConfig, radioRow, radioColumn, OptionConfig, option
    , SegmentControlConfig, segmentControl, Child(..), nested, nestedHideable
    )

{-|


# Buttons

@docs BtnConfig, btnPrimary, btnSecondary, btnDanger, btnCancel
@docs btnSecondaryBorderedLeft, btnSecondaryBorderedRight
@docs btnCallToAction
@docs IconBtnConfig, btnIcon, btnIconDanger, btnIconLarge


# Checkboxes

@docs CheckboxConfig, checkbox


# Text

@docs TextConfig, text, textAppended, formula, formulaAppended


# Radio Selection

@docs RadioConfig, radioRow, radioColumn, OptionConfig, option

@docs SegmentControlConfig, segmentControl, Child, nested, nestedHideable

-}

import Element exposing (Attr, Attribute, Decoration, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Label)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Ui.Atom
import Ui.Atom.Icon
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- BUTTONS


{-| -}
type alias BtnConfig msg =
    { id : String
    , onPress : Maybe msg
    , label : String
    }


{-| -}
btnPrimary : BtnConfig msg -> Element msg
btnPrimary { id, onPress, label } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Font.color Ui.Theme.Color.white
            , Background.color Ui.Theme.Color.primaryLight
            , Element.mouseOver [ Background.color Ui.Theme.Color.primary ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Theme.Typography.button label
            }


{-| -}
btnSecondary : BtnConfig msg -> Element msg
btnSecondary { id, onPress, label } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Background.color Ui.Theme.Color.secondary
            , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Theme.Typography.button label
            }


{-| -}
btnSecondaryBorderedLeft : BtnConfig msg -> Element msg
btnSecondaryBorderedLeft { id, onPress, label } =
    Ui.Theme.Focus.outlineLeft <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Background.color Ui.Theme.Color.secondary
            , Border.width 1
            , Border.color Ui.Theme.Color.secondaryDark
            , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Theme.Typography.button label
            }


{-| -}
btnSecondaryBorderedRight : BtnConfig msg -> Element msg
btnSecondaryBorderedRight { id, onPress, label } =
    Ui.Theme.Focus.outlineRight <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Background.color Ui.Theme.Color.secondary
            , Border.widthEach
                { left = 0
                , right = 1
                , top = 1
                , bottom = 1
                }
            , Border.color Ui.Theme.Color.secondaryDark
            , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Theme.Typography.button label
            }


{-| -}
btnDanger : BtnConfig msg -> Element msg
btnDanger { id, onPress, label } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Font.color Ui.Theme.Color.white
            , Background.color Ui.Theme.Color.danger
            , Element.mouseOver [ Background.color Ui.Theme.Color.dangerDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Theme.Typography.button label
            }


{-| -}
btnCancel : BtnConfig msg -> Element msg
btnCancel { id, onPress, label } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Font.color Ui.Theme.Color.primary
            , Font.underline
            , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = Ui.Theme.Typography.button label
            }



---- CALL TO ACTION


{-| -}
btnCallToAction : BtnConfig msg -> Element msg
btnCallToAction { id, onPress, label } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.width Element.fill
            , Element.paddingXY Ui.Theme.Spacing.level3 Ui.Theme.Spacing.level2
            , Border.rounded Ui.Theme.Spacing.level1
            , Background.color Ui.Theme.Color.secondary
            , Element.mouseOver [ Background.color Ui.Theme.Color.secondaryDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label =
                Element.row
                    [ Element.width Element.fill
                    , Element.spacing Ui.Theme.Spacing.level4
                    ]
                    [ Ui.Theme.Typography.button label
                    , Element.el [ Element.alignRight ] (Ui.Atom.Icon.fa "chevron-right")
                    ]
            }



---- ICON BUTTONS


{-| -}
type alias IconBtnConfig msg =
    { id : String
    , onPress : Maybe msg
    , icon : String
    }


{-| -}
btnIcon : IconBtnConfig msg -> Element msg
btnIcon { id, onPress, icon } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = btnIconLabel (Ui.Atom.Icon.fa icon)
            }


{-| -}
btnIconDanger : IconBtnConfig msg -> Element msg
btnIconDanger { id, onPress, icon } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Font.color Ui.Theme.Color.danger
            , Element.mouseOver [ Font.color Ui.Theme.Color.dangerDark ]
            ]
            { onPress = onPress
            , label = btnIconLabel (Ui.Atom.Icon.fa icon)
            }


{-| -}
btnIconLarge : IconBtnConfig msg -> Element msg
btnIconLarge { id, onPress, icon } =
    Ui.Theme.Focus.outline <|
        Input.button
            [ attributeId id
            , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = btnIconLabel (Ui.Atom.Icon.faLarge icon)
            }


{-| -}
btnIconLabel : Element msg -> Element msg
btnIconLabel icon =
    Element.el
        [ Element.centerX
        , Element.centerY
        ]
        icon



---- CHECKBOXES


{-| -}
type alias CheckboxConfig msg =
    { id : String
    , onChange : Bool -> msg
    , checked : Bool
    , label : String
    }


{-| -}
checkbox : CheckboxConfig msg -> Element msg
checkbox { id, onChange, checked, label } =
    Ui.Theme.Focus.outline <|
        Input.checkbox
            [ attributeId id
            , Element.width Element.fill
            ]
            { onChange = onChange
            , icon = checkboxIcon
            , checked = checked
            , label =
                Input.labelRight
                    [ Element.centerY
                    , Element.paddingXY Ui.Theme.Spacing.level1 0
                    ]
                    (Ui.Theme.Typography.body label)
            }


checkboxIcon : Bool -> Element msg
checkboxIcon checked =
    Element.el
        [ Element.width (Element.px 26)
        , Element.height (Element.px 26)
        , Font.color Ui.Theme.Color.white
        , Element.centerY
        , Font.size 20
        , Font.center
        , Border.rounded 3
        , Border.color <|
            if checked then
                Ui.Theme.Color.primary

            else
                Ui.Theme.Color.black
        , Border.width 1
        , Background.color <|
            if checked then
                Ui.Theme.Color.primary

            else
                Ui.Theme.Color.white
        , Element.focused
            [ Border.color Ui.Theme.Color.primary
            , focusShadow
            ]
        ]
        (if checked then
            Element.el
                [ Border.color Ui.Theme.Color.white
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



---- TEXT


{-| -}
type alias TextConfig msg =
    { id : String
    , onChange : String -> msg
    , text : String
    , label : String
    , help : Maybe String
    }


{-| -}
text : TextConfig msg -> Element msg
text data =
    let
        withShadow attrs =
            if data.help == Nothing then
                [ Element.focused
                    [ Border.color Ui.Theme.Color.primary
                    , focusShadow
                    ]
                , Border.color Ui.Theme.Color.black
                ]
                    ++ attrs

            else
                [ dangerShadow
                , Border.color Ui.Theme.Color.danger
                ]
                    ++ attrs
    in
    Ui.Theme.Focus.outline <|
        Input.text
            (withShadow
                [ attributeId data.id
                , Element.width Element.fill
                , Element.padding 10
                , Font.size 16
                , Background.color Ui.Theme.Color.white
                , Border.rounded 3
                , Border.width 1
                ]
            )
            { onChange = data.onChange
            , text = data.text
            , placeholder = Nothing
            , label =
                labelAbove
                    { label = data.label
                    , help = data.help
                    }
            }


{-| -}
textAppended : String -> { onChange : String -> msg, text : String, label : String } -> Child msg
textAppended id data =
    Appended <|
        Input.text
            [ attributeId id
            , Element.width Element.fill
            , Element.padding 10
            , Font.size 16
            , Background.color Ui.Theme.Color.white
            , borderWidthAppended
            , borderRoundAppended
            , Element.focused
                [ Border.color Ui.Theme.Color.primary
                , focusShadow
                ]
            , Border.color Ui.Theme.Color.black
            ]
            { onChange = data.onChange
            , text = data.text
            , placeholder = Nothing
            , label = Input.labelHidden data.label
            }


{-| -}
formula : TextConfig msg -> Element msg
formula data =
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

        withShadow attrs =
            if data.help == Nothing then
                [ Element.focused
                    [ Border.color Ui.Theme.Color.primary
                    , focusShadow
                    ]
                , Border.color Ui.Theme.Color.black
                ]
                    ++ attrs

            else
                [ dangerShadow
                , Border.color Ui.Theme.Color.danger
                ]
                    ++ attrs
    in
    Ui.Theme.Focus.outline <|
        Input.multiline
            (withShadow
                [ attributeId data.id
                , Element.width Element.fill
                , Element.inFront (lineNumbers lineCount)
                , padding
                , Element.spacing Ui.Theme.Spacing.level1
                , Font.size 16
                , Font.family [ Font.monospace ]
                , Background.color Ui.Theme.Color.white
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
                labelAbove
                    { label = data.label
                    , help = data.help
                    }
            }


{-| -}
formulaAppended : String -> { onChange : String -> msg, text : String, label : String } -> Child msg
formulaAppended id data =
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
    in
    Appended <|
        Input.multiline
            [ attributeId id
            , Element.width Element.fill
            , Element.inFront (lineNumbers lineCount)
            , padding
            , Element.spacing Ui.Theme.Spacing.level1
            , Font.size 16
            , Font.family [ Font.monospace ]
            , Background.color Ui.Theme.Color.white
            , borderWidthAppended
            , borderRoundAppended
            , Element.focused
                [ Border.color Ui.Theme.Color.primary
                , focusShadow
                ]
            , Border.color Ui.Theme.Color.black
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
                , Element.spacing Ui.Theme.Spacing.level1
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
                    , Background.color Ui.Theme.Color.black
                    ]
                    Element.none
                )
            ]



---- RADIO SELECTION


{-| -}
type alias RadioConfig value msg =
    { id : String
    , onChange : value -> msg
    , options : List (Input.Option value msg)
    , selected : Maybe value
    , label : String
    }


{-| -}
radioRow : RadioConfig value msg -> Element msg
radioRow { id, onChange, options, selected, label } =
    Ui.Theme.Focus.outline <|
        Input.radioRow
            [ attributeId id
            , Element.width Element.fill
            , Element.spacing Ui.Theme.Spacing.level4
            ]
            { onChange = onChange
            , options = options
            , selected = selected
            , label =
                Input.labelAbove
                    [ Element.paddingEach
                        { top = 0
                        , bottom = Ui.Theme.Spacing.level2
                        , left = 0
                        , right = 0
                        }
                    ]
                    (Ui.Theme.Typography.bodyBold label)
            }


{-| -}
radioColumn : RadioConfig value msg -> Element msg
radioColumn { id, onChange, options, selected, label } =
    Ui.Theme.Focus.outline <|
        Input.radio
            [ attributeId id
            , Element.width Element.fill
            , Element.spacing Ui.Theme.Spacing.level2
            ]
            { onChange = onChange
            , options = options
            , selected = selected
            , label =
                Input.labelAbove
                    [ Element.paddingEach
                        { top = 0
                        , bottom = Ui.Theme.Spacing.level2
                        , left = 0
                        , right = 0
                        }
                    ]
                    (Ui.Theme.Typography.bodyBold label)
            }


{-| -}
type alias OptionConfig value msg =
    { value : value
    , label : String
    , child : Element msg
    }


{-| -}
option : OptionConfig value msg -> Input.Option value msg
option cfg =
    Input.optionWith cfg.value (radioOption cfg.label cfg.child)


radioOption : String -> Element msg -> Input.OptionState -> Element msg
radioOption label child status =
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
        [ Element.spacing Ui.Theme.Spacing.level2
        , Element.alignLeft
        , Element.width Element.fill
        ]
        [ Element.el
            ([ Element.width (Element.px 26)
             , Element.height (Element.px 26)
             , Element.alignTop
             , Background.color Ui.Theme.Color.white
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
                        Ui.Theme.Color.black

                    Input.Focused ->
                        Ui.Theme.Color.black

                    Input.Selected ->
                        Ui.Theme.Color.primary
             ]
                |> setClass
            )
            Element.none
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Ui.Theme.Spacing.level2
            , Element.paddingXY 0 6
            ]
            [ Element.el
                [ Element.width Element.fill
                , Element.htmlAttribute (Html.Attributes.class "unfocusable")
                ]
                (Ui.Theme.Typography.body label)
            , child
            ]
        ]



---- SEGMENT CONTROL


{-| -}
type alias SegmentControlConfig tag msg =
    { id : String
    , label : Maybe String
    , help : Maybe String
    , onChange : tag -> msg
    , options : List ( tag, String )
    , selected : tag
    , child : Maybe (Child msg)
    }


{-| -}
type Child msg
    = Appended (Element msg)
    | Nested (Element msg)
    | NestedHideable
        { show : Bool
        , onPress : msg
        , shown : Element msg
        , hidden : Element msg
        }


{-| -}
nested : Element msg -> Child msg
nested =
    Nested


{-| -}
nestedHideable :
    { show : Bool
    , onPress : msg
    , shown : Element msg
    , hidden : Element msg
    }
    -> Child msg
nestedHideable =
    NestedHideable


{-| -}
segmentControl : SegmentControlConfig tag msg -> Element msg
segmentControl { id, label, help, onChange, options, selected, child } =
    let
        header =
            case label of
                Nothing ->
                    Element.none

                Just labelText ->
                    Element.el
                        [ attributeId (id ++ "-label")
                        , Element.width Element.fill
                        ]
                        (Ui.Theme.Typography.bodyBold labelText)

        control borderRounded =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , borderRounded
                , Border.width 1
                , Border.color Ui.Theme.Color.primary
                , Background.color Ui.Theme.Color.secondary
                ]
                (Element.row
                    [ attributeId (id ++ "-label")
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Element.htmlAttribute (Html.Attributes.attribute "role" "radiogroup")
                    , Element.htmlAttribute (Html.Attributes.tabindex 0)
                    , Element.htmlAttribute (Html.Attributes.class "segment-control")
                    , onKeyDown onChange (List.map Tuple.first options) selected
                    ]
                    (List.map (Element.map onChange) (segments options selected))
                )

        viewHelp =
            case help of
                Nothing ->
                    Element.none

                Just helpText ->
                    Element.row
                        [ Element.spacing Ui.Theme.Spacing.level1
                        , Font.color Ui.Theme.Color.danger
                        ]
                        [ Ui.Atom.Icon.fa "exclamation-circle"
                        , Ui.Theme.Typography.bodyBold helpText
                        ]
    in
    case child of
        Nothing ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.column
                    [ Element.width Element.fill ]
                    [ Ui.Theme.Focus.outline <|
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Theme.Spacing.level2
                            ]
                            [ header
                            , viewHelp
                            , control (Border.rounded 3)
                            ]
                    ]
                ]

        Just (Appended appended) ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.column
                    [ Element.width Element.fill ]
                    [ Ui.Theme.Focus.outlineTop <|
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Theme.Spacing.level2
                            ]
                            [ header
                            , viewHelp
                            , control <|
                                Border.roundEach
                                    { topLeft = 3
                                    , topRight = 3
                                    , bottomLeft = 0
                                    , bottomRight = 0
                                    }
                            ]
                    ]
                , Ui.Theme.Focus.outlineBottom appended
                ]

        Just (Nested shown) ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.spacing Ui.Theme.Spacing.level1
                    ]
                    [ Ui.Theme.Focus.outline <|
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Theme.Spacing.level2
                            ]
                            [ header
                            , viewHelp
                            , control (Border.rounded 3)
                            ]
                    , Element.el
                        [ Element.width Element.fill
                        , Element.paddingEach
                            { top = 0
                            , bottom = 0
                            , left = Ui.Theme.Spacing.level2
                            , right = 0
                            }
                        ]
                        shown
                    ]
                ]

        Just (NestedHideable { show, onPress, shown, hidden }) ->
            Element.column
                [ Element.width Element.fill
                , Element.inFront <|
                    Element.el
                        [ Element.alignRight ]
                        (disclosureButton
                            { show = show
                            , onPress = onPress
                            }
                        )
                ]
                [ if show then
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Ui.Theme.Spacing.level1
                        ]
                        [ Ui.Theme.Focus.outline <|
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing Ui.Theme.Spacing.level2
                                ]
                                [ header
                                , viewHelp
                                , control (Border.rounded 3)
                                ]
                        , Element.el
                            [ Element.width Element.fill
                            , Element.paddingEach
                                { top = 0
                                , bottom = 0
                                , left = Ui.Theme.Spacing.level2
                                , right = 0
                                }
                            ]
                            shown
                        ]

                  else
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Ui.Theme.Spacing.level2
                        , Element.padding 7
                        ]
                        [ header
                        , viewHelp
                        , hidden
                        ]
                ]


disclosureButton : { show : Bool, onPress : msg } -> Element msg
disclosureButton { show, onPress } =
    Input.button
        [ Element.mouseOver [ Font.color Ui.Theme.Color.primary ]
        , Element.focused [ Font.color Ui.Theme.Color.primary ]
        , fontColorEaseInOut
        ]
        { onPress = Just onPress
        , label =
            Element.el
                [ Element.alignRight
                , Element.padding 3
                ]
                (Ui.Theme.Focus.outline <|
                    Element.row
                        [ Element.spacing Ui.Theme.Spacing.level1 ]
                        [ Ui.Theme.Typography.button <|
                            if show then
                                "Minimize"

                            else
                                "Expand"
                        , Ui.Atom.Icon.fa <|
                            if show then
                                "chevron-up"

                            else
                                "chevron-down"
                        ]
                )
        }


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
                , Background.color Ui.Theme.Color.primary
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
                Ui.Theme.Color.primary

            else
                Ui.Theme.Color.transparent
        , backgroundColorEaseInOut
        , Font.color <|
            if selected then
                Ui.Theme.Color.white

            else
                Ui.Theme.Color.black
        , Element.mouseOver <|
            if selected then
                []

            else
                [ Background.color Ui.Theme.Color.secondaryDark ]
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
        , Border.color Ui.Theme.Color.transparent
        , Element.focused [ Border.color Ui.Theme.Color.primary ]
        ]
        (Element.el
            ([ Element.centerX ] ++ userSelectNone)
            (Ui.Theme.Typography.body label)
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



---- HELPER


focusShadow : Attr never msg
focusShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Theme.Color.primary
        }


dangerShadow : Attr never msg
dangerShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Theme.Color.danger
        }


labelAbove : { label : String, help : Maybe String } -> Label msg
labelAbove { label, help } =
    Input.labelAbove [] <|
        Element.column
            [ sansSerif
            , Element.spacing Ui.Theme.Spacing.level2
            , Element.paddingEach
                { top = 0
                , bottom = Ui.Theme.Spacing.level1
                , left = 0
                , right = 0
                }
            ]
            [ Ui.Theme.Typography.bodyBold label
            , case help of
                Nothing ->
                    Element.none

                Just helpText ->
                    Element.row
                        [ Element.spacing Ui.Theme.Spacing.level1
                        , Font.color Ui.Theme.Color.danger
                        ]
                        [ Ui.Atom.Icon.fa "exclamation-circle"
                        , Ui.Theme.Typography.bodyBold helpText
                        ]
            ]


sansSerif : Attribute msg
sansSerif =
    Font.family
        [ Font.typeface "Rubik"
        , Font.sansSerif
        ]


attributeId : String -> Attribute msg
attributeId id =
    Element.htmlAttribute (Html.Attributes.id id)


borderWidthAppended : Attribute msg
borderWidthAppended =
    Border.widthEach
        { top = 0
        , bottom = 1
        , left = 1
        , right = 1
        }


borderRoundAppended : Attribute msg
borderRoundAppended =
    Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = 3
        , bottomRight = 3
        }


backgroundColorEaseInOut : Attribute msg
backgroundColorEaseInOut =
    Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")


fontColorEaseInOut : Attribute msg
fontColorEaseInOut =
    Element.htmlAttribute (Html.Attributes.style "transition" "color 0.2s ease-in-out 0s")
