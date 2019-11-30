module Ui.Atom exposing
    ( BtnConfig, btnPrimary, btnSecondary, btnDanger, btnCancel
    , btnCallToAction
    , IconBtnConfig, btnIcon, btnIconDanger, btnIconLarge
    , LinkConfig, link
    , CheckboxConfig, checkbox
    , RadioConfig, radioRow, radioColumn, option
    , SegmentControlConfig, segmentControl, Child(..), nested, nestedHideable
    , TextConfig, inputText, inputTextAppended, inputFormula, inputFormulaAppended
    , fa, faBody, faLarge, faBrandLarge
    , iconPoint, iconAxis, iconCircle, iconCurve, iconDetail
    , withFocusOutline
    , withFocusOutlineTop, withFocusOutlineBottom, withFocusOutlineLeft, withFocusOutlineRight
    )

{-|


# Buttons

@docs BtnConfig, btnPrimary, btnSecondary, btnDanger, btnCancel
@docs btnCallToAction
@docs IconBtnConfig, btnIcon, btnIconDanger, btnIconLarge


# Links

@docs LinkConfig, link


# Form Elements

@docs CheckboxConfig, checkbox
@docs RadioConfig, radioRow, radioColumn, option
@docs SegmentControlConfig, segmentControl, Child, nested, nestedHideable
@docs TextConfig, inputText, inputTextAppended, inputFormula, inputFormulaAppended


# Icons

@docs fa, faBody, faLarge, faBrandLarge
@docs iconPoint, iconAxis, iconCircle, iconCurve, iconDetail

@docs withFocusOutline
@docs withFocusOutlineTop, withFocusOutlineBottom, withFocusOutlineLeft, withFocusOutlineRight

-}

import Circle2d
import Element exposing (Attr, Attribute, Decoration, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Label)
import Geometry.Svg as Svg
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import LineSegment2d
import List.Extra as List
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Pixels exposing (pixels)
import Point2d
import Polygon2d
import QuadraticSpline2d
import Svg
import Svg.Attributes
import Ui.Color
import Ui.Space
import Ui.Typography



---- STANDARD


{-| -}
type alias BtnConfig msg =
    { id : String
    , onPress : Maybe msg
    , label : String
    }


{-| -}
btnPrimary : BtnConfig msg -> Element msg
btnPrimary { id, onPress, label } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Font.color Ui.Color.white
            , Background.color Ui.Color.primaryLight
            , Element.mouseOver [ Background.color Ui.Color.primary ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }


{-| -}
btnSecondary : BtnConfig msg -> Element msg
btnSecondary { id, onPress, label } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Background.color Ui.Color.secondary
            , Element.mouseOver [ Background.color Ui.Color.secondaryDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }


{-| -}
btnDanger : BtnConfig msg -> Element msg
btnDanger { id, onPress, label } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Font.color Ui.Color.white
            , Background.color Ui.Color.danger
            , Element.mouseOver [ Background.color Ui.Color.dangerDark ]
            , backgroundColorEaseInOut
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }


{-| -}
btnCancel : BtnConfig msg -> Element msg
btnCancel { id, onPress, label } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Font.color Ui.Color.primary
            , Font.underline
            , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = Ui.Typography.button label
            }



---- CALL TO ACTION


{-| -}
btnCallToAction : BtnConfig msg -> Element msg
btnCallToAction { id, onPress, label } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.width Element.fill
            , Element.paddingXY Ui.Space.level3 Ui.Space.level2
            , Border.rounded Ui.Space.level1
            , Background.color Ui.Color.secondary
            , Element.mouseOver [ Background.color Ui.Color.secondaryDark ]
            , backgroundColorEaseInOut
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


{-| -}
type alias IconBtnConfig msg =
    { id : String
    , onPress : Maybe msg
    , icon : String
    }


{-| -}
btnIcon : IconBtnConfig msg -> Element msg
btnIcon { id, onPress, icon } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = btnIconLabel (fa icon)
            }


{-| -}
btnIconDanger : IconBtnConfig msg -> Element msg
btnIconDanger { id, onPress, icon } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Font.color Ui.Color.danger
            , Element.mouseOver [ Font.color Ui.Color.dangerDark ]
            ]
            { onPress = onPress
            , label = btnIconLabel (fa icon)
            }


{-| -}
btnIconLarge : IconBtnConfig msg -> Element msg
btnIconLarge { id, onPress, icon } =
    withFocusOutline <|
        Input.button
            [ attributeId id
            , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
            ]
            { onPress = onPress
            , label = btnIconLabel (faLarge icon)
            }


{-| -}
btnIconLabel : Element msg -> Element msg
btnIconLabel icon =
    Element.el
        [ Element.centerX
        , Element.centerY
        ]
        icon



---- LINKS


{-| -}
type alias LinkConfig msg =
    { id : String
    , onPress : Maybe msg
    , label : String
    }


{-| -}
link : LinkConfig msg -> Element msg
link { id, onPress, label } =
    Input.button
        [ attributeId id
        , Font.underline
        , Font.color Ui.Color.primary
        , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
        , Element.htmlAttribute <|
            Html.Attributes.style "transition" "color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label = Element.text label
        }



---- CHECKBOX


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
    withFocusOutline <|
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
    withFocusOutline <|
        Input.radioRow
            [ attributeId id
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


{-| -}
radioColumn : RadioConfig value msg -> Element msg
radioColumn { id, onChange, options, selected, label } =
    withFocusOutline <|
        Input.radio
            [ attributeId id
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


{-| -}
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
            , Element.htmlAttribute (Html.Attributes.class "unfocusable")
            ]
            label
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
                        (Ui.Typography.bodyBold labelText)

        control borderRounded =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , borderRounded
                , Border.width 1
                , Border.color Ui.Color.primary
                , Background.color Ui.Color.secondary
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
                        [ Element.spacing Ui.Space.level1
                        , Font.color Ui.Color.danger
                        ]
                        [ fa "exclamation-circle"
                        , Ui.Typography.bodyBold helpText
                        ]
    in
    case child of
        Nothing ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.column
                    [ Element.width Element.fill ]
                    [ withFocusOutline <|
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Space.level2
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
                    [ withFocusOutlineTop <|
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Space.level2
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
                , withFocusOutlineBottom appended
                ]

        Just (Nested shown) ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.spacing Ui.Space.level1
                    ]
                    [ withFocusOutline <|
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing Ui.Space.level2
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
                            , left = Ui.Space.level2
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
                        , Element.spacing Ui.Space.level1
                        ]
                        [ withFocusOutline <|
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing Ui.Space.level2
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
                                , left = Ui.Space.level2
                                , right = 0
                                }
                            ]
                            shown
                        ]

                  else
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Ui.Space.level2
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
        [ Element.mouseOver [ Font.color Ui.Color.primary ]
        , Element.focused [ Font.color Ui.Color.primary ]
        , fontColorEaseInOut
        ]
        { onPress = Just onPress
        , label =
            Element.el
                [ Element.alignRight
                , Element.padding 3
                ]
                (withFocusOutline <|
                    Element.row
                        [ Element.spacing Ui.Space.level1 ]
                        [ Ui.Typography.button <|
                            if show then
                                "Minimize"

                            else
                                "Expand"
                        , fa <|
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
        , backgroundColorEaseInOut
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
        , Element.focused [ Border.color Ui.Color.primary ]
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


{-| -}
type alias TextConfig msg =
    { id : String
    , onChange : String -> msg
    , text : String
    , label : String
    , help : Maybe String
    }


{-| -}
inputText : TextConfig msg -> Element msg
inputText data =
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
                [ attributeId data.id
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
                labelAbove
                    { label = data.label
                    , help = data.help
                    }
            }


{-| -}
inputTextAppended :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        }
    -> Child msg
inputTextAppended id data =
    Appended <|
        Input.text
            [ attributeId id
            , Element.width Element.fill
            , Element.padding 10
            , Font.size 16
            , Background.color Ui.Color.white
            , borderWidthAppended
            , borderRoundAppended
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


{-| -}
inputFormula : TextConfig msg -> Element msg
inputFormula data =
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
                [ attributeId data.id
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
                labelAbove
                    { label = data.label
                    , help = data.help
                    }
            }


{-| -}
inputFormulaAppended :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        }
    -> Child msg
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
    in
    Appended <|
        Input.multiline
            [ attributeId id
            , Element.width Element.fill
            , Element.inFront (lineNumbers lineCount)
            , padding
            , Element.spacing Ui.Space.level1
            , Font.size 16
            , Font.family [ Font.monospace ]
            , Background.color Ui.Color.white
            , borderWidthAppended
            , borderRoundAppended
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


{-| -}
fa : String -> Element msg
fa name =
    faHelp "fas" 14 name


{-| -}
faBody : String -> Element msg
faBody name =
    faHelp "fas" 16 name


{-| -}
faLarge : String -> Element msg
faLarge name =
    faHelp "fas" 24 name


{-| -}
faBrandLarge : String -> Element msg
faBrandLarge name =
    faHelp "fab" 24 name


faHelp : String -> Int -> String -> Element msg
faHelp class size name =
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
                    [ Html.Attributes.class class
                    , Html.Attributes.class ("fa-" ++ name)
                    , Html.Attributes.style "font-size" sizePx
                    , Html.Attributes.style "width" sizePx
                    , Html.Attributes.style "height" sizePx
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "color" "inherit"
                    ]
                    []
        )


{-| -}
iconPoint : Element msg
iconPoint =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.circle2d
                    [ Svg.Attributes.fill "black"
                    ]
                    (Circle2d.withRadius
                        (pixels 2)
                        Point2d.origin
                    )
                ]


{-| -}
iconAxis : Element msg
iconAxis =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.lineSegment2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "3"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (LineSegment2d.from
                        (Point2d.pixels -5 0)
                        (Point2d.pixels 5 0)
                    )
                ]


{-| -}
iconCircle : Element msg
iconCircle =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.circle2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (Circle2d.withRadius
                        (pixels 5)
                        Point2d.origin
                    )
                ]


{-| -}
iconCurve : Element msg
iconCurve =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.quadraticSpline2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeLinecap "round"
                    ]
                    (QuadraticSpline2d.fromControlPoints
                        (Point2d.pixels -5 5)
                        (Point2d.pixels 5 5)
                        (Point2d.pixels 5 -5)
                    )
                ]


{-| -}
iconDetail : Element msg
iconDetail =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-7 -7 14 14"
                , Html.Attributes.style "width" "14px"
                , Html.Attributes.style "height" "14px"
                ]
                [ Svg.polygon2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke "black"
                    ]
                    (Polygon2d.singleLoop
                        [ Point2d.pixels -5 5
                        , Point2d.pixels 5 5
                        , Point2d.pixels 5 -5
                        , Point2d.pixels -5 -3
                        ]
                    )
                ]



---- WITH FOCUS OUTLINE


{-| -}
withFocusOutline : Element msg -> Element msg
withFocusOutline element =
    Element.el
        [ Element.width Element.fill
        , Border.width 3
        , Border.rounded 3
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.complementary ]
        ]
        (Element.el
            [ Element.padding 4
            , Element.width Element.fill
            ]
            element
        )


{-| -}
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
        , Border.roundEach
            { topLeft = 3
            , topRight = 3
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.complementary ]
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


{-| -}
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
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 3
            , bottomRight = 3
            }
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.complementary ]
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


{-| -}
withFocusOutlineLeft : Element msg -> Element msg
withFocusOutlineLeft element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 3
            , left = 3
            , right = 0
            }
        , Border.roundEach
            { topLeft = 3
            , topRight = 0
            , bottomLeft = 3
            , bottomRight = 0
            }
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 4
                , left = 4
                , right = 0
                }
            , Element.width Element.fill
            ]
            element
        )


{-| -}
withFocusOutlineRight : Element msg -> Element msg
withFocusOutlineRight element =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 3
            , bottom = 3
            , left = 0
            , right = 3
            }
        , Border.roundEach
            { topLeft = 0
            , topRight = 3
            , bottomLeft = 0
            , bottomRight = 3
            }
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.complementary ]
        ]
        (Element.el
            [ Element.paddingEach
                { top = 4
                , bottom = 4
                , left = 0
                , right = 4
                }
            , Element.width Element.fill
            ]
            element
        )


focusShadow : Attr never msg
focusShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Color.primary
        }


dangerShadow : Attr never msg
dangerShadow =
    Border.innerShadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = Ui.Color.danger
        }


labelAbove : { label : String, help : Maybe String } -> Label msg
labelAbove { label, help } =
    Input.labelAbove [] <|
        Element.column
            [ sansSerif
            , Element.spacing Ui.Space.level2
            , Element.paddingEach
                { top = 0
                , bottom = Ui.Space.level1
                , left = 0
                , right = 0
                }
            ]
            [ Ui.Typography.bodyBold label
            , case help of
                Nothing ->
                    Element.none

                Just helpText ->
                    Element.row
                        [ Element.spacing Ui.Space.level1
                        , Font.color Ui.Color.danger
                        ]
                        [ fa "exclamation-circle"
                        , Ui.Typography.bodyBold helpText
                        ]
            ]


sansSerif : Attribute msg
sansSerif =
    Font.family
        [ Font.external
            { name = "Rubik"
            , url = "https://fonts.googleapis.com/css?family=Rubik:300"
            }
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
