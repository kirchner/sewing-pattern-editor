module Ui.Atom exposing
    ( btnPrimary, btnSecondary, btnDanger, btnCancel
    , btnCallToAction
    , btnIcon, btnIconDanger, btnIconLarge
    , checkbox
    , radioRow, radioColumn, option
    , fa, faBody, faLarge
    )

{-|


# Buttons

@docs btnPrimary, btnSecondary, btnDanger, btnCancel
@docs btnCallToAction
@docs btnIcon, btnIconDanger, btnIconLarge


# Form Elements

@docs checkbox
@docs radioRow, radioColumn, option


# Icons

@docs fa, faBody, faLarge

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Ui.Color
import Ui.Space
import Ui.Typography



---- STANDARD


btnPrimary : { onPress : Maybe msg, label : String } -> Element msg
btnPrimary { onPress, label } =
    Input.button
        [ Element.paddingXY Ui.Space.level3 Ui.Space.level2
        , Font.color Ui.Color.white
        , Background.color Ui.Color.primaryLight
        , Element.mouseOver [ Background.color Ui.Color.primary ]
        , Element.htmlAttribute (Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
        ]
        { onPress = onPress
        , label = Ui.Typography.button label
        }


btnSecondary : String -> { onPress : Maybe msg, label : String } -> Element msg
btnSecondary =
    btnSecondaryHelp Element.shrink


btnSecondaryHelp : Element.Length -> String -> { onPress : Maybe msg, label : String } -> Element msg
btnSecondaryHelp width id { onPress, label } =
    Input.button
        [ Element.htmlAttribute (Attributes.id id)
        , Element.paddingXY Ui.Space.level3 Ui.Space.level2
        , Element.width width
        , Background.color Ui.Color.secondary
        , Element.mouseOver [ Background.color Ui.Color.secondaryDark ]
        , Element.htmlAttribute (Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
        ]
        { onPress = onPress
        , label = Ui.Typography.button label
        }


btnDanger : { onPress : Maybe msg, label : String } -> Element msg
btnDanger { onPress, label } =
    Input.button
        [ Element.paddingXY Ui.Space.level3 Ui.Space.level2
        , Font.color Ui.Color.white
        , Background.color Ui.Color.danger
        , Element.mouseOver [ Background.color Ui.Color.dangerDark ]
        , Element.htmlAttribute (Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
        ]
        { onPress = onPress
        , label = Ui.Typography.button label
        }


btnCancel : { onPress : Maybe msg, label : String } -> Element msg
btnCancel { onPress, label } =
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
    Input.button
        [ Element.htmlAttribute (Attributes.id id)
        , Element.width Element.fill
        , Element.paddingXY Ui.Space.level3 Ui.Space.level2
        , Border.rounded Ui.Space.level1
        , Background.color Ui.Color.secondary
        , Element.mouseOver [ Background.color Ui.Color.secondaryDark ]
        , Element.htmlAttribute (Attributes.style "transition" "background-color 0.2s ease-in-out 0s")
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
    Input.checkbox
        [ Element.width Element.fill ]
        { onChange = onChange
        , icon = checkboxIcon
        , checked = checked
        , label =
            Input.labelRight
                [ Font.size 16
                , Element.centerY
                , Element.paddingXY Ui.Space.level1 0
                ]
                (Element.text label)
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
                Element.rgb (59 / 255) (153 / 255) (252 / 255)

            else
                Element.rgb (211 / 255) (211 / 255) (211 / 255)
        , Border.shadow <|
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    Element.rgba (238 / 255) (238 / 255) (238 / 255) 0

                else
                    Element.rgb (238 / 255) (238 / 255) (238 / 255)
            }
        , Background.color <|
            if checked then
                Ui.Color.primary

            else
                Ui.Color.white
        , Border.width <|
            if checked then
                0

            else
                1
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


radioRow id { onChange, options, selected, label } =
    Input.radioRow
        [ Element.htmlAttribute (Attributes.id id)
        , Element.width Element.fill
        , Element.spacing Ui.Space.level4
        , Font.size 16
        ]
        { onChange = onChange
        , options = options
        , selected = selected
        , label =
            Input.labelAbove
                [ Font.size 16
                , Font.bold
                , Element.paddingEach
                    { top = 0
                    , bottom = Ui.Space.level2
                    , left = 0
                    , right = 0
                    }
                ]
                (Element.text label)
        }


radioColumn id { onChange, options, selected, label } =
    Input.radio
        [ Element.htmlAttribute (Attributes.id id)
        , Element.width Element.fill
        , Element.spacing Ui.Space.level2
        , Font.size 16
        ]
        { onChange = onChange
        , options = options
        , selected = selected
        , label =
            Input.labelAbove
                [ Font.size 16
                , Font.bold
                , Element.paddingEach
                    { top = 0
                    , bottom = Ui.Space.level2
                    , left = 0
                    , right = 0
                    }
                ]
                (Element.text label)
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
                    Element.htmlAttribute (Attributes.class "focusable")
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
                Attributes.class "unfocusable"
            ]
            label
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
                    [ Attributes.class "fas"
                    , Attributes.class ("fa-" ++ name)
                    , Attributes.style "font-size" sizePx
                    , Attributes.style "width" sizePx
                    , Attributes.style "height" sizePx
                    , Attributes.style "text-align" "center"
                    , Attributes.style "color" "inherit"
                    ]
                    []
        )
