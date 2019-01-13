module View.Input exposing
    ( btnCallToAction
    , btnCancel
    , btnDanger
    , btnDangerIcon
    , btnIcon
    , btnIconLarge
    , btnPrimary
    , btnSecondary
    , btnSecondaryWide
    , dropdown
    , dropdownAppended
    , dropdownWithMenu
    , formula
    , listbox
    , option
    , optionCustom
    , radioColumn
    , radioRow
    , text
    )

{-
   Sewing pattern editor
   Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

import Design
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attributes
import Html.Events as Events
import Listbox exposing (Listbox)
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Store exposing (Entry)
import That exposing (That)
import Those exposing (Those)
import View.Icon



---- BUTTONS


btnPrimary : { onPress : Maybe msg, label : String } -> Element msg
btnPrimary { onPress, label } =
    Input.button
        [ Element.paddingXY 18 10
        , Background.color Design.primary
        , Font.color Design.white
        , Design.fontSmall
        , Element.mouseOver
            [ Background.color Design.primaryDark ]
        , Element.htmlAttribute <|
            Attributes.style "transition" "background-color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label =
            Element.text label
        }


btnSecondary : String -> { onPress : Maybe msg, label : String } -> Element msg
btnSecondary =
    btnSecondaryHelp Element.shrink


btnSecondaryWide : String -> { onPress : Maybe msg, label : String } -> Element msg
btnSecondaryWide =
    btnSecondaryHelp Element.fill


btnSecondaryHelp :
    Element.Length
    -> String
    -> { onPress : Maybe msg, label : String }
    -> Element msg
btnSecondaryHelp width id { onPress, label } =
    Input.button
        [ Element.htmlAttribute <|
            Attributes.id id
        , Element.width width
        , Element.paddingXY 18 10
        , Background.color Design.secondary
        , Font.color Design.black
        , Design.fontSmall
        , Element.mouseOver
            [ Background.color Design.secondaryDark ]
        , Element.htmlAttribute <|
            Attributes.style "transition" "background-color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label =
            Element.text label
        }


btnCallToAction : String -> { onPress : Maybe msg, label : String } -> Element msg
btnCallToAction id { onPress, label } =
    Input.button
        [ Element.htmlAttribute <|
            Attributes.id id
        , Element.width Element.fill
        , Element.paddingXY 18 10
        , Border.rounded 7
        , Background.color Design.secondary
        , Font.color Design.black
        , Design.fontSmall
        , Element.mouseOver
            [ Background.color Design.secondaryDark ]
        , Element.htmlAttribute <|
            Attributes.style "transition" "background-color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label =
            Element.row
                [ Element.width Element.fill ]
                [ Element.text label
                , Element.el
                    [ Element.alignRight ]
                    (View.Icon.fa "chevron-right")
                ]
        }


btnDanger : { onPress : Maybe msg, label : String } -> Element msg
btnDanger { onPress, label } =
    Input.button
        [ Element.paddingXY 18 10
        , Background.color Design.danger
        , Font.color Design.white
        , Design.fontSmall
        , Element.mouseOver
            [ Background.color Design.dangerDark ]
        , Element.htmlAttribute <|
            Attributes.style "transition" "background-color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label =
            Element.text label
        }


btnDangerIcon : { onPress : Maybe msg, icon : String } -> Element msg
btnDangerIcon { onPress, icon } =
    Input.button
        [ Font.color Design.danger
        , Element.mouseOver
            [ Font.color Design.dangerDark ]
        ]
        { onPress = onPress
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
                (View.Icon.fa icon)
        }


btnCancel : { onPress : Maybe msg, label : String } -> Element msg
btnCancel { onPress, label } =
    Input.button
        [ Element.paddingXY 18 10
        , Design.fontSmall
        , Font.color Design.primary
        , Font.underline
        , Element.mouseOver
            [ Font.color Design.primaryDark ]
        ]
        { onPress = onPress
        , label =
            Element.text label
        }


btnIcon : { onPress : Maybe msg, icon : String } -> Element msg
btnIcon { onPress, icon } =
    Input.button
        [ Font.color Design.black
        , Element.mouseOver
            [ Font.color Design.primaryDark ]
        ]
        { onPress = onPress
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
                (View.Icon.fa icon)
        }


btnIconLarge : { onPress : Maybe msg, icon : String } -> Element msg
btnIconLarge { onPress, icon } =
    Input.button
        [ Font.color Design.black
        , Element.mouseOver
            [ Font.color Design.primaryDark ]
        ]
        { onPress = onPress
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
                (View.Icon.faLarge icon)
        }



---- INPUTS


text :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        , help : Maybe String
        }
    -> Element msg
text id data =
    Input.text
        [ Element.htmlAttribute <|
            Attributes.id id
        , Element.width Element.fill
        , Element.padding Design.xSmall
        , Design.fontNormal
        , Font.color Design.black
        , Background.color Design.white
        , Border.rounded 3
        , Border.width 1
        , Border.color Design.black
        ]
        { onChange = data.onChange
        , text = data.text
        , placeholder = Nothing
        , label =
            Input.labelAbove []
                (case data.help of
                    Nothing ->
                        Element.el
                            [ Design.fontSmall
                            , Font.color Design.black
                            , Font.bold
                            , Element.paddingXY 0 Design.xxSmall
                            ]
                            (Element.text data.label)

                    Just helpText ->
                        Element.column
                            [ Element.spacing Design.xxSmall ]
                            [ Element.el
                                [ Design.fontSmall
                                , Font.color Design.black
                                , Font.bold
                                ]
                                (Element.text data.label)
                            , Element.row
                                [ Element.spacing Design.xSmall
                                , Element.paddingXY 0 Design.xxSmall
                                , Font.color Design.danger
                                , Design.fontSmall
                                ]
                                [ View.Icon.fa "exclamation-circle"
                                , Element.text helpText
                                ]
                            ]
                )
        }


formula :
    String
    ->
        { onChange : String -> msg
        , text : String
        , label : String
        , help : Maybe String
        }
    -> Element msg
formula id data =
    let
        lineCount =
            List.length (String.split "\n" data.text)

        padding =
            if lineCount == 1 then
                Element.padding 5

            else
                Element.paddingEach
                    { left =
                        if lineCount < 10 then
                            30

                        else
                            40
                    , right = 5
                    , top = 5
                    , bottom = 5
                    }
    in
    Input.multiline
        [ Element.htmlAttribute (Attributes.id id)
        , Element.width Element.fill
        , Element.inFront (lineNumbers lineCount)
        , padding
        , Element.spacing Design.xSmall
        , Font.size 16
        , Font.color Design.black
        , Design.monospace
        , Background.color Design.white
        , Border.width 1
        , Border.rounded 3
        , Border.color Design.black
        , Element.htmlAttribute (Attributes.rows lineCount)
        , Element.htmlAttribute (Attributes.style "white-space" "pre")
        , Element.clip
        ]
        { onChange = data.onChange
        , text = data.text
        , placeholder = Nothing
        , spellcheck = False
        , label =
            Input.labelAbove []
                (case data.help of
                    Nothing ->
                        Element.el
                            [ Design.fontSmall
                            , Font.color Design.black
                            , Font.bold
                            , Design.sansSerif
                            ]
                            (Element.text data.label)

                    Just helpText ->
                        Element.column
                            [ Element.spacing Design.xxSmall ]
                            [ Element.el
                                [ Design.fontSmall
                                , Font.color Design.black
                                , Font.bold
                                , Design.sansSerif
                                ]
                                (Element.text data.label)
                            , Element.row
                                [ Element.spacing Design.xSmall
                                , Element.paddingEach
                                    { left = 0
                                    , right = 0
                                    , top = Design.xxSmall
                                    , bottom = 0
                                    }
                                , Font.color Design.danger
                                , Design.fontSmall
                                , Design.sansSerif
                                ]
                                [ View.Icon.fa "exclamation-circle"
                                , Element.text helpText
                                ]
                            ]
                )
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
                , Font.color Design.black
                , Design.monospace
                , Element.spacing 5
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
                    , Background.color Design.black
                    ]
                    Element.none
                )
            ]



---- RADIO


radioRow id { onChange, options, selected, label } =
    Input.radioRow
        [ Element.htmlAttribute (Attributes.id id)
        , Element.width Element.fill
        , Element.paddingXY Design.xxSmall Design.xSmall
        , Element.spacing Design.normal
        , Font.size 16
        , Font.color Design.black
        ]
        { onChange = onChange
        , options = options
        , selected = selected
        , label =
            Input.labelAbove
                [ Font.size 12
                , Font.color Design.black
                , Font.bold
                ]
                (Element.text label)
        }


radioColumn id { onChange, options, selected, label } =
    Input.radio
        [ Element.htmlAttribute (Attributes.id id)
        , Element.width Element.fill
        , Element.paddingEach
            { left = Design.xxSmall
            , right = 0
            , top = Design.xSmall
            , bottom = Design.xSmall
            }
        , Element.spacing Design.xSmall
        , Font.size 14
        , Font.color Design.black
        ]
        { onChange = onChange
        , options = options
        , selected = selected
        , label =
            Input.labelAbove
                [ Font.size 12
                , Font.color Design.black
                , Font.bold
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
        [ Element.spacing 10
        , Element.alignLeft
        , Element.width Element.fill
        ]
        [ Element.el
            ([ Element.width (Element.px 14)
             , Element.height (Element.px 14)
             , Element.alignTop
             , Background.color Design.white
             , Border.rounded 7
             , Border.width <|
                case status of
                    Input.Idle ->
                        1

                    Input.Focused ->
                        1

                    Input.Selected ->
                        5
             , Border.color <|
                case status of
                    Input.Idle ->
                        Design.black

                    Input.Focused ->
                        Design.black

                    Input.Selected ->
                        Design.primary
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



---- DROPDPOWNS


dropdown :
    String
    ->
        { lift : Dropdown.Msg entry -> msg
        , entryToString : entry -> String
        , entryToHash : entry -> String
        , label : String
        , options : List entry
        , dropdown : Dropdown
        , selection : Maybe entry
        }
    -> Element msg
dropdown =
    dropdownWithMenu Element.none


dropdownWithMenu :
    Element msg
    -> String
    ->
        { lift : Dropdown.Msg entry -> msg
        , entryToString : entry -> String
        , entryToHash : entry -> String
        , label : String
        , options : List entry
        , dropdown : Dropdown
        , selection : Maybe entry
        }
    -> Element msg
dropdownWithMenu menu id data =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.xSmall
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Element.htmlAttribute (Attributes.id (id ++ "-label"))
                , Element.alignLeft
                , Font.size 12
                , Font.color Design.black
                , Font.bold
                ]
                (Element.text data.label)
            , Element.el [ Element.alignRight ]
                menu
            ]
        , Dropdown.customView dropdownDomFunctions
            (dropdownViewConfig False data.entryToString data.entryToHash)
            { id = id
            , labelledBy = id ++ "-label"
            , lift = data.lift
            }
            (List.map Listbox.option data.options)
            data.dropdown
            data.selection
        ]


dropdownViewConfig appended printOption hashOption =
    Dropdown.customViewConfig hashOption
        { container =
            [ Element.width Element.fill
            , Element.height (Element.px 30)
            ]
        , button =
            \{ maybeSelection } ->
                { attributes =
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.padding 5
                    , Font.size 16
                    , Font.color Design.black
                    , if appended then
                        Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 3
                            , bottomRight = 3
                            }

                      else
                        Border.rounded 3
                    , Border.width 1
                    , Border.color Design.black
                    , Background.color Design.white
                    , Element.mouseOver
                        [ Border.color Design.primaryDark
                        , Font.color Design.primaryDark
                        ]
                    ]
                , children =
                    [ Element.text <|
                        case maybeSelection of
                            Nothing ->
                                ""

                            Just that ->
                                printOption that
                    , Element.el
                        [ Element.alignRight
                        , Element.paddingXY 10 0
                        ]
                        (View.Icon.fa "chevron-down")
                    ]
                }
        , ul =
            [ Element.width Element.fill
            , Element.height
                (Element.fill
                    |> Element.maximum 200
                )
            , Element.scrollbarY
            , Font.color Design.black
            , Background.color Design.white
            , Border.width 1
            , Border.rounded 3
            , Border.color Design.black
            ]
        , liOption =
            \{ focused, hovered } thatPoint ->
                let
                    defaultAttrs =
                        [ Element.pointer
                        , Element.padding 10
                        , Font.size 16
                        , Element.width Element.fill
                        ]
                in
                { attributes =
                    if hovered then
                        [ Background.color Design.secondaryDark ] ++ defaultAttrs

                    else if focused then
                        [ Background.color Design.secondary ] ++ defaultAttrs

                    else
                        defaultAttrs
                , children =
                    [ Element.text (printOption thatPoint) ]
                }
        , liDivider =
            \_ ->
                { attributes = []
                , children = []
                }
        }


dropdownAppended :
    String
    ->
        { lift : Dropdown.Msg entry -> msg
        , entryToString : entry -> String
        , entryToHash : entry -> String
        , label : String
        , options : List entry
        , dropdown : Dropdown
        , selection : Maybe entry
        }
    -> Element msg
dropdownAppended id data =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.xSmall
        ]
        [ Dropdown.customView dropdownDomFunctions
            (dropdownViewConfig True data.entryToString data.entryToHash)
            { id = id
            , labelledBy = id ++ "-label"
            , lift = data.lift
            }
            (List.map Listbox.option data.options)
            data.dropdown
            data.selection
        ]


dropdownDomFunctions =
    let
        attribute name value =
            Element.htmlAttribute (Attributes.attribute name value)

        style name value =
            Element.htmlAttribute (Attributes.style name value)

        on event decoder =
            Element.htmlAttribute (Events.on event decoder)

        preventDefaultOn event decoder =
            Element.htmlAttribute (Events.preventDefaultOn event decoder)
    in
    { ul = Element.column
    , li = Element.row
    , button =
        \attributes children ->
            Input.button attributes
                { onPress = Nothing
                , label =
                    Element.row
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        children
                }
    , div =
        \attributes children ->
            Element.el (Element.below children.ul :: attributes) children.button
    , style = style
    , text = Element.text
    , attribute = attribute
    , on = on
    , preventDefaultOn = preventDefaultOn
    , attributeMap = \noOp -> Element.mapAttribute (\_ -> noOp)
    , htmlMap = \noOp -> Element.map (\_ -> noOp)
    }



---- LISTBOX


listbox :
    String
    ->
        { lift : Listbox.Msg (That object) -> msg
        , entryToString : That object -> String
        , label : String
        , options : List ( That object, Entry object )
        , listbox : Listbox
        , selection : Those object
        }
    -> Element msg
listbox id data =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 3
        ]
        [ Element.el
            [ Element.htmlAttribute (Attributes.id (id ++ "-label"))
            , Font.size 12
            , Font.color Design.black
            , Font.bold
            ]
            (Element.text data.label)
        , Listbox.customView listboxDomFunctions
            (listboxViewConfig data.entryToString)
            { id = id
            , labelledBy = id ++ "-label"
            , lift = data.lift
            }
            (List.map (Tuple.first >> Listbox.option) data.options)
            data.listbox
            (Those.toList data.selection)
        ]


listboxViewConfig printOption =
    Listbox.customViewConfig That.hash
        { ul =
            [ Element.width Element.fill
            , Element.height (Element.px 400)
            , Element.padding 1
            , Element.scrollbarY
            , Font.color Design.black
            , Background.color Design.white
            , Border.width 1
            , Border.rounded 3
            , Border.color Design.black
            ]
        , liOption =
            \{ selected, focused, hovered } thatPoint ->
                let
                    defaultAttrs =
                        [ Element.pointer
                        , Element.paddingEach
                            { left = 0
                            , right = 10
                            , top = 10
                            , bottom = 10
                            }
                        , Font.size 16
                        , Element.width Element.fill
                        ]
                in
                { attributes =
                    if hovered then
                        [ Background.color Design.secondaryDark ] ++ defaultAttrs

                    else if focused then
                        [ Background.color Design.secondary ] ++ defaultAttrs

                    else
                        defaultAttrs
                , children =
                    [ Element.el
                        [ Element.width (Element.px 30) ]
                        (if selected then
                            Element.el [ Element.centerX ]
                                (View.Icon.fa "check")

                         else
                            Element.none
                        )
                    , Element.text (printOption thatPoint)
                    ]
                }
        , liDivider = \_ -> { attributes = [], children = [] }
        , empty = Element.text ""
        , focusable = True
        }


listboxDomFunctions =
    let
        attribute name value =
            Element.htmlAttribute (Attributes.attribute name value)

        on event decoder =
            Element.htmlAttribute (Events.on event decoder)

        preventDefaultOn event decoder =
            Element.htmlAttribute (Events.preventDefaultOn event decoder)
    in
    { ul = Element.column
    , li = Element.row
    , attribute = attribute
    , on = on
    , preventDefaultOn = preventDefaultOn
    , attributeMap = \noOp -> Element.mapAttribute (\_ -> noOp)
    , htmlMap = \noOp -> Element.map (\_ -> noOp)
    }
