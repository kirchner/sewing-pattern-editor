module Ui.Molecule.VariableList exposing (Config, view)

{-|

@docs Config, view

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Pattern exposing (Pattern)
import Ui.Atom.Icon
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography


{-| -}
type alias Config msg =
    { onHover : String -> msg
    , onLeave : String -> msg
    , onFocus : String -> msg
    , onBlur : String -> msg
    , editPressed : String -> msg
    , removePressed : String -> msg
    , editable : Bool
    }


{-| -}
view : Config msg -> Pattern coordinates -> Maybe String -> Maybe String -> Element msg
view cfg pattern focusedVariable hoveredVariable =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Border.widthEach
            { top = 1
            , bottom = 4
            , left = 1
            , right = 1
            }
        , Border.color Ui.Theme.Color.secondaryDark
        , Element.focused [ Border.color Ui.Theme.Color.complementary ]
        ]
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            , Element.paddingXY 0 Ui.Theme.Spacing.level1
            ]
            (Element.table
                [ Element.htmlAttribute (Html.Attributes.tabindex 0) ]
                { data = List.sort (Pattern.variables pattern)
                , columns =
                    [ name cfg focusedVariable hoveredVariable
                    , actions cfg focusedVariable hoveredVariable
                    ]
                }
            )
        )


name :
    Config msg
    -> Maybe String
    -> Maybe String
    ->
        { header : Element msg
        , view : String -> Element msg
        , width : Element.Length
        }
name cfg focusedVariable hoveredVariable =
    { header = Element.none
    , width = Element.fill
    , view =
        \variable ->
            Element.el
                [ Element.paddingEach
                    { top = Ui.Theme.Spacing.level1
                    , bottom = Ui.Theme.Spacing.level1
                    , left = Ui.Theme.Spacing.level3
                    , right = Ui.Theme.Spacing.level1
                    }
                , Events.onMouseEnter (cfg.onHover variable)
                , Events.onMouseLeave (cfg.onLeave variable)
                , Events.onClick (cfg.onFocus variable)
                , backgroundColor focusedVariable hoveredVariable variable
                , fontColor focusedVariable hoveredVariable variable
                ]
                (Ui.Theme.Typography.body variable)
    }


actions :
    Config msg
    -> Maybe String
    -> Maybe String
    ->
        { header : Element msg
        , view : String -> Element msg
        , width : Element.Length
        }
actions cfg focusedVariable hoveredVariable =
    { header = Element.none
    , width = Element.shrink
    , view =
        \variable ->
            let
                action id icon_ onPress =
                    Input.button
                        [ Element.htmlAttribute (Html.Attributes.id (id ++ "--" ++ variable))
                        , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
                        ]
                        { onPress = Just onPress
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                                (Ui.Atom.Icon.fa icon_)
                        }
            in
            Element.row
                [ Element.paddingEach
                    { top = Ui.Theme.Spacing.level1
                    , bottom = Ui.Theme.Spacing.level1
                    , left = Ui.Theme.Spacing.level1
                    , right = Ui.Theme.Spacing.level3
                    }
                , Element.spacing Ui.Theme.Spacing.level2
                , Element.height Element.fill
                , Element.centerY
                , Events.onMouseEnter (cfg.onHover variable)
                , Events.onMouseLeave (cfg.onLeave variable)
                , Events.onClick (cfg.onFocus variable)
                , backgroundColor focusedVariable hoveredVariable variable
                , fontColor focusedVariable hoveredVariable variable
                ]
                (if focusedVariable == Just variable || hoveredVariable == Just variable then
                    if cfg.editable then
                        [ action "edit-btn" "edit" (cfg.editPressed variable)
                        , action "remove-btn" "trash" (cfg.removePressed variable)
                        ]

                    else
                        []

                 else
                    []
                )
    }


backgroundColor : Maybe String -> Maybe String -> String -> Element.Attribute msg
backgroundColor focusedVariable hoveredVariable variable =
    Background.color <|
        if focusedVariable == Just variable then
            Ui.Theme.Color.primary

        else if hoveredVariable == Just variable then
            Ui.Theme.Color.secondary

        else
            Ui.Theme.Color.white


fontColor : Maybe String -> Maybe String -> String -> Element.Attribute msg
fontColor focusedVariable _ variable =
    Font.color <|
        if focusedVariable == Just variable then
            Ui.Theme.Color.white

        else
            Ui.Theme.Color.black
