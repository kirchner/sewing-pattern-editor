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
import Ui.Atom
import Ui.Color
import Ui.Space
import Ui.Typography


type alias Config msg =
    { onHover : String -> msg
    , onLeave : String -> msg
    , onFocus : String -> msg
    , onBlur : String -> msg
    , editPressed : String -> msg
    , removePressed : String -> msg
    }


view : Config msg -> Pattern coordinates -> Maybe String -> Maybe String -> Element msg
view cfg pattern focusedVariable hoveredVariable =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Border.widthEach
            { top = 1
            , bottom = 3
            , left = 1
            , right = 1
            }
        , Border.rounded 3
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.complementary ]
        ]
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
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


name cfg focusedVariable hoveredVariable =
    { header = Element.none
    , width = Element.fill
    , view =
        \variable ->
            Element.el
                [ Element.paddingEach
                    { top = Ui.Space.level1
                    , bottom = Ui.Space.level1
                    , left = Ui.Space.level3
                    , right = Ui.Space.level1
                    }
                , Events.onMouseEnter (cfg.onHover variable)
                , Events.onMouseLeave (cfg.onLeave variable)
                , Events.onClick (cfg.onFocus variable)
                , backgroundColor focusedVariable hoveredVariable variable
                , fontColor focusedVariable hoveredVariable variable
                ]
                (Ui.Typography.body variable)
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
                        , Element.mouseOver [ Font.color Ui.Color.primaryDark ]
                        ]
                        { onPress = Just onPress
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                                (Ui.Atom.fa icon_)
                        }
            in
            Element.row
                [ Element.paddingEach
                    { top = Ui.Space.level1
                    , bottom = Ui.Space.level1
                    , left = Ui.Space.level1
                    , right = Ui.Space.level3
                    }
                , Element.spacing Ui.Space.level2
                , Element.height Element.fill
                , Element.centerY
                , Events.onMouseEnter (cfg.onHover variable)
                , Events.onMouseLeave (cfg.onLeave variable)
                , Events.onClick (cfg.onFocus variable)
                , backgroundColor focusedVariable hoveredVariable variable
                , fontColor focusedVariable hoveredVariable variable
                ]
                (if focusedVariable == Just variable || hoveredVariable == Just variable then
                    [ action "edit-btn" "edit" (cfg.editPressed variable)
                    , action "remove-btn" "trash" (cfg.removePressed variable)
                    ]

                 else
                    []
                )
    }


backgroundColor focusedVariable hoveredVariable variable =
    Background.color <|
        if focusedVariable == Just variable then
            Ui.Color.primary

        else if hoveredVariable == Just variable then
            Ui.Color.secondary

        else
            Ui.Color.white


fontColor focusedVariable hoveredVariable variable =
    Font.color <|
        if focusedVariable == Just variable then
            Ui.Color.white

        else
            Ui.Color.black
