module Ui.Molecule.ObjectList exposing (view)

{-|

@docs view

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Pattern exposing (Pattern)
import Pattern.Draw exposing (Object(..))
import Ui.Atom
import Ui.Color
import Ui.Space
import Ui.Typography


type alias Config msg =
    { onHover : Object -> msg
    , onLeave : Object -> msg
    , onFocus : Object -> msg
    , onBlur : Object -> msg
    , hidePressed : Object -> msg
    , editPressed : Object -> msg
    , removePressed : Object -> msg
    }


objectName : Object -> String
objectName object =
    Maybe.withDefault "" <|
        case object of
            Point aPoint ->
                Pattern.name aPoint

            Axis aAxis ->
                Pattern.name aAxis

            Circle aCircle ->
                Pattern.name aCircle

            Curve aCurve ->
                Pattern.name aCurve

            Detail aDetail ->
                Pattern.name aDetail


view : Config msg -> Pattern coordinates -> Maybe Object -> Maybe Object -> Element msg
view cfg pattern focusedObject hoveredObject =
    let
        objects =
            List.sortBy objectName <|
                List.concat
                    [ List.map Point (Pattern.points pattern)
                    , List.map Axis (Pattern.axes pattern)
                    , List.map Circle (Pattern.circles pattern)
                    , List.map Curve (Pattern.curves pattern)
                    , List.map Detail (Pattern.details pattern)
                    ]

        field i =
            Element.text ("field " ++ String.fromInt i)
    in
    Element.el
        [ Element.width Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Border.width 3
        , Border.dotted
        , Border.color Ui.Color.transparent
        , Element.focused [ Border.color Ui.Color.primary ]
        ]
        (Element.el
            [ Element.padding 4
            , Element.width Element.fill
            , Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            ]
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarY
                ]
                (Element.table
                    [ Element.htmlAttribute (Html.Attributes.tabindex 0) ]
                    { data = objects
                    , columns =
                        [ icon cfg focusedObject hoveredObject
                        , name cfg focusedObject hoveredObject
                        , actions cfg focusedObject hoveredObject
                        ]
                    }
                )
            )
        )


icon cfg focusedObject hoveredObject =
    { header = Element.none
    , width = Element.shrink
    , view =
        \object ->
            Element.el
                [ Element.paddingEach
                    { top = Ui.Space.level1
                    , bottom = Ui.Space.level1
                    , left = Ui.Space.level3
                    , right = Ui.Space.level1
                    }
                , Element.height Element.fill
                , Element.centerY
                , Events.onMouseEnter (cfg.onHover object)
                , Events.onMouseLeave (cfg.onLeave object)
                , Events.onClick (cfg.onFocus object)
                , backgroundColor focusedObject hoveredObject object
                , fontColor focusedObject hoveredObject object
                ]
                (case object of
                    Point _ ->
                        Ui.Atom.iconPoint

                    Axis _ ->
                        Ui.Atom.iconAxis

                    Circle _ ->
                        Ui.Atom.iconCircle

                    Curve _ ->
                        Ui.Atom.iconCurve

                    Detail _ ->
                        Ui.Atom.iconDetail
                )
    }


name cfg focusedObject hoveredObject =
    { header = Element.none
    , width = Element.fill
    , view =
        \object ->
            Element.el
                [ Element.padding Ui.Space.level1
                , Events.onMouseEnter (cfg.onHover object)
                , Events.onMouseLeave (cfg.onLeave object)
                , Events.onClick (cfg.onFocus object)
                , backgroundColor focusedObject hoveredObject object
                , fontColor focusedObject hoveredObject object
                ]
                (Ui.Typography.body (objectName object))
    }


actions cfg focusedObject hoveredObject =
    { header = Element.none
    , width = Element.shrink
    , view =
        \object ->
            let
                action id icon_ onPress =
                    Input.button
                        [ Element.htmlAttribute (Html.Attributes.id (id ++ "--" ++ objectName object))
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
                , Events.onMouseEnter (cfg.onHover object)
                , Events.onMouseLeave (cfg.onLeave object)
                , Events.onClick (cfg.onFocus object)
                , backgroundColor focusedObject hoveredObject object
                , fontColor focusedObject hoveredObject object
                ]
                (if focusedObject == Just object || hoveredObject == Just object then
                    [ action "hide-btn" "eye" (cfg.hidePressed object)
                    , action "edit-btn" "edit" (cfg.editPressed object)
                    , action "remove-btn" "trash" (cfg.removePressed object)
                    ]

                 else
                    []
                )
    }


backgroundColor focusedObject hoveredObject object =
    Background.color <|
        if focusedObject == Just object then
            Ui.Color.primary

        else if hoveredObject == Just object then
            Ui.Color.secondary

        else
            Ui.Color.white


fontColor focusedObject hoveredObject object =
    Font.color <|
        if focusedObject == Just object then
            Ui.Color.white

        else
            Ui.Color.black
