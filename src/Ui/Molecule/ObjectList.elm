module Ui.Molecule.ObjectList exposing
    ( State, init
    , Config, view
    , Msg, update
    )

{-|

@docs State, init
@docs Config, view
@docs Msg, update

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Pattern exposing (Object(..), Pattern)
import Ui.Atom
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- MODEL


{-| -}
type alias State =
    { hoveredObject : Maybe Object
    , focusedObject : Maybe Object
    , selectedObject : Maybe Object
    }


{-| -}
init : State
init =
    { hoveredObject = Nothing
    , focusedObject = Nothing
    , selectedObject = Nothing
    }



---- VIEW


{-| -}
type alias Config msg =
    { toMsg : Msg -> msg
    , hidePressed : Object -> msg
    , editPressed : Object -> msg
    , removePressed : Object -> msg
    }


{-| -}
view : Config msg -> Pattern coordinates -> State -> Element msg
view cfg pattern state =
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
                [ Element.htmlAttribute (Html.Attributes.tabindex 0)
                , Element.htmlAttribute <|
                    Html.Events.preventDefaultOn "keydown"
                        (Decode.field "code" Decode.string
                            |> Decode.andThen
                                (\code ->
                                    case code of
                                        "ArrowUp" ->
                                            Decode.succeed ( cfg.toMsg PressedArrowUp, True )

                                        "ArrowDown" ->
                                            Decode.succeed ( cfg.toMsg PressedArrowDown, True )

                                        "Space" ->
                                            Decode.succeed ( cfg.toMsg PressedSpace, True )

                                        _ ->
                                            Decode.fail "not handling that key here"
                                )
                        )
                ]
                { data = objects
                , columns =
                    [ icon cfg state
                    , name cfg state
                    , actions cfg state
                    ]
                }
            )
        )


icon cfg state =
    { header = Element.none
    , width = Element.shrink
    , view =
        \object ->
            Element.el
                [ Element.paddingEach
                    { top = Ui.Theme.Spacing.level1
                    , bottom = Ui.Theme.Spacing.level1
                    , left = Ui.Theme.Spacing.level3
                    , right = Ui.Theme.Spacing.level1
                    }
                , Element.height Element.fill
                , Element.centerY
                , Events.onMouseEnter (cfg.toMsg (HoveredObject object))
                , Events.onMouseLeave (cfg.toMsg (LeftObject object))
                , Events.onClick (cfg.toMsg (ClickedObject object))
                , Border.widthEach
                    { top = 1
                    , bottom = 1
                    , left = 1
                    , right = 0
                    }
                , borderColor state object
                , backgroundColor state object
                , fontColor state object
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


name cfg state =
    { header = Element.none
    , width = Element.fill
    , view =
        \object ->
            Element.el
                [ Element.padding Ui.Theme.Spacing.level1
                , Events.onMouseEnter (cfg.toMsg (HoveredObject object))
                , Events.onMouseLeave (cfg.toMsg (LeftObject object))
                , Events.onClick (cfg.toMsg (ClickedObject object))
                , Border.widthEach
                    { top = 1
                    , bottom = 1
                    , left = 0
                    , right = 0
                    }
                , borderColor state object
                , backgroundColor state object
                , fontColor state object
                ]
                ( Ui.Theme.Typography.body (objectName object))
    }


actions cfg state =
    { header = Element.none
    , width = Element.shrink
    , view =
        \object ->
            let
                action id icon_ onPress =
                    Input.button
                        [ Element.htmlAttribute (Html.Attributes.id (id ++ "--" ++ objectName object))
                        , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
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
                    { top = Ui.Theme.Spacing.level1
                    , bottom = Ui.Theme.Spacing.level1
                    , left = Ui.Theme.Spacing.level1
                    , right = Ui.Theme.Spacing.level3
                    }
                , Element.spacing Ui.Theme.Spacing.level2
                , Element.height Element.fill
                , Element.centerY
                , Events.onMouseEnter (cfg.toMsg (HoveredObject object))
                , Events.onMouseLeave (cfg.toMsg (LeftObject object))
                , Events.onClick (cfg.toMsg (ClickedObject object))
                , Border.widthEach
                    { top = 1
                    , bottom = 1
                    , left = 0
                    , right = 1
                    }
                , borderColor state object
                , backgroundColor state object
                , fontColor state object
                ]
                (if state.focusedObject == Just object || state.hoveredObject == Just object then
                    [ action "hide-btn" "eye" (cfg.hidePressed object)
                    , action "edit-btn" "edit" (cfg.editPressed object)
                    , action "remove-btn" "trash" (cfg.removePressed object)
                    ]

                 else
                    []
                )
    }


borderColor state object =
    Border.color <|
        if state.focusedObject == Just object then
            Ui.Theme.Color.black

        else if state.selectedObject == Just object then
            Ui.Theme.Color.primary

        else if state.hoveredObject == Just object then
            Ui.Theme.Color.secondary

        else
            Ui.Theme.Color.white


backgroundColor state object =
    Background.color <|
        if state.selectedObject == Just object then
            Ui.Theme.Color.primary

        else if state.hoveredObject == Just object then
            Ui.Theme.Color.secondary

        else
            Ui.Theme.Color.white


fontColor state object =
    Font.color <|
        if state.selectedObject == Just object then
            Ui.Theme.Color.white

        else
            Ui.Theme.Color.black



---- UPDATE


{-| -}
type Msg
    = -- OBJECTS
      HoveredObject Object
    | LeftObject Object
    | FocusedObject Object
    | ClickedObject Object
      -- KEYBOARD
    | PressedArrowDown
    | PressedArrowUp
    | PressedSpace


{-| -}
update : Msg -> Pattern coordinates -> State -> State
update msg pattern state =
    case msg of
        -- OBJECTS
        HoveredObject object ->
            { state | hoveredObject = Just object }

        LeftObject _ ->
            { state | hoveredObject = Nothing }

        FocusedObject object ->
            { state | focusedObject = Just object }

        ClickedObject object ->
            { state
                | focusedObject = Just object
                , selectedObject = Just object
            }

        -- KEYBOARD
        PressedArrowDown ->
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
            in
            { state
                | focusedObject =
                    state.focusedObject
                        |> Maybe.map (nextAfter objects)
            }

        PressedArrowUp ->
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
            in
            { state
                | focusedObject =
                    state.focusedObject
                        |> Maybe.map (nextAfter (List.reverse objects))
            }

        PressedSpace ->
            { state | selectedObject = state.focusedObject }


nextAfter : List a -> a -> a
nextAfter listA a =
    case listA of
        [] ->
            a

        first :: rest ->
            if first == a then
                Maybe.withDefault first (List.head rest)

            else
                nextAfterHelp a first rest


nextAfterHelp : a -> a -> List a -> a
nextAfterHelp a first listA =
    case listA of
        [] ->
            first

        next :: rest ->
            if next == a then
                Maybe.withDefault first (List.head rest)

            else
                nextAfterHelp a first rest


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
