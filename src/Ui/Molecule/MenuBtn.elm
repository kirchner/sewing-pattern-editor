module Ui.Molecule.MenuBtn exposing
    ( State, init
    , viewPrimary, viewSecondary, Config, Action
    , Msg, update
    )

{-|

@docs State, init
@docs viewPrimary, viewSecondary, Config, Action
@docs Msg, update

-}

import Browser.Dom
import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Task
import Ui.Atom
import Ui.Atom.Icon
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- STATE


{-| -}
type State
    = State
        { last : Int
        , selected : Int
        , open : Bool
        , preventBlur : Bool
        }


{-| -}
init : State
init =
    State
        { last = 0
        , selected = 0
        , open = False
        , preventBlur = False
        }



---- VIEW


{-| -}
type alias Config action msg =
    { id : String
    , onMsg : Msg action -> msg
    , actions : List (Action action msg)
    }


{-| -}
type alias Action action msg =
    { icon : Element msg
    , label : String
    , action : action
    }


{-| -}
viewPrimary : Config action msg -> State -> Element msg
viewPrimary =
    view
        { background = Ui.Theme.Color.primaryLight
        , backgroundMouseOver = Ui.Theme.Color.primary
        , font = Ui.Theme.Color.white
        }


{-| -}
viewSecondary : Config action msg -> State -> Element msg
viewSecondary =
    view
        { background = Ui.Theme.Color.secondary
        , backgroundMouseOver = Ui.Theme.Color.secondaryDark
        , font = Ui.Theme.Color.black
        }


type alias Colors =
    { background : Color
    , backgroundMouseOver : Color
    , font : Color
    }


{-| -}
view : Colors -> Config action msg -> State -> Element msg
view colors ({ id, onMsg, actions } as config) ((State { last, selected, open }) as state) =
    case List.getAt last actions of
        Nothing ->
            Element.none

        Just { icon, label, action } ->
            Element.row
                [ Element.spacing 0
                , Element.below <|
                    if open then
                        viewMenu config state

                    else
                        Element.none
                ]
                [ Ui.Theme.Focus.outlineLeft <|
                    Input.button
                        [ Element.paddingXY Ui.Theme.Spacing.level2 Ui.Theme.Spacing.level2
                        , Font.color colors.font
                        , Background.color colors.background
                        , Element.mouseOver [ Background.color colors.backgroundMouseOver ]
                        , backgroundColorEaseInOut
                        , Border.roundEach
                            { topLeft = 3
                            , topRight = 0
                            , bottomLeft = 3
                            , bottomRight = 0
                            }
                        ]
                        { onPress = Just (onMsg (PressedActionButton action))
                        , label = icon
                        }
                , Ui.Theme.Focus.outlineRight <|
                    Input.button
                        [ attributeId (menuBtnId id)
                        , Element.paddingXY Ui.Theme.Spacing.level1 Ui.Theme.Spacing.level2
                        , Element.height (Element.px 38)
                        , Element.width (Element.px 22)
                        , Font.color colors.font
                        , Background.color colors.background
                        , Element.mouseOver [ Background.color colors.backgroundMouseOver ]
                        , backgroundColorEaseInOut
                        , Border.roundEach
                            { topLeft = 0
                            , topRight = 3
                            , bottomLeft = 0
                            , bottomRight = 3
                            }
                        , Events.onMouseDown (onMsg MouseDownOnMenuButton)
                        , Events.onMouseUp (onMsg MouseUpOnMenuButton)
                        ]
                        { onPress = Just (onMsg (PressedMenuButton id))
                        , label = Ui.Atom.Icon.faSmall "chevron-down"
                        }
                ]


viewMenu : Config action msg -> State -> Element msg
viewMenu { id, onMsg, actions } (State { selected }) =
    Element.column
        [ attributeId (menuId id)
        , htmlAttribute "tabindex" "-1"
        , htmlAttribute "aria-labelledby" (menuBtnId id)
        , htmlAttribute "aria-activedescendant" (menuItemId id selected)
        , Element.htmlAttribute <|
            Html.Events.preventDefaultOn "keydown" (keyDownDecoder actions selected onMsg)
        , Events.onLoseFocus (onMsg BluredMenu)
        , Element.moveRight 7
        , Border.rounded 3
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 6
            , color = Ui.Theme.Color.grayDark
            }
        , Background.color Ui.Theme.Color.white
        ]
        (List.indexedMap (viewAction onMsg selected) actions)


keyDownDecoder : List (Action action msg) -> Int -> (Msg action -> msg) -> Decoder ( msg, Bool )
keyDownDecoder actions selected onMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\rawKey ->
                case rawKey of
                    "ArrowUp" ->
                        Decode.succeed
                            ( onMsg (PressedArrowUp (List.length actions)), True )

                    "ArrowDown" ->
                        Decode.succeed
                            ( onMsg (PressedArrowDown (List.length actions)), True )

                    "Enter" ->
                        case List.getAt selected actions of
                            Nothing ->
                                Decode.fail "not handling that key here"

                            Just { action } ->
                                Decode.succeed
                                    ( onMsg (PressedEnter action), True )

                    _ ->
                        Decode.fail "not handling that key here"
            )


viewAction : (Msg action -> msg) -> Int -> Int -> Action action msg -> Element msg
viewAction onMsg selected index { icon, label, action } =
    Element.row
        [ Element.pointer
        , Element.padding Ui.Theme.Spacing.level2
        , Element.spacing Ui.Theme.Spacing.level3
        , Element.width Element.fill
        , Element.mouseOver <|
            if selected == index then
                [ Background.color Ui.Theme.Color.primary
                , Font.color Ui.Theme.Color.white
                ]

            else
                [ Background.color Ui.Theme.Color.secondary ]
        , Background.color <|
            if selected == index then
                Ui.Theme.Color.primary

            else
                Ui.Theme.Color.transparent
        , Font.color <|
            if selected == index then
                Ui.Theme.Color.white

            else
                Ui.Theme.Color.black
        , Element.htmlAttribute (Html.Events.onClick (onMsg (PressedMenuItem index action)))
        ]
        [ icon
        , Ui.Theme.Typography.button label
        ]



---- IDS


menuBtnId : String -> String
menuBtnId id =
    id ++ "--menu-btn"


menuId : String -> String
menuId id =
    id ++ "--menu"


menuItemId : String -> Int -> String
menuItemId id index =
    id ++ "--menu-item-" ++ String.fromInt index



---- HELPERS


attributeId : String -> Attribute msg
attributeId id =
    Element.htmlAttribute (Html.Attributes.id id)


htmlAttribute : String -> String -> Attribute msg
htmlAttribute name value =
    Element.htmlAttribute (Html.Attributes.attribute name value)


backgroundColorEaseInOut : Attribute msg
backgroundColorEaseInOut =
    Element.htmlAttribute (Html.Attributes.style "transition" "background-color 0.2s ease-in-out 0s")



---- UPDATE


{-| -}
type
    Msg action
    -- ACTION BUTTON
    = PressedActionButton action
      -- MENU BUTTON
    | PressedMenuButton String
    | MouseDownOnMenuButton
    | MouseUpOnMenuButton
    | AttemptedFocus
      -- MENU
    | BluredMenu
    | PressedArrowUp Int
    | PressedArrowDown Int
    | PressedEnter action
      -- MENU ITEM
    | PressedMenuItem Int action


{-| -}
update : Msg action -> State -> ( State, Cmd (Msg action), Maybe action )
update msg ((State stuff) as state) =
    case msg of
        -- ACTION BUTTON
        PressedActionButton action ->
            ( state
            , Cmd.none
            , Just action
            )

        -- MENU BUTTON
        PressedMenuButton id ->
            ( State
                { stuff
                    | open = not stuff.open
                    , selected = 0
                }
            , if stuff.open then
                Cmd.none

              else
                Task.attempt (\_ -> AttemptedFocus) (Browser.Dom.focus (menuId id))
            , Nothing
            )

        MouseDownOnMenuButton ->
            ( State { stuff | preventBlur = True }
            , Cmd.none
            , Nothing
            )

        MouseUpOnMenuButton ->
            ( State { stuff | preventBlur = False }
            , Cmd.none
            , Nothing
            )

        AttemptedFocus ->
            ( state, Cmd.none, Nothing )

        BluredMenu ->
            if stuff.preventBlur then
                ( state, Cmd.none, Nothing )

            else
                ( State { stuff | open = False }
                , Cmd.none
                , Nothing
                )

        PressedArrowUp numberOfActions ->
            ( State { stuff | selected = modBy numberOfActions (stuff.selected - 1) }
            , Cmd.none
            , Nothing
            )

        PressedArrowDown numberOfActions ->
            ( State { stuff | selected = modBy numberOfActions (stuff.selected + 1) }
            , Cmd.none
            , Nothing
            )

        PressedEnter action ->
            ( State
                { stuff
                    | open = False
                    , last = stuff.selected
                }
            , Cmd.none
            , Just action
            )

        PressedMenuItem index action ->
            ( State
                { stuff
                    | open = False
                    , last = index
                }
            , Cmd.none
            , Just action
            )
