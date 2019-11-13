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
import Ui.Color
import Ui.Space
import Ui.Typography



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
    , actions : List (Action action)
    }


{-| -}
type alias Action action =
    { label : String
    , action : action
    }


{-| -}
viewPrimary : Config action msg -> State -> Element msg
viewPrimary =
    view
        { background = Ui.Color.primaryLight
        , backgroundMouseOver = Ui.Color.primary
        , font = Ui.Color.white
        }


{-| -}
viewSecondary : Config action msg -> State -> Element msg
viewSecondary =
    view
        { background = Ui.Color.secondary
        , backgroundMouseOver = Ui.Color.secondaryDark
        , font = Ui.Color.black
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

        Just { label, action } ->
            Element.row
                [ Element.spacing 1
                , Element.below <|
                    if open then
                        viewMenu config state

                    else
                        Element.none
                ]
                [ Ui.Atom.withFocusOutlineLeft <|
                    Input.button
                        [ Element.paddingXY Ui.Space.level3 Ui.Space.level2
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
                        , label = Ui.Typography.button label
                        }
                , Ui.Atom.withFocusOutlineRight <|
                    Input.button
                        [ attributeId (menuBtnId id)
                        , Element.padding Ui.Space.level2
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
                        , label = Ui.Atom.fa "chevron-down"
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
        , Element.width (Element.minimum 200 Element.fill)
        , Element.moveDown 4
        , Element.moveRight 7
        , Border.rounded 3
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 6
            , color = Ui.Color.grayDark
            }
        , Background.color Ui.Color.white
        ]
        (List.indexedMap (viewAction onMsg selected) actions)


keyDownDecoder : List (Action action) -> Int -> (Msg action -> msg) -> Decoder ( msg, Bool )
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


viewAction : (Msg action -> msg) -> Int -> Int -> Action action -> Element msg
viewAction onMsg selected index { label, action } =
    Element.el
        [ Element.pointer
        , Element.padding Ui.Space.level2
        , Element.width Element.fill
        , Element.mouseOver <|
            if selected == index then
                [ Background.color Ui.Color.primary
                , Font.color Ui.Color.white
                ]

            else
                [ Background.color Ui.Color.secondary ]
        , Background.color <|
            if selected == index then
                Ui.Color.primary

            else
                Ui.Color.transparent
        , Font.color <|
            if selected == index then
                Ui.Color.white

            else
                Ui.Color.black
        , Element.htmlAttribute (Html.Events.onClick (onMsg (PressedMenuItem index action)))
        ]
        (Ui.Typography.button label)



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
