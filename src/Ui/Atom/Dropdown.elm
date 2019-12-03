module Ui.Atom.Dropdown exposing
    ( Msg, Dropdown, Instance, init
    , ViewConfig, view, viewAppended
    , UpdateConfig, update, subscriptions
    )

{-|

@docs Msg, Dropdown, Instance, init

@docs ViewConfig, view, viewAppended
@docs UpdateConfig, update, subscriptions

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attributes
import Html.Events as Events
import Listbox
import Listbox.Dropdown as Dropdown
import Ui.Atom
import Ui.Atom.Input exposing (Child(..))
import Ui.Color
import Ui.Space
import Ui.Typography


{-| -}
type alias Msg entry =
    Dropdown.Msg entry


{-| -}
type alias Dropdown =
    Dropdown.Dropdown



---- INIT


{-| -}
init : Dropdown
init =
    Dropdown.init



---- VIEW


{-| -}
type alias Instance entry msg =
    { id : String
    , label : String
    , lift : Msg entry -> msg
    }


{-| -}
type alias ViewConfig entry =
    { entryToString : entry -> String
    , entryToHash : entry -> String
    }


{-| -}
view :
    ViewConfig entry
    -> Instance entry msg
    -> List entry
    -> Dropdown
    -> Maybe entry
    -> Element msg
view =
    viewWithMenu Element.none


viewWithMenu :
    Element msg
    -> ViewConfig entry
    -> Instance entry msg
    -> List entry
    -> Dropdown
    -> Maybe entry
    -> Element msg
viewWithMenu menu config instance options dropdown selection =
    Ui.Atom.withFocusOutline <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing Ui.Space.level2
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ Element.el
                    [ Element.htmlAttribute (Attributes.id (instance.id ++ "-label"))
                    , Element.alignLeft
                    ]
                    (Ui.Typography.bodyBold instance.label)
                , Element.el [ Element.alignRight ]
                    menu
                ]
            , Dropdown.customView dropdownDomFunctions
                (dropdownViewConfig False config.entryToString config.entryToHash)
                { id = instance.id
                , label = Listbox.labelledBy (instance.id ++ "-label")
                , lift = instance.lift
                }
                (List.map Listbox.option options)
                dropdown
                selection
            ]


{-| -}
viewAppended :
    ViewConfig entry
    -> Instance entry msg
    -> List entry
    -> Dropdown
    -> Maybe entry
    -> Child msg
viewAppended config instance options dropdown selection =
    Appended <|
        Dropdown.customView dropdownDomFunctions
            (dropdownViewConfig True config.entryToString config.entryToHash)
            { id = instance.id
            , label = Listbox.labelledBy (instance.id ++ "-label")
            , lift = instance.lift
            }
            (List.map Listbox.option options)
            dropdown
            selection


dropdownViewConfig :
    Bool
    -> (entry -> String)
    -> (entry -> String)
    -> Dropdown.CustomViewConfig entry Never (Element.Attribute Never) (Element Never)
dropdownViewConfig appended printOption hashOption =
    Dropdown.customViewConfig hashOption
        { container =
            [ Element.width Element.fill ]
        , button =
            \{ maybeSelection } ->
                { attributes =
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.padding (Ui.Space.level2 - 2)
                    , Font.size 16
                    , if appended then
                        Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 3
                            , bottomRight = 3
                            }

                      else
                        Border.rounded 3
                    , if appended then
                        Border.widthEach
                            { top = 0
                            , bottom = 1
                            , left = 1
                            , right = 1
                            }

                      else
                        Border.width 1
                    , Border.color Ui.Color.black
                    , Background.color Ui.Color.white
                    , Element.mouseOver
                        [ Border.color Ui.Color.primary ]
                    , Element.focused
                        [ Border.color Ui.Color.primary
                        , Border.innerShadow
                            { offset = ( 0, 0 )
                            , size = 1
                            , blur = 0
                            , color = Ui.Color.primary
                            }
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
                        , Element.paddingXY Ui.Space.level1 0
                        , Element.focused
                            [ Font.color Ui.Color.primary ]
                        , Element.mouseOver
                            [ Font.color Ui.Color.primary ]
                        ]
                        (Ui.Atom.fa "chevron-down")
                    ]
                }
        , ul =
            [ Element.width Element.fill
            , Element.height
                (Element.fill
                    |> Element.maximum 200
                )
            , Element.moveDown Ui.Space.level2
            , Border.rounded 3
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 6
                , color = Ui.Color.grayDark
                }
            , Element.scrollbarY
            , Background.color Ui.Color.white
            ]
        , liOption =
            \{ focused, hovered, selected } thatPoint ->
                { attributes =
                    [ Element.pointer
                    , Element.padding Ui.Space.level2
                    , Font.size 16
                    , Font.color <|
                        if selected then
                            Ui.Color.white

                        else
                            Ui.Color.black
                    , Element.width Element.fill
                    , Background.color <|
                        if selected then
                            Ui.Color.primary

                        else if hovered || focused then
                            Ui.Color.secondary

                        else
                            Ui.Color.transparent
                    ]
                , children =
                    [ Element.text (printOption thatPoint) ]
                }
        , liDivider =
            \_ ->
                { attributes = []
                , children = []
                }
        }


dropdownDomFunctions =
    let
        property name value =
            Element.htmlAttribute (Attributes.property name value)

        attribute name value =
            Element.htmlAttribute (Attributes.attribute name value)

        style name value =
            Element.htmlAttribute (Attributes.style name value)

        on event decoder =
            Element.htmlAttribute (Events.on event decoder)

        preventDefaultOn event decoder =
            Element.htmlAttribute (Events.preventDefaultOn event decoder)
    in
    { div =
        \attributes children ->
            Element.el (Element.below children.ul :: attributes) children.button
    , text = Element.text
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
    , ul = Element.column
    , li = Element.row
    , on = on
    , preventDefaultOn = preventDefaultOn
    , property = property
    , attribute = attribute
    , style = style
    , attributeMap = \noOp -> Element.mapAttribute (\_ -> noOp)
    , htmlMap = \noOp -> Element.map (\_ -> noOp)
    }



---- UPDATE


{-| -}
type alias UpdateConfig entry =
    { entryToHash : entry -> String }


{-| -}
update :
    UpdateConfig entry
    -> List entry
    -> Dropdown.Msg entry
    -> Dropdown
    -> Maybe entry
    -> ( Dropdown, Cmd (Dropdown.Msg entry), Maybe entry )
update { entryToHash } options =
    Dropdown.update (dropdownUpdateConfig entryToHash) (List.map Listbox.option options)


{-| -}
subscriptions : Dropdown -> Sub (Msg entry)
subscriptions =
    Dropdown.subscriptions


dropdownUpdateConfig : (entry -> String) -> Dropdown.UpdateConfig entry
dropdownUpdateConfig entryToHash =
    Dropdown.updateConfig
        { uniqueId = entryToHash
        , behaviour =
            { jumpAtEnds = True
            , separateFocus = False
            , selectionFollowsFocus = False
            , handleHomeAndEnd = True
            , closeAfterMouseSelection = True
            , typeAhead = Listbox.noTypeAhead
            , minimalGap = 0
            , initialGap = 0
            }
        }
