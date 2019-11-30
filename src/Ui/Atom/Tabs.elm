module Ui.Atom.Tabs exposing (Config, view)

{-|

@docs Config, Tab, view

-}

import Element exposing (Element)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Ui.Atom
import Ui.Color
import Ui.Space
import Ui.Typography


type alias Config tag msg =
    { label : String
    , tabs : List (Tab tag)
    , selected : tag
    , content : tag -> Element msg
    , onSelect : tag -> msg
    }


type alias Tab tag =
    { tag : tag
    , id : String
    , label : String
    }


view : Config tag msg -> Element msg
view { label, tabs, selected, content, onSelect } =
    let
        viewTab tab =
            Input.button
                ([ Element.paddingXY 0 Ui.Space.level1
                 , Font.color <|
                    if tab.tag == selected then
                        Ui.Color.primary

                    else
                        Ui.Color.grayDark
                 , Element.mouseOver
                    [ Font.color Ui.Color.black ]
                 , Border.widthEach
                    { top = 0
                    , bottom = 2
                    , left = 0
                    , right = 0
                    }
                 , Border.color <|
                    if tab.tag == selected then
                        Ui.Color.primary

                    else
                        Ui.Color.transparent
                 , attributeId (tab.id ++ "--tab")
                 , attribute "role" "tab"
                 , attribute "aria-controls" (tab.id ++ "--tabpanel")
                 , attribute "aria-selected" <|
                    if tab.tag == selected then
                        "true"

                    else
                        "false"
                 ]
                    |> addTabindex tab.tag
                )
                { onPress = Just (onSelect tab.tag)
                , label = Ui.Typography.bodyBold tab.label
                }

        addTabindex tag attrs =
            if tag == selected then
                attrs

            else
                tabindex -1 :: attrs

        selectedId =
            List.find (\{ tag } -> tag == selected) tabs
                |> Maybe.map .id
                |> Maybe.withDefault ""
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        ]
        [ Ui.Atom.withFocusOutline <|
            Element.row
                [ Element.width Element.fill
                , Element.spacing Ui.Space.level3
                , Border.widthEach
                    { top = 0
                    , bottom = 1
                    , left = 0
                    , right = 0
                    }
                , Border.color Ui.Color.secondaryDark
                , attribute "role" "tablist"
                , attribute "aria-label" label
                , onKeyDown onSelect (List.map .tag tabs) selected
                ]
                (List.map viewTab tabs)
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            , attribute "role" "tabpanel"
            , attributeId (selectedId ++ "--tabpanel")
            , attribute "aria-labelledby" (selectedId ++ "--tab")
            ]
            (content selected)
        ]


onKeyDown : (tab -> msg) -> List tab -> tab -> Element.Attribute msg
onKeyDown toMsg tabs selectedTab =
    let
        ( prevTab, nextTab ) =
            case List.splitWhen (\thisTab -> thisTab == selectedTab) tabs of
                Nothing ->
                    unchanged

                Just ( start, end ) ->
                    case ( List.last start, end ) of
                        ( Nothing, _ :: next :: _ ) ->
                            case List.last tabs of
                                Nothing ->
                                    unchanged

                                Just last ->
                                    ( last, next )

                        ( Just prev, _ :: next :: _ ) ->
                            ( prev, next )

                        ( Just prev, _ :: [] ) ->
                            case List.head tabs of
                                Nothing ->
                                    unchanged

                                Just first ->
                                    ( prev, first )

                        _ ->
                            unchanged

        unchanged =
            ( selectedTab, selectedTab )
    in
    Element.htmlAttribute <|
        Html.Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                Decode.succeed (toMsg prevTab)

                            "ArrowUp" ->
                                Decode.succeed (toMsg prevTab)

                            "ArrowRight" ->
                                Decode.succeed (toMsg nextTab)

                            "ArrowDown" ->
                                Decode.succeed (toMsg nextTab)

                            _ ->
                                Decode.fail "not handling that key here"
                    )
            )



---- HELPER


attributeId : String -> Element.Attribute msg
attributeId id =
    Element.htmlAttribute (Html.Attributes.id id)


tabindex : Int -> Element.Attribute msg
tabindex index =
    Element.htmlAttribute (Html.Attributes.tabindex index)


attribute : String -> String -> Element.Attribute msg
attribute name id =
    Element.htmlAttribute (Html.Attributes.attribute name id)
