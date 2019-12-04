module Ui.Atom exposing (LinkConfig, link)

{-|


# Links

@docs LinkConfig, link

-}

import Element exposing (Attr, Attribute, Decoration, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Label)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Listbox
import Listbox.Dropdown as Dropdown exposing (Dropdown)
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography



---- LINKS


{-| -}
type alias LinkConfig msg =
    { id : String
    , onPress : Maybe msg
    , label : String
    }


{-| -}
link : LinkConfig msg -> Element msg
link { id, onPress, label } =
    Input.button
        [ attributeId id
        , Font.underline
        , Font.color Ui.Theme.Color.primary
        , Element.mouseOver [ Font.color Ui.Theme.Color.primaryDark ]
        , Element.htmlAttribute <|
            Html.Attributes.style "transition" "color 0.2s ease-in-out 0s"
        ]
        { onPress = onPress
        , label = Element.text label
        }


attributeId : String -> Attribute msg
attributeId id =
    Element.htmlAttribute (Html.Attributes.id id)
