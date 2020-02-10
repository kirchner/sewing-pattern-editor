module Ui.Atom exposing (LinkConfig, link)

{-|


# Links

@docs LinkConfig, link

-}

import Element exposing (Attribute, Element)
import Element.Font as Font
import Element.Input as Input exposing (Label)
import Html.Attributes
import Ui.Theme.Color



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
