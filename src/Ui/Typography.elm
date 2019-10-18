module Ui.Typography exposing
    ( headingOne, headingTwo, headingThree, headingFour
    , bodyBold, body
    , button
    )

{-|

@docs headingOne, headingTwo, headingThree, headingFour

@docs bodyBold, body

@docs button

-}

import Element exposing (Attribute, Element)
import Element.Font as Font
import Element.Region as Region


headingOne : String -> Element msg
headingOne text =
    Element.paragraph
        [ Font.size 44
        , Font.family
            [ Font.external
                { name = "Mansalva"
                , url = "https://fonts.googleapis.com/css?family=Mansalva"
                }
            ]
        , Region.heading 1
        ]
        [ Element.text text ]


headingTwo : String -> Element msg
headingTwo text =
    heading { level = 2, fontSize = 36 } text


headingThree : String -> Element msg
headingThree text =
    heading { level = 3, fontSize = 28 } text


headingFour : String -> Element msg
headingFour text =
    heading { level = 4, fontSize = 20 } text


heading : { level : Int, fontSize : Int } -> String -> Element msg
heading { level, fontSize } text =
    Element.paragraph
        [ Font.size fontSize
        , Region.heading level
        ]
        [ Element.text text ]


bodyBold : String -> Element msg
bodyBold text =
    Element.el
        [ Font.size 16
        , Font.bold
        ]
        (Element.text text)


body : String -> Element msg
body text =
    Element.el
        [ Font.size 16 ]
        (Element.text text)


button : String -> Element msg
button text =
    Element.el
        [ Font.size 14 ]
        (Element.text text)
