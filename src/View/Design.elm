module View.Design exposing
    ( black
    , danger
    , dangerDark
    , fontLarge
    , fontNormal
    , fontSmall
    , grayDark
    , large
    , monospace
    , normal
    , primary
    , primaryDark
    , sansSerif
    , secondary
    , secondaryDark
    , small
    , white
    , xLarge
    , xSmall
    , xxSmall
    , xxxSmall
    )

{-

   http://paletton.com/#uid=43i0s0kmQrEaoKRhxwdrum-xsg2

-}

import Element
import Element.Font as Font



---- SPACING


xxxSmall : Int
xxxSmall =
    2


xxSmall : Int
xxSmall =
    4


xSmall : Int
xSmall =
    8


small : Int
small =
    16


normal : Int
normal =
    32


large : Int
large =
    64


xLarge : Int
xLarge =
    128



---- FONT


fontSmall =
    Font.size 13


fontNormal =
    Font.size 15


fontLarge =
    Font.size 18


sansSerif =
    Font.family
        [ Font.external
            { name = "Roboto"
            , url = "https://fonts.googleapis.com/css?family=Roboto"
            }
        , Font.sansSerif
        ]


monospace =
    Font.family
        [ Font.external
            { name = "Roboto Mono"
            , url = "https://fonts.googleapis.com/css?family=Roboto+Mono"
            }
        , Font.monospace
        ]



---- MAIN COLORS


primary =
    Element.rgb255 38 132 132


primaryDark =
    Element.rgb255 15 108 108


secondary =
    Element.rgb255 239 238 234


secondaryDark =
    Element.rgb255 217 215 205


danger =
    Element.rgb255 220 71 63


dangerDark =
    Element.rgb255 180 33 25



---- GRADIENTS


white =
    Element.rgb255 255 255 255


black =
    Element.rgb255 36 28 21


grayDark =
    Element.rgba255 36 28 21 0.65
