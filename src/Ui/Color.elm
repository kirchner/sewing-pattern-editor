module Ui.Color exposing
    ( primaryBright, primaryLight, primary, primaryDark
    , secondary, secondaryDark
    , complementary, complementaryDark
    , neutral, neutralDark
    , danger, dangerDark
    , success
    , white, black
    , grayDark
    , transparent
    )

{-|


# Color

<http://paletton.com/#uid=73i2p0kmQrEaoKRhxwdrum-xsg2>


## Main

@docs primaryBright, primaryLight, primary, primaryDark
@docs secondary, secondaryDark
@docs complementary, complementaryDark
@docs neutral, neutralDark
@docs danger, dangerDark
@docs success


## Gradients

@docs white, black
@docs grayDark

@docs transparent

-}

import Element exposing (Color)



---- MAIN COLORS


{-| -}
primaryBright : Color
primaryBright =
    Element.rgb255 138 204 204


{-| -}
primaryLight : Color
primaryLight =
    Element.rgb255 70 154 154


{-| -}
primary : Color
primary =
    Element.rgb255 38 132 132


{-| -}
primaryDark : Color
primaryDark =
    Element.rgb255 15 108 108


{-| -}
secondary : Color
secondary =
    Element.rgb255 239 238 234


{-| -}
secondaryDark : Color
secondaryDark =
    Element.rgb255 217 215 205


{-| -}
complementary : Color
complementary =
    Element.rgb255 220 134 63


{-| -}
complementaryDark : Color
complementaryDark =
    Element.rgb255 180 95 25


{-| -}
neutral : Color
neutral =
    Element.rgb255 220 166 63


{-| -}
neutralDark : Color
neutralDark =
    Element.rgb255 180 127 25


{-| -}
danger : Color
danger =
    Element.rgb255 220 71 63


{-| -}
dangerDark : Color
dangerDark =
    Element.rgb255 180 33 25


{-| -}
success : Color
success =
    Element.rgb255 48 169 69



---- GRADIENTS


{-| -}
white : Color
white =
    Element.rgb255 255 255 255


{-| -}
black : Color
black =
    Element.rgb255 36 28 21


{-| -}
grayDark : Color
grayDark =
    Element.rgba255 36 28 21 0.65


{-| -}
transparent : Color
transparent =
    Element.rgba255 255 255 255 0
