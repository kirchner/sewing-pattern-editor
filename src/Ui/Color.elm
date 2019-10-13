module Ui.Color exposing
    ( primaryBright, primaryLight, primary, primaryDark
    , secondary, secondaryDark
    , complementary, complementaryDark
    , neutral, neutralDark
    , danger, dangerDark
    , success
    , grayDark
    , transparent
    , black, white
    )

{-|


# Color


## Main

@docs primaryBright, primaryLight, primary, primaryDark
@docs secondary, secondaryDark
@docs complementary, complementaryDark
@docs neutral, neutralDark
@docs danger, dangerDark
@docs success


## Gradients

@docs white black
@docs grayDark

@docs transparent

-}

import Element



---- MAIN COLORS


primaryBright =
    Element.rgb255 138 204 204


primaryLight =
    Element.rgb255 70 154 154


primary =
    Element.rgb255 38 132 132


primaryDark =
    Element.rgb255 15 108 108


secondary =
    Element.rgb255 239 238 234


secondaryDark =
    Element.rgb255 217 215 205


complementary =
    Element.rgb255 220 134 63


complementaryDark =
    Element.rgb255 180 95 25


neutral =
    Element.rgb255 220 166 63


neutralDark =
    Element.rgb255 180 127 25


danger =
    Element.rgb255 220 71 63


dangerDark =
    Element.rgb255 180 33 25


{-| <http://paletton.com/#uid=73i2p0kmQrEaoKRhxwdrum-xsg2>
-}
success =
    Element.rgb255 48 169 69



---- GRADIENTS


white =
    Element.rgb255 255 255 255


black =
    Element.rgb255 36 28 21


grayDark =
    Element.rgba255 36 28 21 0.65


transparent =
    Element.rgba255 255 255 255 0
