module Design exposing
    ( xxxSmall, xxSmall, xSmall, small, normal, large, xLarge
    , fontSmall, fontNormal, fontLarge, fontXLarge, fontXXLarge
    , sansSerif, monospace
    , primary, primaryDark
    , secondary, secondaryDark
    , danger, dangerDark
    , grayDark
    , black, white
    )

{-|


# Spacing

@docs xxxSmall, xxSmall, xSmall, small, normal, large, xLarge


# Font

@docs fontSmall, fontNormal, fontLarge, fontXLarge, fontXXLarge

@docs sansSerif, monospace


# Color


## Main

@docs primary, primaryDark
@docs secondary, secondaryDark
@docs danger, dangerDark


## Gradients

@docs white black
@docs grayDark

-}

{-

   http://paletton.com/#uid=43i0s0kmQrEaoKRhxwdrum-xsg2

-}
{-
   Sewing pattern editor
   Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
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


fontXLarge =
    Font.size 21


fontXXLarge =
    Font.size 30


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
