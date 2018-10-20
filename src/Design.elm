module Design exposing
    ( Grey(..)
    , backgroundColor
    , fontColor
    , large
    , normal
    , small
    , toColor
    , white
    , xLarge
    , xSmall
    , xxSmall
    )

import Color
import Element exposing (Attr)
import Element.Background as Background
import Element.Font as Font



---- SPACING


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



--- COLOR


type Grey
    = Brightest
    | Bright
    | Brightish
    | Darkish
    | Dark
    | Darkest


toColor grey =
    Element.fromRgb <|
        Color.toRgba <|
            case grey of
                Brightest ->
                    brightest

                Bright ->
                    bright

                Brightish ->
                    brightish

                Darkish ->
                    darkish

                Dark ->
                    dark

                Darkest ->
                    darkest


backgroundColor : Grey -> Attr decorative msg
backgroundColor grey =
    Background.color <|
        color <|
            case grey of
                Brightest ->
                    brightest

                Bright ->
                    bright

                Brightish ->
                    brightish

                Darkish ->
                    darkish

                Dark ->
                    dark

                Darkest ->
                    darkest


fontColor : Grey -> Attr decorative msg
fontColor grey =
    Font.color <|
        color <|
            case grey of
                Brightest ->
                    black

                Bright ->
                    black

                Brightish ->
                    black

                Darkish ->
                    white

                Dark ->
                    white

                Darkest ->
                    white


color =
    Element.fromRgb << Color.toRgba



-- BLACK AND WHITE


white =
    Color.rgb255 229 223 197


black =
    Color.rgb255 0 0 0



-- GREY


brightest =
    Color.rgb255 240 240 240


bright =
    Color.rgb255 210 210 210


brightish =
    Color.rgb255 180 180 180


darkish =
    Color.rgb255 97 97 97


dark =
    Color.rgb255 66 66 66


darkest =
    Color.rgb255 33 33 33
