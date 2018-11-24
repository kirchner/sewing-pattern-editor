module Design exposing
    ( Grey(..)
    , backgroundColor
    , borderColor
    , fontColor
    , large
    , normal
    , small
    , toColor
    , white
    , xLarge
    , xSmall
    , xxSmall
    , xxxSmall
    )

import Color
import Element exposing (Attr)
import Element.Background as Background
import Element.Border as Border
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


borderColor : Grey -> Attr decorative msg
borderColor grey =
    Border.color <|
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
    Color.rgb255 66 66 66


dark =
    Color.rgb255 33 33 33


darkest =
    Color.rgb255 5 5 5
