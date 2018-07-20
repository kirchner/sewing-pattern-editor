module Measurements
    exposing
        ( Angle
        , Length
        , Ratio
        )

import Expr exposing (Expr)


type Angle
    = Angle Expr


type Length
    = Length Expr


type Ratio
    = Ratio Expr
