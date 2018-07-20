module Expr
    exposing
        ( DoesNotCompute(..)
        , Expr
        , compute
        , parse
        )

import Dict exposing (Dict)
import Parser


type Expr
    = Number Float
    | Symbol String
    | Sum Expr Expr
    | Difference Expr Expr
    | Product Expr Expr
    | Quotient Expr Expr


parse : String -> Result Parser.DeadEnd Expr
parse =
    Debug.todo ""


compute : Dict String Expr -> Expr -> Result DoesNotCompute Float
compute =
    Debug.todo ""


type DoesNotCompute
    = MissingVariable String
    | RecursiveExpression (List String)
