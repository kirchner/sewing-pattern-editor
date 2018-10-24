module Expr exposing
    ( Expr(..)
    , evaluate
    , parse
    )

import Char
import Dict exposing (Dict)
import Parser exposing (..)
import Set


type Expr
    = Number Float
    | Variable String
    | Function String (List String)
    | Sum Expr Expr
    | Difference Expr Expr
    | Product Expr Expr
    | Quotient Expr Expr


parse : List String -> String -> Result (List DeadEnd) Expr
parse reservedWords string =
    Parser.run (expr reservedWords) string


evaluate :
    (String -> List String -> Maybe Float)
    -> Dict String Expr
    -> Expr
    -> Maybe Float
evaluate functions variables e =
    case e of
        Number float ->
            Just float

        Variable name ->
            Dict.get name variables
                |> Maybe.andThen (evaluate functions variables)

        Function name args ->
            functions name args

        Sum exprA exprB ->
            Maybe.map2 (\a b -> a + b)
                (evaluate functions variables exprA)
                (evaluate functions variables exprB)

        Difference exprA exprB ->
            Maybe.map2 (\a b -> a - b)
                (evaluate functions variables exprA)
                (evaluate functions variables exprB)

        Product exprA exprB ->
            Maybe.map2 (\a b -> a * b)
                (evaluate functions variables exprA)
                (evaluate functions variables exprB)

        Quotient exprA exprB ->
            Maybe.map2 (\a b -> a / b)
                (evaluate functions variables exprA)
                (evaluate functions variables exprB)



---- PARSER


digits : Parser Expr
digits =
    number
        { int = Just (Number << toFloat)
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just Number
        }


var : List String -> Parser Expr
var reservedWords =
    map Variable <|
        variable
            { start = Char.isAlpha
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList reservedWords
            }


function : List String -> Parser Expr
function reservedWords =
    succeed Function
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.empty
            }
        |. symbol "("
        |. spaces
        |= (variable
                { start = Char.isAlpha
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.fromList reservedWords
                }
                |> andThen
                    (\firstArg ->
                        argsHelp reservedWords [ firstArg ]
                    )
           )
        |. symbol ")"


argsHelp : List String -> List String -> Parser (List String)
argsHelp reservedWords revArgs =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed identity
                |. symbol ","
                |. spaces
                |= variable
                    { start = Char.isAlpha
                    , inner = \c -> Char.isAlphaNum c || c == '_'
                    , reserved = Set.fromList reservedWords
                    }
                |> andThen (\nextArg -> argsHelp reservedWords (nextArg :: revArgs))
            , succeed (List.reverse revArgs)
            ]


term : List String -> Parser Expr
term reservedWords =
    oneOf
        [ digits
        , backtrackable (function reservedWords)
        , var reservedWords
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expr reservedWords)
            |. spaces
            |. symbol ")"
        ]


expr : List String -> Parser Expr
expr reservedWords =
    term reservedWords
        |> andThen (exprHelp reservedWords [])


exprHelp : List String -> List ( Expr, Operator ) -> Expr -> Parser Expr
exprHelp reservedWords revOps prevExpr =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed Tuple.pair
                |= operator
                |. spaces
                |= term reservedWords
                |> andThen
                    (\( op, nextExpr ) ->
                        exprHelp reservedWords (( prevExpr, op ) :: revOps) nextExpr
                    )
            , lazy (\_ -> succeed (finalize revOps prevExpr))
            ]


type Operator
    = AddOp
    | DifOp
    | MulOp
    | DivOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AddOp) (symbol "+")
        , map (\_ -> DifOp) (symbol "-")
        , map (\_ -> MulOp) (symbol "*")
        , map (\_ -> DivOp) (symbol "/")
        ]


finalize : List ( Expr, Operator ) -> Expr -> Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( nextExpr, MulOp ) :: otherRevOps ->
            finalize otherRevOps (Product nextExpr finalExpr)

        ( nextExpr, DivOp ) :: otherRevOps ->
            finalize otherRevOps (Quotient nextExpr finalExpr)

        ( nextExpr, AddOp ) :: otherRevOps ->
            Sum (finalize otherRevOps nextExpr) finalExpr

        ( nextExpr, DifOp ) :: otherRevOps ->
            Difference (finalize otherRevOps nextExpr) finalExpr
