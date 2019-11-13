module Expr exposing
    ( Expr(..), BoolExpr(..)
    , parse
    )

{-|

@docs Expr, BoolExpr
@docs parse

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

import Char
import Dict exposing (Dict)
import Parser exposing (..)
import Set


{-| -}
type Expr
    = Number Float
    | Variable String
    | Function String (List String)
    | Sum Expr Expr
    | Difference Expr Expr
    | Product Expr Expr
    | Quotient Expr Expr
    | Max Expr Expr
    | IfThenElse BoolExpr Expr Expr


{-| -}
type BoolExpr
    = ExprTrue
    | ExprFalse
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    | Equal Expr Expr
    | GreaterThan Expr Expr
    | StrictlyGreaterThan Expr Expr


{-| -}
parse : List String -> String -> Result (List DeadEnd) Expr
parse reservedWords string =
    Parser.run (expr (keyWords ++ reservedWords)) string


keyWords =
    [ "max"
    , "if"
    , "then"
    , "else"
    ]



---- PARSER


digits : Parser Expr
digits =
    Parser.map Number digitsHelp


digitsHelp : Parser Float
digitsHelp =
    number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }


var : List String -> Parser Expr
var reservedWords =
    map Variable <|
        variable
            { start = isVarChar
            , inner = isVarChar
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
                { start = isVarChar
                , inner = isVarChar
                , reserved = Set.fromList reservedWords
                }
                |> andThen
                    (\firstArg ->
                        argsHelp reservedWords [ firstArg ]
                    )
           )
        |. symbol ")"


max : List String -> Parser Expr
max reservedWords =
    succeed Max
        |. keyword "max"
        |. symbol "("
        |. spaces
        |= lazy (\_ -> expr reservedWords)
        |. spaces
        |. symbol ","
        |. spaces
        |= lazy (\_ -> expr reservedWords)
        |. spaces
        |. symbol ")"


ifThenElse : List String -> Parser Expr
ifThenElse reservedWords =
    succeed IfThenElse
        |. keyword "if"
        |. space
        |= lazy (\_ -> boolExpr reservedWords)
        |. space
        |. keyword "then"
        |. space
        |= lazy (\_ -> expr reservedWords)
        |. space
        |. keyword "else"
        |. space
        |= lazy (\_ -> expr reservedWords)


argsHelp : List String -> List String -> Parser (List String)
argsHelp reservedWords revArgs =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed identity
                |. symbol ","
                |. spaces
                |= variable
                    { start = isVarChar
                    , inner = isVarChar
                    , reserved = Set.fromList reservedWords
                    }
                |> andThen (\nextArg -> argsHelp reservedWords (nextArg :: revArgs))
            , succeed (List.reverse revArgs)
            ]


term : List String -> Parser Expr
term reservedWords =
    oneOf
        [ succeed (\num -> Number (-1 * num))
            |. symbol "-"
            |. spaces
            |= digitsHelp
        , digits
        , max reservedWords
        , ifThenElse reservedWords
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
        |> andThen
            (\t ->
                Parser.succeed identity
                    |= exprHelp reservedWords [] t
            )


exprHelp : List String -> List ( Expr, Operator ) -> Expr -> Parser Expr
exprHelp reservedWords revOps prevExpr =
    oneOf
        [ succeed Tuple.pair
            |= backtrackable
                (succeed identity
                    |. spaces
                    |= operator
                )
            |. spaces
            |= term reservedWords
            |> andThen
                (\( op, nextExpr ) ->
                    Parser.succeed identity
                        |= exprHelp reservedWords (( prevExpr, op ) :: revOps) nextExpr
                )
        , lazy (\_ -> succeed (finalize revOps prevExpr))
        ]


space : Parser ()
space =
    Parser.succeed ()
        |. Parser.oneOf
            [ symbol " "
            , symbol "\n"
            ]
        |. spaces


isVarChar : Char -> Bool
isVarChar char =
    [ ' '
    , '\t'
    , '\n'
    , ','
    , '+'
    , '-'
    , '*'
    , '/'
    , '('
    , ')'
    , '&'
    , '|'
    , '='
    , '>'
    , '<'
    ]
        |> List.member char
        |> not


boolExpr : List String -> Parser BoolExpr
boolExpr reservedWords =
    boolTerm reservedWords
        |> andThen
            (\t ->
                Parser.succeed identity
                    |= boolExprHelp reservedWords [] t
            )


boolExprHelp : List String -> List ( BoolExpr, BoolOperator ) -> BoolExpr -> Parser BoolExpr
boolExprHelp reservedWords revOps prevExpr =
    oneOf
        [ succeed Tuple.pair
            |. backtrackable space
            |= boolOperator
            |. space
            |= boolTerm reservedWords
            |> andThen
                (\( op, nextExpr ) ->
                    Parser.succeed identity
                        |= boolExprHelp reservedWords
                            (( prevExpr, op ) :: revOps)
                            nextExpr
                )
        , lazy (\_ -> succeed (finalizeBool revOps prevExpr))
        ]


boolTerm : List String -> Parser BoolExpr
boolTerm reservedWords =
    Parser.oneOf
        [ Parser.map (always ExprTrue) (keyword "true")
        , Parser.map (always ExprFalse) (keyword "false")
        , succeed Not
            |. keyword "not"
            |. space
            |= lazy (\_ -> boolExpr reservedWords)
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> boolExpr reservedWords)
            |. spaces
            |. symbol ")"
        , succeed identity
            |= lazy (\_ -> expr reservedWords)
            |. spaces
            |> andThen
                (\firstExpr ->
                    oneOf
                        [ succeed (Equal firstExpr)
                            |. symbol "=="
                            |. spaces
                            |= lazy (\_ -> expr reservedWords)
                        , succeed (Not << Equal firstExpr)
                            |. symbol "/="
                            |. spaces
                            |= lazy (\_ -> expr reservedWords)
                        , succeed identity
                            |. symbol ">"
                            |= oneOf
                                [ succeed (StrictlyGreaterThan firstExpr)
                                    |. symbol "="
                                    |. spaces
                                    |= lazy (\_ -> expr reservedWords)
                                , succeed (GreaterThan firstExpr)
                                    |. spaces
                                    |= lazy (\_ -> expr reservedWords)
                                ]
                        , succeed identity
                            |. symbol "<"
                            |= oneOf
                                [ succeed (Not << StrictlyGreaterThan firstExpr)
                                    |. symbol "="
                                    |. spaces
                                    |= lazy (\_ -> expr reservedWords)
                                , succeed (Not << GreaterThan firstExpr)
                                    |. spaces
                                    |= lazy (\_ -> expr reservedWords)
                                ]
                        ]
                )
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

        ( nextExpr, DivOp ) :: otherRevOps ->
            collect otherRevOps [ ( nextExpr, DivOp ) ] finalExpr

        ( nextExpr, MulOp ) :: otherRevOps ->
            collect otherRevOps [ ( nextExpr, MulOp ) ] finalExpr

        ( nextExpr, AddOp ) :: otherRevOps ->
            Sum (finalize otherRevOps nextExpr) finalExpr

        ( nextExpr, DifOp ) :: otherRevOps ->
            Difference (finalize otherRevOps nextExpr) finalExpr


collect : List ( Expr, Operator ) -> List ( Expr, Operator ) -> Expr -> Expr
collect revOps collectedOps finalExpr =
    case revOps of
        [] ->
            finalizeProducts (List.reverse collectedOps) finalExpr

        ( nextExpr, DivOp ) :: otherRevOps ->
            collect otherRevOps (( nextExpr, DivOp ) :: collectedOps) finalExpr

        ( nextExpr, MulOp ) :: otherRevOps ->
            collect otherRevOps (( nextExpr, MulOp ) :: collectedOps) finalExpr

        ( nextExpr, AddOp ) :: otherRevOps ->
            Sum (finalize otherRevOps nextExpr) <|
                finalizeProducts (List.reverse collectedOps) finalExpr

        ( nextExpr, DifOp ) :: otherRevOps ->
            Difference (finalize otherRevOps nextExpr) <|
                finalizeProducts (List.reverse collectedOps) finalExpr


finalizeProducts : List ( Expr, Operator ) -> Expr -> Expr
finalizeProducts revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( nextExpr, DivOp ) :: otherRevOps ->
            Quotient (finalizeProducts otherRevOps nextExpr) finalExpr

        ( nextExpr, MulOp ) :: otherRevOps ->
            Product (finalizeProducts otherRevOps nextExpr) finalExpr

        ( nextExpr, AddOp ) :: otherRevOps ->
            -- this should never happen
            Sum (finalize otherRevOps nextExpr) finalExpr

        ( nextExpr, DifOp ) :: otherRevOps ->
            -- this should never happen
            Difference (finalize otherRevOps nextExpr) finalExpr


type BoolOperator
    = AndOp
    | OrOp


boolOperator : Parser BoolOperator
boolOperator =
    oneOf
        [ map (\_ -> AndOp) (symbol "&&")
        , map (\_ -> OrOp) (symbol "||")
        ]


finalizeBool : List ( BoolExpr, BoolOperator ) -> BoolExpr -> BoolExpr
finalizeBool revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( nextExpr, AndOp ) :: otherRevOps ->
            finalizeBool otherRevOps (And nextExpr finalExpr)

        ( nextExpr, OrOp ) :: otherRevOps ->
            Or (finalizeBool otherRevOps nextExpr) finalExpr
