module ExprTest exposing (suite)

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

import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, float, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "Expr.parse"
        [ fuzz int "an integer" <|
            \i ->
                i
                    |> String.fromInt
                    |> parse []
                    |> Expect.equal (Ok (Number (toFloat i)))
        , fuzz float "a float" <|
            \f ->
                f
                    |> String.fromFloat
                    |> parse []
                    |> Expect.equal (Ok (Number f))
        , test "a variable" <|
            \_ ->
                "SomeVar"
                    |> parse []
                    |> Expect.equal (Ok (Variable "SomeVar"))
        , describe "a function"
            [ fuzz spaces "with one arguments" <|
                \s ->
                    [ "func("
                    , s
                    , "arg"
                    , s
                    , ")"
                    ]
                        |> String.concat
                        |> parse [ "func" ]
                        |> Expect.equal (Ok (Function "func" [ "arg" ]))
            , fuzz spaces "with two arguments" <|
                \s ->
                    [ "func("
                    , s
                    , "arg1,"
                    , s
                    , "arg2"
                    , s
                    , ")"
                    ]
                        |> String.concat
                        |> parse [ "func" ]
                        |> Expect.equal (Ok (Function "func" [ "arg1", "arg2" ]))
            ]
        , fuzz3 int float spaces "a sum" <|
            \i f s ->
                [ String.fromInt i
                , s
                , "+"
                , s
                , String.fromFloat f
                ]
                    |> String.concat
                    |> parse []
                    |> Expect.equal
                        (Ok
                            (Sum
                                (Number (toFloat i))
                                (Number f)
                            )
                        )
        , fuzz3 int float spaces "a difference" <|
            \i f s ->
                [ String.fromInt i
                , s
                , "-"
                , s
                , String.fromFloat f
                ]
                    |> String.concat
                    |> parse []
                    |> Expect.equal
                        (Ok
                            (Difference
                                (Number (toFloat i))
                                (Number f)
                            )
                        )
        , fuzz3 int float spaces "a product" <|
            \i f s ->
                [ String.fromInt i
                , s
                , "*"
                , s
                , String.fromFloat f
                ]
                    |> String.concat
                    |> parse []
                    |> Expect.equal
                        (Ok
                            (Product
                                (Number (toFloat i))
                                (Number f)
                            )
                        )
        , fuzz3 int float spaces "a quotient" <|
            \i f s ->
                [ String.fromInt i
                , s
                , "/"
                , s
                , String.fromFloat f
                ]
                    |> String.concat
                    |> parse []
                    |> Expect.equal
                        (Ok
                            (Quotient
                                (Number (toFloat i))
                                (Number f)
                            )
                        )
        , describe "associativity"
            [ describe "* and /"
                [ test "without parentheses" <|
                    \_ ->
                        "1 * 2 / 3"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Quotient
                                        (Product (Number 1) (Number 2))
                                        (Number 3)
                                    )
                                )
                , test "with parentheses" <|
                    \_ ->
                        "1 * (2 / 3)"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Product
                                        (Number 1)
                                        (Quotient
                                            (Number 2)
                                            (Number 3)
                                        )
                                    )
                                )
                ]
            , describe "+ and -"
                [ test "without parentheses" <|
                    \_ ->
                        "1 + 2 - 3"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Difference
                                        (Sum (Number 1) (Number 2))
                                        (Number 3)
                                    )
                                )
                , test "with parentheses" <|
                    \_ ->
                        "1 + (2 - 3)"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Sum
                                        (Number 1)
                                        (Difference
                                            (Number 2)
                                            (Number 3)
                                        )
                                    )
                                )
                ]
            ]
        , describe "precedence"
            [ describe "without parentheses"
                [ test "first + then *" <|
                    \_ ->
                        "1 + 2 * 3"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Sum
                                        (Number 1)
                                        (Product
                                            (Number 2)
                                            (Number 3)
                                        )
                                    )
                                )
                , test "first * then +" <|
                    \_ ->
                        "1 * 2 + 3"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Sum
                                        (Product
                                            (Number 1)
                                            (Number 2)
                                        )
                                        (Number 3)
                                    )
                                )
                ]
            , describe "with parentheses"
                [ test "first + then *" <|
                    \_ ->
                        "(1 + 2) * 3"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Product
                                        (Sum
                                            (Number 1)
                                            (Number 2)
                                        )
                                        (Number 3)
                                    )
                                )
                , test "first * then +" <|
                    \_ ->
                        "1 * (2 + 3)"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (Product
                                        (Number 1)
                                        (Sum
                                            (Number 2)
                                            (Number 3)
                                        )
                                    )
                                )
                ]
            ]
        , fuzz3 int float spaces "a max" <|
            \i f s ->
                [ "max("
                , s
                , String.fromInt i
                , s
                , ","
                , s
                , String.fromFloat f
                , s
                , ")"
                ]
                    |> String.concat
                    |> parse []
                    |> Expect.equal (Ok (Max (Number (toFloat i)) (Number f)))
        , describe "a conditional" <|
            [ test "true" <|
                \_ ->
                    "if true then 1 else 2"
                        |> parse []
                        |> Expect.equal (Ok (IfThenElse ExprTrue (Number 1) (Number 2)))
            , test "false" <|
                \_ ->
                    "if false then 1 else 2"
                        |> parse []
                        |> Expect.equal (Ok (IfThenElse ExprFalse (Number 1) (Number 2)))
            , test "not" <|
                \_ ->
                    "if not true then 1 else 2"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse (Not ExprTrue)
                                    (Number 1)
                                    (Number 2)
                                )
                            )
            , test "and" <|
                \_ ->
                    "if true && false then 1 else 2"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse (And ExprTrue ExprFalse)
                                    (Number 1)
                                    (Number 2)
                                )
                            )
            , test "or" <|
                \_ ->
                    "if false || true then 1 else 2"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse (Or ExprFalse ExprTrue)
                                    (Number 1)
                                    (Number 2)
                                )
                            )
            , describe "precedence"
                [ test "without parentheses" <|
                    \_ ->
                        "if true && false || true then 1 else 2"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (IfThenElse
                                        (Or
                                            (And ExprTrue ExprFalse)
                                            ExprTrue
                                        )
                                        (Number 1)
                                        (Number 2)
                                    )
                                )
                , test "with parentheses" <|
                    \_ ->
                        "if true && (false || true) then 1 else 2"
                            |> parse []
                            |> Expect.equal
                                (Ok
                                    (IfThenElse
                                        (And
                                            ExprTrue
                                            (Or ExprFalse ExprTrue)
                                        )
                                        (Number 1)
                                        (Number 2)
                                    )
                                )
                ]
            , test "equal" <|
                \_ ->
                    "if 1 == 2 then 2 else 3"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse
                                    (Equal (Number 1) (Number 2))
                                    (Number 2)
                                    (Number 3)
                                )
                            )
            , test "not equal" <|
                \_ ->
                    "if 1 /= 2 then 2 else 3"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse
                                    (Not (Equal (Number 1) (Number 2)))
                                    (Number 2)
                                    (Number 3)
                                )
                            )
            , test "greater than" <|
                \_ ->
                    "if 1 > 2 then 2 else 3"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse
                                    (GreaterThan (Number 1) (Number 2))
                                    (Number 2)
                                    (Number 3)
                                )
                            )
            , test "strictly greater than" <|
                \_ ->
                    "if 1 >= 2 then 2 else 3"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse
                                    (StrictlyGreaterThan (Number 1) (Number 2))
                                    (Number 2)
                                    (Number 3)
                                )
                            )
            , test "smaller than" <|
                \_ ->
                    "if 1 < 2 then 2 else 3"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse
                                    (Not (GreaterThan (Number 1) (Number 2)))
                                    (Number 2)
                                    (Number 3)
                                )
                            )
            , test "strictly smaller than" <|
                \_ ->
                    "if 1 <= 2 then 2 else 3"
                        |> parse []
                        |> Expect.equal
                            (Ok
                                (IfThenElse
                                    (Not (StrictlyGreaterThan (Number 1) (Number 2)))
                                    (Number 2)
                                    (Number 3)
                                )
                            )
            ]
        ]



---- FUZZER


spaces : Fuzzer String
spaces =
    Fuzz.oneOf
        [ Fuzz.constant ""
        , Fuzz.constant " "
        , Fuzz.constant "\n"
        ]
