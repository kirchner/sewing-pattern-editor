module StateResult exposing
    ( StateResult
    , andThen
    , combine
    , embed
    , err
    , join
    , map
    , map2
    , map3
    , map4
    , ok
    , traverse
    , with
    , withDefault
    )

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

import Result.Extra as Result
import State exposing (State)


type alias StateResult s err a =
    State s (Result err a)


ok : a -> StateResult s err a
ok a =
    State.state (Ok a)


err : err -> StateResult s err a
err error =
    State.state (Err error)


traverse : (a -> StateResult s err b) -> List a -> StateResult s err (List b)
traverse func listA =
    State.traverse func listA
        |> State.map sort


combine : List (StateResult s err a) -> StateResult s err (List a)
combine =
    State.combine
        >> State.map Result.combine


sort : List (Result err b) -> Result err (List b)
sort results =
    let
        sortHelp result ( bs, errors ) =
            case result of
                Ok b ->
                    ( b :: bs, errors )

                Err error ->
                    ( bs, error :: errors )
    in
    case List.foldr sortHelp ( [], [] ) results of
        ( bs, [] ) ->
            Ok bs

        ( _, error :: _ ) ->
            Err error


withDefault : a -> StateResult s err a -> State s a
withDefault a stateResultA =
    State.map
        (Result.withDefault a)
        stateResultA


andThen : (a -> StateResult s err b) -> StateResult s err a -> StateResult s err b
andThen func =
    State.andThen <|
        \resultA ->
            case resultA of
                Err error ->
                    err error

                Ok a ->
                    func a


join : StateResult s err (Result err a) -> StateResult s err a
join =
    andThen <|
        \result ->
            case result of
                Err error ->
                    err error

                Ok circle2d_ ->
                    ok circle2d_


map : (a -> b) -> StateResult s err a -> StateResult s err b
map func =
    State.map (Result.map func)


map2 : (a -> b -> c) -> StateResult s err a -> StateResult s err b -> StateResult s err c
map2 func =
    State.map2 (Result.map2 func)


map3 :
    (a -> b -> c -> d)
    -> StateResult s err a
    -> StateResult s err b
    -> StateResult s err c
    -> StateResult s err d
map3 func =
    State.map3 (Result.map3 func)


map4 :
    (a -> b -> c -> d -> e)
    -> StateResult s err a
    -> StateResult s err b
    -> StateResult s err c
    -> StateResult s err d
    -> StateResult s err e
map4 func a b c d =
    State.map (Result.map4 func) a
        |> State.andMap b
        |> State.andMap c
        |> State.andMap d


with : StateResult s err a -> StateResult s err (a -> b) -> StateResult s err b
with =
    \b a -> map2 (<|) a b


embed : (s -> a) -> StateResult s err a
embed f =
    State.embed (Ok << f)
