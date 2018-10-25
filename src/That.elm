module That exposing
    ( That
    , areEqual
    , decoder
    , encode
    , hash
    , objectId
    , that
    , toComparable
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

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type That object
    = That Int


hash : That object -> String
hash (That id) =
    String.concat
        [ "object-id-"
        , String.fromInt id
        ]


that : Int -> That object
that id =
    That id


objectId : That object -> Int
objectId (That id) =
    id


toComparable : That object -> Int
toComparable (That id) =
    id


areEqual : That a -> That a -> Bool
areEqual thatA thatB =
    toComparable thatA == toComparable thatB


encode : That a -> Value
encode (That id) =
    Encode.object
        [ ( "id", Encode.int id )
        ]


decoder : Decoder (That a)
decoder =
    Decode.succeed That
        |> Decode.required "id" Decode.int
