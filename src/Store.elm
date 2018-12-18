module Store exposing
    ( Entry
    , Store
    , decoder
    , empty
    , encode
    , get
    , getByName
    , insert
    , member
    , remove
    , toList
    , updateValue
    , values
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

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type Store a
    = Store (StoreData a)


type alias StoreData a =
    { entries : Dict Int (Entry a)
    , nextId : Int
    }


type alias Entry a =
    { name : Maybe String
    , value : a
    }


empty : Store a
empty =
    Store
        { entries = Dict.empty
        , nextId = 0
        }


insert : Maybe String -> a -> Store a -> ( Store a, Int )
insert name value (Store store) =
    let
        id =
            store.nextId + 1
    in
    ( Store
        { store
            | entries = Dict.insert store.nextId (Entry name value) store.entries
            , nextId = id
        }
    , store.nextId
    )


remove : Int -> Store a -> Store a
remove id (Store store) =
    Store { store | entries = Dict.remove id store.entries }


get : Store a -> Int -> Maybe (Entry a)
get (Store { entries }) id =
    Dict.get id entries


getByName : Store a -> String -> Maybe ( Int, Entry a )
getByName (Store { entries }) name =
    entries
        |> Dict.toList
        |> List.filter (\( _, entry ) -> entry.name == Just name)
        |> List.head


updateValue : Int -> a -> Store a -> Store a
updateValue id newValue (Store data) =
    Store
        { data
            | entries =
                Dict.update id
                    (Maybe.map (\entry -> { entry | value = newValue }))
                    data.entries
        }


member : Store a -> Int -> Bool
member (Store { entries }) id =
    Dict.member id entries


toList : Store a -> List ( Int, Entry a )
toList (Store { entries }) =
    entries
        |> Dict.toList


values : Store a -> List (Entry a)
values (Store { entries }) =
    entries
        |> Dict.values


encode : (a -> Value) -> Store a -> Value
encode encodeA (Store { entries, nextId }) =
    let
        encodeEntry ( id, { name, value } ) =
            Encode.object
                [ ( "id", Encode.int id )
                , ( "name"
                  , case name of
                        Nothing ->
                            Encode.null

                        Just actualName ->
                            Encode.string actualName
                  )
                , ( "value", encodeA value )
                ]
    in
    Encode.object
        [ ( "entries"
          , entries
                |> Dict.toList
                |> Encode.list encodeEntry
          )
        , ( "nextId", Encode.int nextId )
        ]


decoder : Decoder a -> Decoder (Store a)
decoder aDecoder =
    let
        entryDecoder =
            Decode.map2 Tuple.pair
                (Decode.field "id" Decode.int)
                (Decode.succeed Entry
                    |> Decode.required "name" (Decode.nullable Decode.string)
                    |> Decode.required "value" aDecoder
                )
    in
    Decode.map Store
        (Decode.succeed StoreData
            |> Decode.required "entries" (Decode.map Dict.fromList (Decode.list entryDecoder))
            |> Decode.required "nextId" Decode.int
        )
