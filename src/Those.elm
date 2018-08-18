module Those
    exposing
        ( Those
        , decoder
        , encode
        , fromList
        , insert
        , member
        , none
        , remove
        , toList
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import That exposing (That)


type Those a
    = Those (List (That a))


none : Those a
none =
    Those []


fromList : List (That a) -> Those a
fromList thats =
    thats
        |> List.uniqueBy That.toComparable
        |> Those


toList : Those a -> List (That a)
toList (Those thats) =
    thats


insert : That a -> Those a -> Those a
insert that (Those those) =
    that
        :: those
        |> List.uniqueBy That.toComparable
        |> Those


remove : That a -> Those a -> Those a
remove that (Those those) =
    those
        |> List.filter (not << That.areEqual that)
        |> Those


member : That a -> Those a -> Bool
member that (Those those) =
    those
        |> List.any (That.areEqual that)


encode : Those a -> Value
encode (Those thats) =
    Encode.list That.encode thats


decoder : Decoder (Those a)
decoder =
    Decode.map Those (Decode.list That.decoder)
