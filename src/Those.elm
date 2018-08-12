module Those
    exposing
        ( Element
        , That
        , These
        , Those
        , empty
        , get
        , insert
        , insertIntoThese
        , memberOfThese
        , none
        , removeFromThese
        , rename
        , theseFromList
        , toList
        , values
        )

import Dict exposing (Dict)
import Set exposing (Set)


type Those a
    = Those Int (Dict Int (Element a))


type alias Element a =
    { name : Maybe String
    , a : a
    }


type That a
    = That Int


empty : Those a
empty =
    Those 0 Dict.empty


insert : Maybe String -> a -> Those a -> Those a
insert name a (Those nextId dict) =
    let
        element =
            { name = name
            , a = a
            }
    in
    Those (nextId + 1) (Dict.insert nextId element dict)


rename : a -> String -> Those a -> Those a
rename =
    Debug.todo ""


get : That a -> Those a -> Maybe (Element a)
get (That id) (Those _ dict) =
    Dict.get id dict


values : Those a -> List (Element a)
values (Those _ dict) =
    Dict.values dict


toList : Those a -> List ( That a, Element a )
toList (Those _ dict) =
    Dict.toList dict
        |> List.map (Tuple.mapFirst That)



-- SET


type These a
    = These (Set Int)


none : These a
none =
    These Set.empty


theseFromList : List (That a) -> These a
theseFromList =
    let
        getId (That id) =
            id
    in
    List.map getId
        >> Set.fromList
        >> These


insertIntoThese : That a -> These a -> These a
insertIntoThese (That id) (These ids) =
    These (Set.insert id ids)


removeFromThese : That a -> These a -> These a
removeFromThese (That id) (These ids) =
    These (Set.remove id ids)


memberOfThese : That a -> These a -> Bool
memberOfThese (That id) (These ids) =
    Set.member id ids
