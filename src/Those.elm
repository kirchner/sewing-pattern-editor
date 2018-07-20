module Those
    exposing
        ( That
        , Those
        , empty
        , insert
        , rename
        )

import Dict exposing (Dict)


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
