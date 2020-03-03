module Result.Extra exposing
    ( resolve
    , combine
    )

{-|

@docs resolve
@docs combine

-}


{-| -}
resolve : Result a a -> a
resolve result =
    case result of
        Err a ->
            a

        Ok a ->
            a


{-| -}
combine : List (Result err a) -> Result err (List a)
combine =
    List.foldl
        (\result resultList ->
            case resultList of
                Err _ ->
                    resultList

                Ok list ->
                    case result of
                        Err err ->
                            Err err

                        Ok a ->
                            Ok (a :: list)
        )
        (Ok [])
        >> Result.map List.reverse
