module Result.Extra exposing (resolve)


resolve : Result a a -> a
resolve result =
    case result of
        Err a ->
            a

        Ok a ->
            a
