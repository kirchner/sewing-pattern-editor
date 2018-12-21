module Triple exposing (mapThird)


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird func ( a, b, c ) =
    ( a, b, func c )
