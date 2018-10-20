module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Pattern String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Pattern (s "pattern" </> string)
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser
