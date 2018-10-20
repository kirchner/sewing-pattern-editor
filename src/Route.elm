module Route exposing (Route(..), fromUrl, prefixFromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Pattern String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ oneOf
            [ Parser.map Home Parser.top
            , Parser.map Pattern (s "pattern" </> string)
            ]
        , oneOf
            [ Parser.map Home (s "sewing-pattern-editor")
            , Parser.map Pattern (s "sewing-pattern-editor" </> s "pattern" </> string)
            ]
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


prefixFromUrl : Url -> String
prefixFromUrl url =
    case String.split "/" url.path of
        _ :: first :: _ ->
            if first == "sewing-pattern-editor" then
                "/sewing-pattern-editor"

            else
                ""

        _ ->
            ""
