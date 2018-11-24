module Route exposing
    ( Point(..)
    , Route(..)
    , fromUrl
    , prefixFromUrl
    , toString
    )

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string, top)


type Route
    = Home
    | Editor String (Maybe Point)


type Point
    = NewLeftOf
    | NewRightOf


toString : String -> Route -> String
toString prefix route =
    prefix
        ++ (case route of
                Home ->
                    "/"

                Editor patternSlug maybePoint ->
                    case maybePoint of
                        Nothing ->
                            Builder.absolute
                                [ "editor"
                                , patternSlug
                                ]
                                []

                        Just point ->
                            Builder.absolute
                                [ "editor"
                                , patternSlug
                                , "points"
                                , case point of
                                    NewLeftOf ->
                                        "new-left-of"

                                    NewRightOf ->
                                        "new-right-of"
                                ]
                                []
           )


parser : Parser (Route -> a) a
parser =
    oneOf [ top, s "sewing-pattern-editor" ]
        </> oneOf
                [ Parser.map Home top
                , Parser.map Editor
                    (s "editor"
                        </> string
                        </> pointParser
                    )
                ]


pointParser : Parser (Maybe Point -> a) a
pointParser =
    oneOf
        [ Parser.map (Just NewLeftOf) (s "points" </> s "new-left-of")
        , Parser.map (Just NewRightOf) (s "points" </> s "new-right-of")
        , Parser.map Nothing top
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
