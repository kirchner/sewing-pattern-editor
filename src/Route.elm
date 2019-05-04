module Route exposing
    ( Point(..)
    , Route(..)
    , fromUrl
    , prefixFromUrl
    , toString
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

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string, top)


type Route
    = Patterns
    | Editor String (Maybe Point)


type Point
    = NewLeftOf
    | NewRightOf


toString : String -> Route -> String
toString prefix route =
    prefix
        ++ (case route of
                Patterns ->
                    "/patterns"

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
                [ Parser.map Patterns top
                , Parser.map Patterns (s "patterns")
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
