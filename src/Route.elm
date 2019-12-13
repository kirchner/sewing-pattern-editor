module Route exposing
    ( Route(..)
    , fromUrl, toString
    , replaceUrl
    )

{-|

@docs Route
@docs fromUrl, toString
@docs replaceUrl

-}

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

import Browser.Navigation
import Git
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


{-| -}
type Route
    = Patterns
    | Measurements
    | Persons
    | GitHub Git.Repo Git.Ref (Maybe String)


{-| -}
toString : Route -> String
toString route =
    case route of
        Patterns ->
            "/patterns"

        Measurements ->
            "/measurements"

        Persons ->
            "/persons"

        GitHub repo ref _ ->
            String.join "/"
                [ "github"
                , repo.owner
                , repo.name
                , Git.refToString ref
                ]


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map GitHub (s "github" </> Git.repoParser </> Git.refParser <?> Query.string "code")
        ]


{-| -}
fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


{-| -}
replaceUrl : Browser.Navigation.Key -> Route -> Cmd msg
replaceUrl key route =
    Browser.Navigation.replaceUrl key (toString route)
