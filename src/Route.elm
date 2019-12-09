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
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


{-| -}
type Route
    = Patterns
    | Measurements
    | Persons
    | Editor String String (Maybe String) (Maybe String)


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

        Editor owner repo maybeRef _ ->
            case maybeRef of
                Nothing ->
                    String.join "/" [ owner, repo ]

                Just ref ->
                    String.join "/" [ owner, repo, "tree", ref ]


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Patterns top
        , map Patterns (s "patterns")
        , map Measurements (s "measurements")
        , map Persons (s "persons")
        , map (\owner repo maybeCode -> Editor owner repo Nothing maybeCode)
            (string </> string <?> Query.string "code")
        , map (\owner repo ref maybeCode -> Editor owner repo (Just ref) maybeCode)
            (string </> string </> s "tree" </> string <?> Query.string "code")
        ]


{-| -}
fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


{-| -}
replaceUrl : Browser.Navigation.Key -> Route -> Cmd msg
replaceUrl key route =
    Browser.Navigation.replaceUrl key (toString route)
