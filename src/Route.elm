module Route exposing
    ( Route(..), NewParameters, fromUrl
    , WithCode, fromUrlWithCode
    , absolute, relative, crossOrigin
    , replaceUrl, pushUrl
    )

{-|

@docs Route, NewParameters, fromUrl
@docs WithCode, fromUrlWithCode
@docs absolute, relative, crossOrigin
@docs replaceUrl, pushUrl

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
import LocalStorage
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


{-| -}
type Route
    = Patterns
    | Pattern LocalStorage.Address
    | PatternNew NewParameters
    | Details LocalStorage.Address


{-| -}
type alias NewParameters =
    { name : Maybe String
    , description : Maybe String
    , storageSolution : Maybe String
    , slug : Maybe String
    , repositoryName : Maybe String
    , visibility : Maybe String
    }


{-| -}
type alias WithCode =
    { route : Route
    , code : Maybe String
    }


{-| -}
fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routeParser


{-| -}
fromUrlWithCode : Url -> Maybe WithCode
fromUrlWithCode =
    Parser.parse parser


{-| -}
replaceUrl : Browser.Navigation.Key -> Route -> Cmd msg
replaceUrl key route =
    Browser.Navigation.replaceUrl key (absolute route [])


{-| -}
pushUrl : Browser.Navigation.Key -> Route -> Cmd msg
pushUrl key route =
    Browser.Navigation.pushUrl key (absolute route [])


{-| -}
absolute : Route -> List QueryParameter -> String
absolute route otherParameters =
    Url.Builder.absolute (pathSegments route)
        (parameters route ++ otherParameters)


{-| -}
relative : Route -> List QueryParameter -> String
relative route otherParameters =
    Url.Builder.relative (pathSegments route)
        (parameters route ++ otherParameters)


{-| -}
crossOrigin : String -> Route -> List QueryParameter -> String
crossOrigin domain route otherParameters =
    Url.Builder.crossOrigin domain
        (pathSegments route)
        (parameters route ++ otherParameters)


pathSegments : Route -> List String
pathSegments route =
    case route of
        Patterns ->
            []

        Pattern address ->
            LocalStorage.addressToPathSegments address

        PatternNew _ ->
            [ "new" ]

        Details address ->
            LocalStorage.addressToPathSegments address ++ [ "details" ]


parameters : Route -> List QueryParameter
parameters route =
    case route of
        Patterns ->
            []

        Pattern _ ->
            []

        PatternNew params ->
            List.filterMap identity
                [ Maybe.map (Url.Builder.string "name") params.name
                , Maybe.map (Url.Builder.string "description") params.description
                , Maybe.map (Url.Builder.string "storageSolution") params.storageSolution
                , Maybe.map (Url.Builder.string "slug") params.slug
                , Maybe.map (Url.Builder.string "repositoryName") params.repositoryName
                , Maybe.map (Url.Builder.string "visibility") params.visibility
                ]

        Details _ ->
            []



---- PARSER


parser : Parser (WithCode -> a) a
parser =
    map WithCode (routeParser <?> Query.string "code")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Patterns top
        , map Patterns (top </> s "app.html")
        , map Pattern (top </> LocalStorage.addressParser)
        , map PatternNew (top </> s "new" </> newParameters)
        , map Details (top </> LocalStorage.addressParser </> s "details")
        ]


newParameters : Parser (NewParameters -> a) a
newParameters =
    map NewParameters
        (top
            <?> Query.string "name"
            <?> Query.string "description"
            <?> Query.string "storageSolution"
            <?> Query.string "slug"
            <?> Query.string "repositoryName"
            <?> Query.string "visibility"
        )
