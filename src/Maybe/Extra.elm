module Maybe.Extra exposing (andThen2, andThen3)

{-|

@docs andThen2, andThen3

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


{-| -}
andThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
andThen2 func maybeA maybeB =
    Maybe.withDefault Nothing <|
        Maybe.map2 func
            maybeA
            maybeB


{-| -}
andThen3 : (a -> b -> c -> Maybe d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
andThen3 func maybeA maybeB maybeC =
    Maybe.withDefault Nothing <|
        Maybe.map3 func
            maybeA
            maybeB
            maybeC
