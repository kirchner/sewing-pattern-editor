module StoredPattern exposing
    ( Position
    , StoredPattern
    , decoder
    , encode
    , init
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

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Pattern exposing (Pattern)
import Point2d exposing (Point2d)


type alias StoredPattern =
    { slug : String
    , name : String
    , pattern : Pattern
    , zoom : Float
    , center : Point2d
    }


type alias Position =
    { x : Float
    , y : Float
    }


init : String -> String -> StoredPattern
init slug name =
    { slug = slug
    , name = name
    , pattern =
        Pattern.empty
            |> Pattern.insertPoint
                (Just "origin")
                (Pattern.origin { x = 0, y = 0 })
            |> Tuple.first
    , zoom = 1
    , center = Point2d.origin
    }


decoder : Decoder StoredPattern
decoder =
    Decode.succeed StoredPattern
        |> Decode.required "slug" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "pattern" Pattern.decoder
        |> Decode.required "zoom" Decode.float
        |> Decode.required "center" point2dDecoder


point2dDecoder : Decoder Point2d
point2dDecoder =
    Decode.succeed (\x y -> Point2d.fromCoordinates ( x, y ))
        |> Decode.required "x" Decode.float
        |> Decode.required "y" Decode.float


encode : StoredPattern -> Value
encode { slug, name, pattern, zoom, center } =
    Encode.object
        [ ( "slug", Encode.string slug )
        , ( "name", Encode.string name )
        , ( "pattern", Pattern.encode pattern )
        , ( "zoom", Encode.float zoom )
        , ( "center", encodePoint2d center )
        ]


encodePoint2d : Point2d -> Value
encodePoint2d point =
    Encode.object
        [ ( "x", Encode.float (Point2d.xCoordinate point) )
        , ( "y", Encode.float (Point2d.yCoordinate point) )
        ]
