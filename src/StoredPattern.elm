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
import Length exposing (Meters)
import Pattern exposing (Pattern)
import Point2d exposing (Point2d)


type alias StoredPattern coordinates =
    { slug : String
    , name : String
    , pattern : Pattern coordinates
    , zoom : Float
    , center : Point2d Meters coordinates
    }


type alias Position =
    { x : Float
    , y : Float
    }


init : String -> String -> StoredPattern coordinates
init slug name =
    { slug = slug
    , name = name
    , pattern =
        case Pattern.insertPoint "Origin" (Pattern.origin 0 0) Pattern.empty of
            Err _ ->
                Pattern.empty

            Ok pattern ->
                pattern
    , zoom = 1
    , center = Point2d.origin
    }


decoder : Decoder (StoredPattern coordinates)
decoder =
    Decode.succeed StoredPattern
        |> Decode.required "slug" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "pattern" Pattern.decoder
        |> Decode.required "zoom" Decode.float
        |> Decode.required "center" point2dDecoder


point2dDecoder : Decoder (Point2d Meters coordinates)
point2dDecoder =
    Decode.succeed Point2d.meters
        |> Decode.required "x" Decode.float
        |> Decode.required "y" Decode.float


encode : StoredPattern coordinates -> Value
encode { slug, name, pattern, zoom, center } =
    Encode.object
        [ ( "slug", Encode.string slug )
        , ( "name", Encode.string name )
        , ( "pattern", Pattern.encode pattern )
        , ( "zoom", Encode.float zoom )
        , ( "center", encodePoint2d center )
        ]


encodePoint2d : Point2d Meters coordinates -> Value
encodePoint2d point =
    Encode.object
        [ ( "x", Encode.float (Length.inMeters (Point2d.xCoordinate point)) )
        , ( "y", Encode.float (Length.inMeters (Point2d.yCoordinate point)) )
        ]
