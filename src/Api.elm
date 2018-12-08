module Api exposing
    ( createPattern
    , getPattern
    , getPatterns
    , updatePattern
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

import Http
import Json.Decode as Decode
import StoredPattern


getPatterns onReceived =
    Http.get
        { url = "/patterns"
        , expect = Http.expectJson onReceived (Decode.list StoredPattern.decoder)
        }


getPattern onReceived slug =
    Http.get
        { url = "/patterns/" ++ slug
        , expect = Http.expectJson onReceived StoredPattern.decoder
        }


createPattern onCreateResponse storedPattern =
    Http.post
        { url = "/patterns"
        , body = Http.jsonBody (StoredPattern.encode storedPattern)
        , expect = Http.expectWhatever onCreateResponse
        }


updatePattern onUpdateResponse storedPattern =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/patterns"
        , body = Http.jsonBody (StoredPattern.encode storedPattern)
        , expect = Http.expectWhatever onUpdateResponse
        , timeout = Nothing
        , tracker = Nothing
        }
