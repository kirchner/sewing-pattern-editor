module Api exposing
    ( createPattern
    , getPattern
    , updatePattern
    , getPatterns
    )

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
