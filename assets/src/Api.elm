module Api exposing (getUser)

import Http
import Viewer exposing (Viewer)


getUser : (Result Http.Error Viewer -> msg) -> Cmd msg
getUser receivedViewer =
    Http.request
        { method = "GET"
        , headers = []
        , url = "/api/user"
        , body = Http.emptyBody
        , expect = Http.expectJson receivedViewer Viewer.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
