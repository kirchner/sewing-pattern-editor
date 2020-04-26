module Auth exposing (logout)

import Http
import Session exposing (Session)


logout : Session -> (Result Http.Error () -> msg) -> Cmd msg
logout session receivedLogout =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "x-csrf-token" (Session.csrfToken session) ]
        , url = "/auth/logout"
        , body = Http.emptyBody
        , expect = Http.expectWhatever receivedLogout
        , timeout = Nothing
        , tracker = Nothing
        }
