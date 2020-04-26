module Session exposing
    ( Session
    , key, csrfToken, viewer
    , fromViewer
    )

{-|

@docs Session
@docs key, csrfToken, viewer
@docs fromViewer

-}

import Browser.Navigation
import Viewer exposing (Viewer)


{-| -}
type Session
    = Guest SessionData
    | LoggedIn Viewer SessionData


type alias SessionData =
    { key : Browser.Navigation.Key
    , csrfToken : String
    }


{-| -}
key : Session -> Browser.Navigation.Key
key session =
    case session of
        Guest stuff ->
            stuff.key

        LoggedIn _ stuff ->
            stuff.key


{-| -}
csrfToken : Session -> String
csrfToken session =
    case session of
        Guest stuff ->
            stuff.csrfToken

        LoggedIn _ stuff ->
            stuff.csrfToken


{-| -}
viewer : Session -> Maybe Viewer
viewer session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn val _ ->
            Just val


{-| -}
fromViewer :
    { key : Browser.Navigation.Key
    , csrfToken : String
    }
    -> Maybe Viewer
    -> Session
fromViewer data maybeViewer =
    case maybeViewer of
        Nothing ->
            Guest data

        Just val ->
            LoggedIn val data
