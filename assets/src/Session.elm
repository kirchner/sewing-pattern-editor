module Session exposing
    ( Session
    , navKey, domain
    , anonymous
    )

{-|

@docs Session
@docs navKey, domain
@docs anonymous

-}

import Browser.Navigation
import Route exposing (Route)
import Url.Builder exposing (QueryParameter)


{-| -}
type Session
    = Anonymous SessionData


type alias SessionData =
    { key : Browser.Navigation.Key
    , domain : String
    }


{-| -}
navKey : Session -> Browser.Navigation.Key
navKey session =
    case session of
        Anonymous { key } ->
            key


{-| -}
domain : Session -> String
domain session =
    case session of
        Anonymous stuff ->
            stuff.domain


{-| -}
anonymous : Browser.Navigation.Key -> String -> Session
anonymous key domain_ =
    Anonymous
        { key = key
        , domain = domain_
        }
