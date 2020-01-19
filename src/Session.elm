module Session exposing
    ( Session
    , navKey, domain
    , anonymous, githubUser
    )

{-|

@docs Session
@docs navKey, domain
@docs anonymous, githubUser

-}

import Browser.Navigation


{-| -}
type Session
    = Anonymous SessionData
    | GithubUser String SessionData


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

        GithubUser _ { key } ->
            key


{-| -}
domain : Session -> String
domain session =
    case session of
        Anonymous stuff ->
            stuff.domain

        GithubUser _ stuff ->
            stuff.domain


{-| -}
anonymous : Browser.Navigation.Key -> String -> Session
anonymous key domain_ =
    Anonymous
        { key = key
        , domain = domain_
        }


{-| -}
githubUser : String -> Browser.Navigation.Key -> String -> Session
githubUser accessToken key domain_ =
    GithubUser accessToken
        { key = key
        , domain = domain_
        }
