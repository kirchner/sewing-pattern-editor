module Session exposing
    ( Session
    , navKey, domain
    , githubCred
    , anonymous, githubUser, toGithubUser
    )

{-|

@docs Session
@docs navKey, domain
@docs githubCred
@docs anonymous, githubUser, toGithubUser

-}

import Browser.Navigation
import Github


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
githubCred : Session -> Github.Cred
githubCred session =
    case session of
        Anonymous _ ->
            Github.noCred

        GithubUser accessToken _ ->
            Github.oauthCred accessToken


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


{-| -}
toGithubUser : String -> Session -> Session
toGithubUser accessToken session =
    case session of
        Anonymous data ->
            GithubUser accessToken data

        GithubUser _ data ->
            GithubUser accessToken data
