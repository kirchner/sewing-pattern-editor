module Session exposing
    ( Session
    , navKey, domain
    , githubCred
    , requestGithubCred
    , anonymous, githubUser, toGithubUser
    )

{-|

@docs Session
@docs navKey, domain
@docs githubCred
@docs requestGithubCred
@docs anonymous, githubUser, toGithubUser

-}

import Browser.Navigation
import Github
import Route exposing (Route)
import Url.Builder exposing (QueryParameter)


{-| -}
type Session
    = Anonymous String SessionData
    | GithubUser String SessionData


type alias SessionData =
    { key : Browser.Navigation.Key
    , domain : String
    }


{-| -}
navKey : Session -> Browser.Navigation.Key
navKey session =
    case session of
        Anonymous _ { key } ->
            key

        GithubUser _ { key } ->
            key


{-| -}
domain : Session -> String
domain session =
    case session of
        Anonymous _ stuff ->
            stuff.domain

        GithubUser _ stuff ->
            stuff.domain


{-| -}
githubCred : Session -> Github.Cred
githubCred session =
    case session of
        Anonymous _ _ ->
            Github.noCred

        GithubUser accessToken _ ->
            Github.oauthCred accessToken


{-| -}
requestGithubCred : Session -> Route -> List QueryParameter -> Cmd msg
requestGithubCred session route params =
    case session of
        Anonymous clientId _ ->
            Browser.Navigation.load <|
                Url.Builder.crossOrigin "https://github.com"
                    [ "login", "oauth", "authorize" ]
                    [ Url.Builder.string "client_id" clientId
                    , Url.Builder.string "redirect_uri" <|
                        Route.crossOrigin (domain session) route params
                    , Url.Builder.string "scope" "repo"
                    ]

        GithubUser _ _ ->
            Cmd.none


{-| -}
anonymous : String -> Browser.Navigation.Key -> String -> Session
anonymous clientId key domain_ =
    Anonymous clientId
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
        Anonymous _ data ->
            GithubUser accessToken data

        GithubUser _ data ->
            GithubUser accessToken data
