module Session exposing
    ( Session
    , navKey, domain
    , githubCred
    , requestGithubCred
    , anonymous
    )

{-|

@docs Session
@docs navKey, domain
@docs githubCred
@docs requestGithubCred
@docs anonymous

-}

import Browser.Navigation
import Github
import Route exposing (Route)
import Url.Builder exposing (QueryParameter)


{-| -}
type Session
    = Anonymous String SessionData


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


{-| -}
domain : Session -> String
domain session =
    case session of
        Anonymous _ stuff ->
            stuff.domain


{-| -}
githubCred : Session -> Github.Cred
githubCred session =
    case session of
        Anonymous _ _ ->
            Github.noCred


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


{-| -}
anonymous : String -> Browser.Navigation.Key -> String -> Session
anonymous clientId key domain_ =
    Anonymous clientId
        { key = key
        , domain = domain_
        }
