module Git exposing
    ( Identity(..)
    , getPattern
    , Meta, getMeta
    , Repo, Permissions, getRepo
    )

{-|

    github:

        commit1 <- commit2 <- commit3 (master)


    locally:

        commit1 <- commit2 <- commit3 (master) [ <- commit4 <- commit5 ]

@docs Identity

@docs getPattern
@docs Meta, getMeta
@docs Repo, Permissions, getRepo

-}

import Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Pattern exposing (Pattern)
import Regex
import Url.Builder exposing (QueryParameter)


type Identity
    = Anonymous
    | OauthToken String


getPattern :
    Identity
    ->
        { owner : String
        , repo : String
        , ref : String
        , onPattern : Result Http.Error (Pattern coordinates) -> msg
        }
    -> Cmd msg
getPattern identity { owner, repo, ref, onPattern } =
    get identity
        { endpoint = [ "repos", owner, repo, "contents", "pattern.json" ]
        , params = [ Url.Builder.string "ref" ref ]
        , onData = onPattern
        , decoder =
            contentsDecoder
                |> Decode.andThen (decodeContent Pattern.decoder)
        }


type alias Meta =
    { name : String
    , description : String
    }


metaDecoder : Decoder Meta
metaDecoder =
    Decode.succeed Meta
        |> Decode.required "name" Decode.string
        |> Decode.required "description" Decode.string


getMeta :
    Identity
    ->
        { owner : String
        , repo : String
        , ref : String
        , onMeta : Result Http.Error Meta -> msg
        }
    -> Cmd msg
getMeta identity { owner, repo, ref, onMeta } =
    get identity
        { endpoint = [ "repos", owner, repo, "contents", "meta.json" ]
        , params = [ Url.Builder.string "ref" ref ]
        , onData = onMeta
        , decoder =
            contentsDecoder
                |> Decode.andThen (decodeContent metaDecoder)
        }


decodeContent : Decoder a -> Contents -> Decoder a
decodeContent aDecoder { content } =
    let
        removeNewlines =
            Regex.replace
                (Maybe.withDefault Regex.never (Regex.fromString "\\n"))
                (\_ -> "")
    in
    case Base64.decode (removeNewlines content) of
        Err error ->
            Decode.fail error

        Ok encodedContent ->
            case Decode.decodeString aDecoder encodedContent of
                Err _ ->
                    Decode.fail "could not decode pattern.json content"

                Ok pattern ->
                    Decode.succeed pattern


type alias Contents =
    { content : String }


contentsDecoder : Decoder Contents
contentsDecoder =
    Decode.succeed Contents
        |> Decode.required "content" Decode.string



---- REPO


type alias Repo =
    { permissions : Permissions }


type alias Permissions =
    { admin : Bool
    , push : Bool
    , pull : Bool
    }


noPermissions : Permissions
noPermissions =
    { admin = False
    , push = False
    , pull = False
    }


getRepo :
    Identity
    ->
        { owner : String
        , repo : String
        , onRepo : Result Http.Error Repo -> msg
        }
    -> Cmd msg
getRepo identity { owner, repo, onRepo } =
    get identity
        { endpoint = [ "repos", owner, repo ]
        , params = []
        , onData = onRepo
        , decoder = repoDecoder
        }


repoDecoder : Decoder Repo
repoDecoder =
    Decode.succeed Repo
        |> Decode.optional "permissions" permissionsDecoder noPermissions


permissionsDecoder : Decoder Permissions
permissionsDecoder =
    Decode.succeed Permissions
        |> Decode.required "admin" Decode.bool
        |> Decode.required "push" Decode.bool
        |> Decode.required "pull" Decode.bool



---- SHARED


get :
    Identity
    ->
        { endpoint : List String
        , params : List QueryParameter
        , onData : Result Http.Error a -> msg
        , decoder : Decoder a
        }
    -> Cmd msg
get identity { endpoint, params, onData, decoder } =
    Http.request
        { method = "GET"
        , headers = headers identity
        , url = url endpoint params
        , body = Http.emptyBody
        , expect = Http.expectJson onData decoder
        , timeout = Nothing
        , tracker = Nothing
        }


headers : Identity -> List Http.Header
headers identity =
    case identity of
        Anonymous ->
            [ Http.header "Accept" "application/vnd.github.v3+json"
            ]

        OauthToken oauthToken ->
            [ Http.header "Accept" "application/vnd.github.v3+json"
            , Http.header "Authorization" ("token " ++ oauthToken)
            ]


url : List String -> List QueryParameter -> String
url =
    Url.Builder.crossOrigin "https://api.github.com"
