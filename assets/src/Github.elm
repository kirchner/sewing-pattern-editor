module Github exposing
    ( Cred(..)
    , noCred
    , Repo, Ref, defaultRef, commit, branch, tag
    , repoDecoder, encodeRepo, refDecoder, encodeRef
    , refToString, refToPathSegments, refFromString
    , repoParser, refParser
    , PatternData, getPattern, putPattern
    , Meta, getMeta, putMeta, metaDecoder, encodeMeta
    , Permissions, getPermissions
    , User, getAuthenticatedUser
    , Repository, createRepository
    )

{-|

@docs Cred
@docs noCred, oauthCred
@docs Repo, Ref, defaultRef, commit, branch, tag
@docs repoDecoder, encodeRepo, refDecoder, encodeRef
@docs refToString, refToPathSegments, refFromString
@docs repoParser, refParser

@docs PatternData, getPattern, putPattern
@docs Meta, getMeta, putMeta, metaDecoder, encodeMeta
@docs Permissions, getPermissions
@docs User, getAuthenticatedUser
@docs Repository, createRepository

-}

import Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Pattern exposing (Pattern)
import Regex
import Url.Builder exposing (QueryParameter)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Cred
    = Anonymous


noCred : Cred
noCred =
    Anonymous


type alias Repo =
    { owner : String
    , name : String
    }


repoDecoder : Decoder Repo
repoDecoder =
    Decode.succeed Repo
        |> Decode.required "owner" Decode.string
        |> Decode.required "name" Decode.string


encodeRepo : Repo -> Value
encodeRepo repo =
    Encode.object
        [ ( "owner", Encode.string repo.owner )
        , ( "name", Encode.string repo.name )
        ]


repoParser : Parser (Repo -> a) a
repoParser =
    map Repo (string </> string)


type Ref
    = Commit String
    | Branch String
    | Tag String


refDecoder : Decoder Ref
refDecoder =
    Decode.oneOf
        [ Decode.map Commit (Decode.field "commit" Decode.string)
        , Decode.map Branch (Decode.field "branch" Decode.string)
        , Decode.map Tag (Decode.field "tag" Decode.string)
        ]


encodeRef : Ref -> Value
encodeRef ref =
    case ref of
        Commit sha ->
            Encode.object [ ( "commit", Encode.string sha ) ]

        Branch name ->
            Encode.object [ ( "branch", Encode.string name ) ]

        Tag name ->
            Encode.object [ ( "branch", Encode.string name ) ]


refParser : Parser (Ref -> a) a
refParser =
    oneOf
        [ map Commit (s "commits" </> string)
        , map Branch (s "branches" </> string)
        , map Tag (s "tags" </> string)
        , map defaultRef top
        ]


refToPathSegments : Ref -> List String
refToPathSegments ref =
    case ref of
        Commit sha ->
            [ "commits", sha ]

        Branch name ->
            [ "branches", name ]

        Tag name ->
            [ "tags", name ]


refToString : Ref -> String
refToString ref =
    case ref of
        Commit sha ->
            "commits/" ++ sha

        Branch name ->
            "branches/" ++ name

        Tag name ->
            "tags/" ++ name


refFromString : String -> Maybe Ref
refFromString rawRef =
    case String.split "/" rawRef of
        "commits" :: sha :: [] ->
            Just (Commit sha)

        "branches" :: name :: [] ->
            Just (Branch name)

        "Tags" :: name :: [] ->
            Just (Tag name)

        _ ->
            Nothing


defaultRef : Ref
defaultRef =
    Branch "master"


commit : String -> Ref
commit =
    Commit


branch : String -> Ref
branch =
    Branch


tag : String -> Ref
tag =
    Tag



---- PATTERN


type alias PatternData coordinates =
    { pattern : Pattern coordinates
    , sha : String
    }


getPattern :
    Cred
    ->
        { repo : Repo
        , ref : Ref
        , onPatternData : Result Http.Error (PatternData coordinates) -> msg
        }
    -> Cmd msg
getPattern cred { repo, ref, onPatternData } =
    get cred
        { repo = repo
        , endpoint = [ "contents", "pattern.json" ]
        , params = [ refParam ref ]
        , onData = onPatternData
        , decoder =
            contentsDecoder
                |> Decode.andThen
                    (\contents ->
                        case Decode.decodeString Pattern.decoder contents.content of
                            Err _ ->
                                Decode.fail "could not decode pattern"

                            Ok pattern ->
                                Decode.succeed
                                    { pattern = pattern
                                    , sha = contents.sha
                                    }
                    )
        }


putPattern :
    Cred
    ->
        { repo : Repo
        , message : String
        , pattern : Pattern coordinates
        , sha : String
        , onSha : Result Http.Error String -> msg
        }
    -> Cmd msg
putPattern cred { repo, message, pattern, sha, onSha } =
    let
        content =
            pattern
                |> Pattern.encode
                |> Encode.encode 2
                |> Base64.encode
    in
    put cred
        { repo = repo
        , endpoint = [ "contents", "pattern.json" ]
        , body =
            Encode.object
                [ ( "message", Encode.string message )
                , ( "content", Encode.string content )
                , ( "sha", Encode.string sha )
                ]
        , onData = onSha
        , decoder = Decode.at [ "content", "sha" ] Decode.string
        }



---- GET META


type alias Meta =
    { name : String
    , description : String
    }


metaDecoder : Decoder Meta
metaDecoder =
    Decode.succeed Meta
        |> Decode.required "name" Decode.string
        |> Decode.required "description" Decode.string


encodeMeta : Meta -> Value
encodeMeta meta =
    Encode.object
        [ ( "name", Encode.string meta.name )
        , ( "description", Encode.string meta.description )
        ]


getMeta :
    Cred
    ->
        { repo : Repo
        , ref : Ref
        , onMeta : Result Http.Error Meta -> msg
        }
    -> Cmd msg
getMeta cred { repo, ref, onMeta } =
    get cred
        { repo = repo
        , endpoint = [ "contents", "meta.json" ]
        , params = [ refParam ref ]
        , onData = onMeta
        , decoder =
            contentsDecoder
                |> Decode.andThen
                    (\contents ->
                        case Decode.decodeString metaDecoder contents.content of
                            Err _ ->
                                Decode.fail "could not decode meta"

                            Ok meta ->
                                Decode.succeed meta
                    )
        }


putMeta :
    Cred
    ->
        { repo : Repo
        , message : String
        , meta : Meta
        , sha : String
        , onSha : Result Http.Error String -> msg
        }
    -> Cmd msg
putMeta cred { repo, message, meta, sha, onSha } =
    let
        content =
            meta
                |> encodeMeta
                |> Encode.encode 2
                |> Base64.encode
    in
    put cred
        { repo = repo
        , endpoint = [ "contents", "meta.json" ]
        , body =
            Encode.object
                [ ( "message", Encode.string message )
                , ( "content", Encode.string content )
                , ( "sha", Encode.string sha )
                ]
        , onData = onSha
        , decoder = Decode.at [ "content", "sha" ] Decode.string
        }



---- CONTENTS


type alias Contents =
    { content : String
    , sha : String
    }


contentsDecoder : Decoder Contents
contentsDecoder =
    Decode.succeed Contents
        |> Decode.required "content" (Decode.andThen decodeBase64 Decode.string)
        |> Decode.required "sha" Decode.string


decodeBase64 : String -> Decoder String
decodeBase64 base64encodedText =
    let
        removeNewlines =
            Regex.replace
                (Maybe.withDefault Regex.never (Regex.fromString "\\n"))
                (\_ -> "")
    in
    case Base64.decode (removeNewlines base64encodedText) of
        Err error ->
            Decode.fail error

        Ok text ->
            Decode.succeed text



---- GET PERMISSIONS


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


getPermissions :
    Cred
    ->
        { repo : Repo
        , onPermissions : Result Http.Error Permissions -> msg
        }
    -> Cmd msg
getPermissions cred { repo, onPermissions } =
    get cred
        { repo = repo
        , endpoint = []
        , params = []
        , onData = onPermissions
        , decoder = permissionsDecoder
        }


permissionsDecoder : Decoder Permissions
permissionsDecoder =
    Decode.oneOf
        [ Decode.field "permissions"
            (Decode.succeed Permissions
                |> Decode.required "admin" Decode.bool
                |> Decode.required "push" Decode.bool
                |> Decode.required "pull" Decode.bool
            )
        , Decode.succeed noPermissions
        ]



---- USER


type alias User =
    { login : String }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Decode.required "login" Decode.string


getAuthenticatedUser :
    Cred
    -> { onUser : Result Http.Error User -> msg }
    -> Cmd msg
getAuthenticatedUser cred { onUser } =
    Http.request
        { method = "GET"
        , headers = headers cred
        , url = Url.Builder.crossOrigin "https://api.github.com" [ "user" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson onUser userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



---- CREATE REPOSITORY


type alias Repository =
    { name : String
    , description : String
    , homepage : String
    , private : Bool
    }


createRepository :
    Cred
    ->
        { repository : Repository
        , onRepository : Result Http.Error Repository -> msg
        }
    -> Cmd msg
createRepository cred { repository, onRepository } =
    Http.request
        { method = "POST"
        , headers = headers cred
        , url = Url.Builder.crossOrigin "https://api.github.com" [ "user", "repos" ] []
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string repository.name )
                    , ( "description", Encode.string repository.description )
                    , ( "homepage", Encode.string repository.homepage )
                    , ( "private", Encode.bool repository.private )
                    ]
        , expect = Http.expectJson onRepository repositoryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


repositoryDecoder : Decoder Repository
repositoryDecoder =
    Decode.succeed Repository
        |> Decode.required "name" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.required "homepage" Decode.string
        |> Decode.required "private" Decode.bool



---- SHARED


get :
    Cred
    ->
        { repo : Repo
        , endpoint : List String
        , params : List QueryParameter
        , onData : Result Http.Error a -> msg
        , decoder : Decoder a
        }
    -> Cmd msg
get cred { repo, endpoint, params, onData, decoder } =
    Http.request
        { method = "GET"
        , headers = headers cred
        , url = url repo endpoint params
        , body = Http.emptyBody
        , expect = Http.expectJson onData decoder
        , timeout = Nothing
        , tracker = Nothing
        }


put :
    Cred
    ->
        { repo : Repo
        , endpoint : List String
        , body : Value
        , onData : Result Http.Error a -> msg
        , decoder : Decoder a
        }
    -> Cmd msg
put cred { repo, endpoint, body, onData, decoder } =
    Http.request
        { method = "PUT"
        , headers = headers cred
        , url = url repo endpoint []
        , body = Http.jsonBody body
        , expect = Http.expectJson onData decoder
        , timeout = Nothing
        , tracker = Nothing
        }


headers : Cred -> List Http.Header
headers cred =
    case cred of
        Anonymous ->
            [ Http.header "Accept" "application/vnd.github.v3+json"
            ]


url : Repo -> List String -> List QueryParameter -> String
url repo endpoint =
    Url.Builder.crossOrigin "https://api.github.com"
        ("repos" :: repo.owner :: repo.name :: endpoint)


refParam : Ref -> Url.Builder.QueryParameter
refParam ref =
    Url.Builder.string "ref" <|
        case ref of
            Commit sha ->
                sha

            Branch name ->
                name

            Tag name ->
                name
