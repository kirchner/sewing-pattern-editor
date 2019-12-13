module Git exposing
    ( Identity(..), Repo, Ref, defaultRef, commit, branch, tag, refToString, refFromString
    , repoParser, refParser
    , PatternData, getPattern, putPattern
    , Meta, getMeta
    , Permissions, getPermissions
    )

{-|

@docs Identity, Repo, Ref, defaultRef, commit, branch, tag, refToString, refFromString
@docs repoParser, refParser

@docs PatternData, getPattern, putPattern
@docs Meta, getMeta
@docs Permissions, getPermissions

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


type Identity
    = Anonymous
    | OauthToken String


type alias Repo =
    { owner : String
    , name : String
    }


repoParser : Parser (Repo -> a) a
repoParser =
    map Repo (string </> string)


type Ref
    = Commit String
    | Branch String
    | Tag String


refParser : Parser (Ref -> a) a
refParser =
    oneOf
        [ map Commit (s "commits" </> string)
        , map Branch (s "branches" </> string)
        , map Tag (s "tags" </> string)
        , map defaultRef top
        ]


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
    Identity
    ->
        { repo : Repo
        , ref : Ref
        , onPatternData : Result Http.Error (PatternData coordinates) -> msg
        }
    -> Cmd msg
getPattern identity { repo, ref, onPatternData } =
    get identity
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
    Identity
    ->
        { repo : Repo
        , message : String
        , pattern : Pattern coordinates
        , sha : String
        , onSha : Result Http.Error String -> msg
        }
    -> Cmd msg
putPattern identity { repo, message, pattern, sha, onSha } =
    let
        content =
            pattern
                |> Pattern.encode
                |> Encode.encode 2
                |> Base64.encode
    in
    put identity
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


getMeta :
    Identity
    ->
        { repo : Repo
        , ref : Ref
        , onMeta : Result Http.Error Meta -> msg
        }
    -> Cmd msg
getMeta identity { repo, ref, onMeta } =
    get identity
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
    Identity
    ->
        { repo : Repo
        , onPermissions : Result Http.Error Permissions -> msg
        }
    -> Cmd msg
getPermissions identity { repo, onPermissions } =
    get identity
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



---- SHARED


get :
    Identity
    ->
        { repo : Repo
        , endpoint : List String
        , params : List QueryParameter
        , onData : Result Http.Error a -> msg
        , decoder : Decoder a
        }
    -> Cmd msg
get identity { repo, endpoint, params, onData, decoder } =
    Http.request
        { method = "GET"
        , headers = headers identity
        , url = url repo endpoint params
        , body = Http.emptyBody
        , expect = Http.expectJson onData decoder
        , timeout = Nothing
        , tracker = Nothing
        }


put :
    Identity
    ->
        { repo : Repo
        , endpoint : List String
        , body : Value
        , onData : Result Http.Error a -> msg
        , decoder : Decoder a
        }
    -> Cmd msg
put identity { repo, endpoint, body, onData, decoder } =
    Http.request
        { method = "PUT"
        , headers = headers identity
        , url = url repo endpoint []
        , body = Http.jsonBody body
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
