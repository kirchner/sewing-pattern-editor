module Storage.Address exposing
    ( Address(..), toKey, parser, toPathSegments
    , decoder, encode
    )

{-|

@docs Address, toKey, parser, toPathSegments
@docs decoder, encode

-}

import Github
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Address
    = GithubRepo { repo : Github.Repo, ref : Github.Ref }
    | Browser { slug : String }


toKey : Address -> String -> String
toKey address name =
    String.join "/" <|
        case address of
            GithubRepo { repo, ref } ->
                [ "github", repo.owner, repo.name, Github.refToString ref, name ]

            Browser { slug } ->
                [ "browser", slug, name ]



---- URL


parser : Parser (Address -> a) a
parser =
    oneOf
        [ map (\repo ref -> GithubRepo { repo = repo, ref = ref })
            (top </> s "github" </> Github.repoParser </> Github.refParser)
        , map (\slug -> Browser { slug = slug })
            (top </> s "browser" </> string)
        ]


toPathSegments : Address -> List String
toPathSegments address =
    case address of
        GithubRepo { repo, ref } ->
            "github" :: repo.owner :: repo.name :: Github.refToPathSegments ref

        Browser { slug } ->
            [ "browser", slug ]



---- JSON


decoder : Decoder Address
decoder =
    Decode.oneOf
        [ Decode.succeed (\repo ref -> GithubRepo { repo = repo, ref = ref })
            |> Decode.required "repo" Github.repoDecoder
            |> Decode.required "ref" Github.refDecoder
            |> Decode.ensureType "gitRepo"
        , Decode.succeed (\slug -> Browser { slug = slug })
            |> Decode.required "slug" Decode.string
            |> Decode.ensureType "browser"
        ]


encode : Address -> Value
encode address =
    case address of
        GithubRepo { repo, ref } ->
            Encode.withType "gitRepo"
                [ ( "repo", Github.encodeRepo repo )
                , ( "ref", Github.encodeRef ref )
                ]

        Browser { slug } ->
            Encode.withType "browser"
                [ ( "slug", Encode.string slug ) ]
