module That
    exposing
        ( Change
        , That
        , areEqual
        , changes
        , decoder
        , encode
        , objectId
        , that
        , toComparable
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type That object
    = That ThatData


type alias ThatData =
    { changes : List Change
    , objectId : Int
    }


type alias Change =
    { transformationId : Int
    , branch : Maybe Int
    }


that : List Change -> Int -> That object
that cs id =
    That (ThatData cs id)


objectId : That object -> Int
objectId (That data) =
    data.objectId


changes : That object -> List Change
changes (That data) =
    data.changes


toComparable : That a -> ( List ( Int, Int ), Int )
toComparable (That data) =
    ( List.map
        (\{ transformationId, branch } ->
            case branch of
                Nothing ->
                    ( -1, transformationId )

                Just branchId ->
                    ( branchId, transformationId )
        )
        data.changes
    , data.objectId
    )


areEqual : That a -> That a -> Bool
areEqual thatA thatB =
    toComparable thatA == toComparable thatB


encode : That a -> Value
encode (That data) =
    Encode.object
        [ ( "changes"
          , Encode.list encodeChange data.changes
          )
        , ( "objectId", Encode.int data.objectId )
        ]


encodeChange : Change -> Value
encodeChange { transformationId, branch } =
    Encode.object
        [ ( "transformationId", Encode.int transformationId )
        , ( "branch"
          , case branch of
                Nothing ->
                    Encode.null

                Just actualBranch ->
                    Encode.int actualBranch
          )
        ]


decoder : Decoder (That a)
decoder =
    Decode.succeed ThatData
        |> Decode.required "changes" (Decode.list changeDecoder)
        |> Decode.required "objectId" Decode.int
        |> Decode.map That


changeDecoder : Decoder Change
changeDecoder =
    Decode.succeed Change
        |> Decode.required "transformationId" Decode.int
        |> Decode.required "branch" (Decode.nullable Decode.int)
