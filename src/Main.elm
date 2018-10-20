module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Json.Encode exposing (Value)
import Page.Pattern as Pattern
import Url exposing (Url)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type Model
    = Pattern Pattern.Model


init : Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init value url key =
    let
        ( patternModel, patternCmd ) =
            Pattern.init value
    in
    ( Pattern patternModel
    , Cmd.map PatternMsg patternCmd
    )


view : Model -> Browser.Document Msg
view model =
    case model of
        Pattern patternModel ->
            let
                patternDocument =
                    Pattern.view patternModel
            in
            { title = patternDocument.title
            , body = List.map (Html.map PatternMsg) patternDocument.body
            }


type Msg
    = NoOp
    | PatternMsg Pattern.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( PatternMsg patternMsg, Pattern patternModel ) ->
            let
                ( newPatternModel, patternCmd ) =
                    Pattern.update patternMsg patternModel
            in
            ( Pattern newPatternModel
            , Cmd.map PatternMsg patternCmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Pattern patternModel ->
            Sub.map PatternMsg (Pattern.subscriptions patternModel)


onUrlRequest urlRequest =
    NoOp


onUrlChange url =
    NoOp
