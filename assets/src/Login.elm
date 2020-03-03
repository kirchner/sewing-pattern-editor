module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



---- MODEL


type Model
    = Error
    | Loading LoadingModel
    | LoggedOut LoggedOutModel
    | LoggedIn LoggedInModel


type alias LoadingModel =
    { csrfToken : String
    }


type alias LoggedOutModel =
    { csrfToken : String
    , email : String
    }


type alias LoggedInModel =
    { csrfToken : String
    , user : User
    }


type alias User =
    { name : String
    , username : String
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Decode.required "name" Decode.string
        |> Decode.required "username" Decode.string



---- INIT


type alias Flags =
    { csrfToken : String }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Decode.required "csrfToken" Decode.string


init : Value -> ( Model, Cmd Msg )
init value =
    case Decode.decodeValue flagsDecoder value of
        Err _ ->
            ( Error
            , Cmd.none
            )

        Ok { csrfToken } ->
            ( Loading { csrfToken = csrfToken }
            , getSession
            )



---- UPDATE


type Msg
    = UserChangedEmail String
    | UserSubmittedLogInForm
    | ReceivedSession (Result Http.Error User)
    | UserPressedLogOut
    | ReceivedDeletedSession (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Error ->
            ( model, Cmd.none )

        Loading loadingModel ->
            updateLoading msg loadingModel

        LoggedOut loggedOutModel ->
            updateLoggedOut msg loggedOutModel

        LoggedIn loggedInModel ->
            updateLoggedIn msg loggedInModel


updateLoading : Msg -> LoadingModel -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        ReceivedSession result ->
            case result of
                Err httpError ->
                    case httpError of
                        Http.BadStatus 404 ->
                            ( LoggedOut
                                { csrfToken = model.csrfToken
                                , email = ""
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( Error
                            , Cmd.none
                            )

                Ok user ->
                    ( LoggedIn
                        { csrfToken = model.csrfToken
                        , user = user
                        }
                    , Cmd.none
                    )

        _ ->
            ( Loading model, Cmd.none )


updateLoggedOut : Msg -> LoggedOutModel -> ( Model, Cmd Msg )
updateLoggedOut msg model =
    case msg of
        UserChangedEmail email ->
            ( LoggedOut { model | email = email }
            , Cmd.none
            )

        UserSubmittedLogInForm ->
            ( LoggedOut model
            , createSession model.csrfToken model.email
            )

        ReceivedSession result ->
            case result of
                Err httpError ->
                    ( LoggedOut model
                    , Cmd.none
                    )

                Ok user ->
                    ( LoggedIn
                        { csrfToken = model.csrfToken
                        , user = user
                        }
                    , Cmd.none
                    )

        _ ->
            ( LoggedOut model, Cmd.none )


updateLoggedIn : Msg -> LoggedInModel -> ( Model, Cmd Msg )
updateLoggedIn msg model =
    case msg of
        UserPressedLogOut ->
            ( LoggedIn model
            , deleteSession model.csrfToken
            )

        ReceivedDeletedSession result ->
            case result of
                Err _ ->
                    ( LoggedIn model
                    , Cmd.none
                    )

                Ok _ ->
                    ( LoggedOut
                        { csrfToken = model.csrfToken
                        , email = ""
                        }
                    , Browser.Navigation.reload
                    )

        _ ->
            ( LoggedIn model
            , Cmd.none
            )


getSession : Cmd Msg
getSession =
    Http.get
        { url = "/api/sessions"
        , expect =
            Http.expectJson ReceivedSession <|
                Decode.field "data" userDecoder
        }


createSession : String -> String -> Cmd Msg
createSession csrfToken email =
    Http.request
        { method = "POST"
        , url = "/api/sessions"
        , headers = [ Http.header "X-CSRF-Token" csrfToken ]
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "user"
                      , Encode.object
                            [ ( "email", Encode.string email )
                            , ( "password", Encode.null )
                            ]
                      )
                    ]
        , expect =
            Http.expectJson ReceivedSession <|
                Decode.field "data" userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteSession : String -> Cmd Msg
deleteSession csrfToken =
    Http.request
        { method = "DELETE"
        , url = "/api/sessions"
        , headers = [ Http.header "X-CSRF-Token" csrfToken ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever ReceivedDeletedSession
        , timeout = Nothing
        , tracker = Nothing
        }



---- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Error ->
            { title = "SewingHub - Error"
            , body = []
            }

        Loading _ ->
            { title = "SewingHub - Loading"
            , body = []
            }

        LoggedOut loggedOutModel ->
            { title = "SewingHub"
            , body = [ viewLogInForm loggedOutModel ]
            }

        LoggedIn loggedInModel ->
            { title = "SewingHub"
            , body = [ viewLoggedIn loggedInModel ]
            }


viewLogInForm : LoggedOutModel -> Html Msg
viewLogInForm model =
    Html.div []
        [ Html.h1 [] [ Html.text "Please log in" ]
        , Html.form
            [ Events.onSubmit UserSubmittedLogInForm ]
            [ Html.label []
                [ Html.text "Your email"
                , Html.input
                    [ Attributes.type_ "text"
                    , Attributes.value model.email
                    , Events.onInput UserChangedEmail
                    ]
                    []
                ]
            , Html.button
                [ Attributes.type_ "submit" ]
                [ Html.text "Log in" ]
            ]
        ]


viewLoggedIn : LoggedInModel -> Html Msg
viewLoggedIn model =
    Html.div []
        [ Html.h1 [] [ Html.text ("Hello " ++ model.user.username) ]
        , Html.button
            [ Events.onClick UserPressedLogOut ]
            [ Html.text "Log out" ]
        ]
