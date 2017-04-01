module Services.LoginService exposing (..)

import Http as Http exposing (Error, header, jsonBody)
import Json.Encode as En
import Json.Decode as De exposing (Decoder, at, int, map3, string)


-- Function only

import Interpol exposing (store)


type alias Authentication =
    { status : Status
    , authentication : User
    }


type alias User =
    { id : String
    , uuid : String
    , session : String
    }


type Status
    = None
    | LoginSucceed
    | LoginFailed


type AuthStatus
    = Succeed User
    | Fail Error


init : Authentication
init =
    Authentication None (User "" "" "")


update : AuthStatus -> Authentication -> ( Authentication, Cmd msg )
update msg model =
    case msg of
        Succeed auth ->
            ( { model | status = LoginSucceed, authentication = auth }, Cmd.batch [ store (encoder auth) ] )

        Fail _ ->
            ( { model | status = LoginFailed }, Cmd.none )


encoder : User -> String
encoder auth =
    En.encode 0
        (En.object
            [ ( "id", En.string auth.id )
            , ( "uuid", En.string auth.uuid )
            , ( "session", En.string auth.session )
            ]
        )


decoder : Decoder User
decoder =
    map3 User
        (at [ "chat_id" ] string)
        (at [ "uuid" ] string)
        (at [ "user_session" ] string)


formPayload : String -> String -> En.Value
formPayload username password =
    En.object
        [ ( "username", En.string username )
        , ( "password", En.string password )
        ]


verify : En.Value -> Cmd AuthStatus
verify payload =
    Http.send authResult
        (Http.request
            { method = "POST"
            , headers = [ header "Content-Type" "application/json" ]
            , url = "http://localhost:3000/login"
            , body = jsonBody payload
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
        )


authResult : Result Error User -> AuthStatus
authResult result =
    case result of
        Ok r ->
            Succeed r

        Err err ->
            Fail err
