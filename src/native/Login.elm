module Login exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (align, type', style)
import Html.Events exposing (onClick, onInput)
import Http as Http exposing (Error)
import Json.Decode as De exposing (Decoder, at, int, string, object3)
import Json.Encode as En
import Task exposing (Task, perform)


type alias Model =
    { username : String
    , password : String
    , status : String
    , user : AuthResp
    }


type alias AuthResp =
    { id : String
    , uuid : String
    , session : String
    }


type Msg
    = Username String
    | Password String
    | Auth
    | Succeed AuthResp
    | Fail Error


init : Model
init =
    Model "" "" "" (AuthResp "" "" "")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username' ->
            ( { model | username = username' }, Cmd.none )

        Password password' ->
            ( { model | password = password' }, Cmd.none )

        Auth ->
            ( model, auth model.username model.password )

        Succeed resp ->
            ( { model
                | status = "success"
                , user = resp
              }
            , Cmd.none
            )

        Fail err ->
            ( { model | status = "failed" }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ align "center" ]
        [ h1 [] [ text "Title" ]
        , br [] []
        , text "Username: "
        , input [ onInput Username, type' "text" ] []
        , br [] []
        , text "Password: "
        , input [ onInput Password, type' "password" ] []
        , br [] []
        , div []
            [ text model.status
            , button [ onClick Auth ] [ text "Login" ]
            ]
        ]


decoder : Decoder AuthResp
decoder =
    object3 AuthResp
        (at [ "chat_id" ] string)
        (at [ "uuid" ] string)
        (at [ "user_session" ] string)


verify : String -> String -> Task Error AuthResp
verify username password =
    let
        payload =
            En.object
                [ ( "username", En.string username )
                , ( "password", En.string password )
                ]
    in
        Http.fromJson decoder <|
            Http.send Http.defaultSettings
                { verb = "POST"
                , headers = [ ( "Content-Type", "application/json" ) ]
                , url = "http://localhost:3000/login"
                , body = Http.string <| En.encode 0 payload
                }


auth : String -> String -> Cmd Msg
auth username password =
    -- perform Fail Succeed <| verify username password
    verify username password |> perform Fail Succeed
