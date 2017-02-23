module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (align, type_, style)
import Html.Events exposing (onClick, onInput)
import Http as Http exposing (Error)
import Json.Decode as De exposing (Decoder, at, int, string, map3)
import Json.Encode as En
import Task exposing (Task, perform)


-- Function only

import Interpol exposing (store)
import Navigation exposing (newUrl)


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
    = Auth
    | Username String
    | Password String
    | Succeed AuthResp
    | Fail Error


init : Model
init =
    Model "" "" "" (AuthResp "" "" "")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username_ ->
            ( { model | username = username_ }, Cmd.none )

        Password password_ ->
            ( { model | password = password_ }, Cmd.none )

        Auth ->
            ( model, auth model.username model.password )

        Succeed resp ->
            ( { model
                | status = "succeed"
                , user = resp
              }
            , Cmd.batch
                [ store (encodeAuth resp)
                , newUrl "#main"
                ]
            )

        Fail err ->
            ( { model | status = "failed" }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ align "center" ]
        [ h1 [] [ text "Title" ]
        , br [] []
        , text "Username: "
        , input [ onInput Username, type_ "text" ] []
        , br [] []
        , text "Password: "
        , input [ onInput Password, type_ "password" ] []
        , br [] []
        , div []
            [ text model.status
            , button [ onClick Auth ] [ text "Login" ]
            ]
        ]


decoder : Decoder AuthResp
decoder =
    map3 AuthResp
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
        { verb = "POST"
        , headers = [ ( "Content-Type", "application/json" ) ]
        , url = "http://localhost:3000/login"
        , body = Http.getString <| En.encode 0 payload
        }
            |> Http.send
            |> Http.expectJson decoder


auth : String -> String -> Cmd Msg
auth username password =
    perform Fail Succeed (verify username password)


encodeAuth : AuthResp -> String
encodeAuth auth =
    let
        payload =
            En.object
                [ ( "id", En.string auth.id )
                , ( "uuid", En.string auth.uuid )
                , ( "session", En.string auth.session )
                ]
    in
        En.encode 0 payload


makeLogin : String -> AuthResp -> Model
makeLogin status login =
    Model "" "" status login
