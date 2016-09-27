module Login exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (align, type', style)
import Html.Events exposing (onClick, onInput)
import Http as Http exposing (Error)
import Json.Decode as De exposing (Decoder, at, int, string)
import Json.Encode as En
import Task exposing (Task, perform)
import User


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { account : String
    , password : String
    , status : String
    , cookie : String
    }


type Msg
    = Auth
    | Username String
    | Password String
    | Succeed (Maybe String)
    | Fail Error


decoder : Decoder (Maybe String)
decoder =
    De.maybe <| at [ "user_session" ] string


verify : String -> String -> Task Error (Maybe String)
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
    perform Fail Succeed <| verify username password


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "" "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Auth ->
            ( model, auth model.account model.password )

        Username username ->
            ( { model | account = username }, Cmd.none )

        Password password' ->
            ( { model | password = password' }, Cmd.none )

        Succeed resp ->
            let
                maybeSession =
                    Maybe.withDefault "unknown" resp

                newUser =
                    User.Context model.username model.password maybeSession
            in
                (Debug.log maybeSession)
                ( { model | status = "succeed", cookie = maybeSession }, Cmd.none )

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
