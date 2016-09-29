module ActionBar exposing (..)

import CSS exposing (actionPanel, floatLeft, floatRight)
import Html exposing (Html, button, div, textarea, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Json.Encode as En
import Json.Decode exposing (at, int)
import Http as Http exposing (Error)
import Task exposing (Task, perform)


type alias Model =
    { message : String
    , action : String
    , visible : String
    }


type alias Context =
    { send_from : String
    , send_to : String
    , session : String
    }


type Msg
    = Send
    | Content String
    | Visible String
    | SendSucceed Int
    | Fail Error


send : String -> String -> String -> String -> Task Error Int
send s f t m =
    let
        payload =
            En.object
                [ ( "from", En.string f )
                , ( "to", En.string t )
                , ( "content", En.string m )
                , ( "cookie", En.string s )
                ]
    in
        Http.fromJson (at [ "status" ] int) <|
            Http.send Http.defaultSettings
                { verb = "POST"
                , headers = [ ( "Content-Type", "application/json" ) ]
                , url = "http://localhost:3000/reply"
                , body = Http.string <| En.encode 0 payload
                }


send_message : Context -> String -> Cmd Msg
send_message ctx message =
    perform Fail SendSucceed <| send ctx.session ctx.send_from ctx.send_to message


init : Model
init =
    Model "" "" "hidden"


update : Context -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        Content input ->
            ( { model | message = input }, Cmd.none )

        Visible value ->
            ( { model | visible = value }, Cmd.none )

        Send ->
            (Debug.log <| toString model.message)
            ( model, send_message ctx model.message )

        SendSucceed resp ->
            (Debug.log <| toString resp)
            ( { model | action = toString resp }, Cmd.none )

        Fail err ->
            ( { model | action = toString err }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ actionPanel, style [ ( "visibility", model.visible ) ] ]
        [ button [ floatRight, onClick Send ] [ text "send" ]
        , textarea [ floatLeft, style [ ( "width", "90%" ) ], onInput Content ] []
        ]
