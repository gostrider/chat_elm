module MessageItem exposing (..)

import CSS exposing (floatLeft, floatRight, messagePanel)
import Html exposing (Html, br, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Json.Decode exposing (Decoder, at, string, list, object5)
import Http exposing (Error, get)
import Task exposing (Task, perform)


type alias MessageItem =
    { sender : String
    , recipient : String
    , body : String
    , send_direction : String
    , avatar : String
    }


type alias Model =
    List MessageItem


type Msg
    = Succeed Model
    | Fail Error


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )


decodeMsgList : Decoder Model
decodeMsgList =
    list <|
        object5 MessageItem
            (at [ "sender" ] string)
            (at [ "recipient" ] string)
            (at [ "body" ] string)
            (at [ "send_direction" ] string)
            (at [ "avatar" ] string)


messageList : String -> Task Error Model
messageList user_id =
    get decodeMsgList <| "http://localhost:3000/get_userchat?user_id=" ++ user_id


getMessageList : String -> Cmd Msg
getMessageList user_id =
    perform Fail Succeed <| messageList user_id


msgItem : MessageItem -> Html Msg
msgItem { body, send_direction } =
    let
        content config =
            div []
                [ div [ config ] [ text body ]
                , br [] []
                ]
    in
        case send_direction of
            "client" ->
                content floatLeft

            "server" ->
                content floatRight

            _ ->
                content floatLeft


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Succeed messageList' ->
            ( messageList', Cmd.none )

        Fail _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ messagePanel ] <| List.map msgItem model
