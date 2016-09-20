module MessageItem exposing (..)

import CSS
import Html exposing (Html, br, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Json.Decode exposing (Decoder, at, string, list, object5)
import Http exposing (Error, get)
import Task exposing (Task, perform)


type alias Model =
    { sender : String
    , recipient : String
    , body : String
    , send_direction : String
    , avatar : String
    }


type Msg
    = Succeed (List Model)
    | Fail Error


decodeMsgList : Decoder (List Model)
decodeMsgList =
    list <|
        object5 Model
            (at [ "sender" ] string)
            (at [ "recipient" ] string)
            (at [ "body" ] string)
            (at [ "send_direction" ] string)
            (at [ "avatar" ] string)


messageList : String -> Task Error (List Model)
messageList user_id =
    get decodeMsgList <| "http://localhost:3000/get_userchat?user_id=" ++ user_id


getMessageList : String -> Cmd Msg
getMessageList user_id =
    perform Fail Succeed <| messageList user_id


msgItem : Model -> Html Msg
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
                content CSS.floatLeft

            "server" ->
                content CSS.floatRight

            _ ->
                content CSS.floatLeft
