module ChatItem exposing (..)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, at, string, list, object3)
import Http exposing (Error, get)
import Task exposing (Task, perform)


type alias Model =
    { sender : String
    , avatar : String
    , body : String
    }


type Msg
    = GetUserChat String
    | Succeed (List Model)
    | Fail Error


decodeChatList : Decoder (List Model)
decodeChatList =
    list <|
        object3 Model
            (at [ "reduction", "sender" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "avatar" ] string)


chatList : Task Error (List Model)
chatList =
    get decodeChatList "http://localhost:3000/get_userlist"


getChatList : Cmd Msg
getChatList =
    perform Fail Succeed chatList


chatItem : Model -> Html Msg
chatItem { sender, avatar, body } =
    div [ onClick <| GetUserChat sender ] [ text sender ]
