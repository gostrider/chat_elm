module ChatItem exposing (..)

import CSS exposing (leftPanel)
import Html.App as App
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, at, string, list, object3)
import Http exposing (Error, get)
import Task exposing (Task, perform)
import MessageItem


type alias ChatItem =
    { sender : String
    , avatar : String
    , body : String
    }


type alias Model =
    List ChatItem


type Msg
    = GetUserChat String
    | Succeed Model
    | Fail Error


init : ( Model, Cmd Msg )
init =
    ( [], getChatList )


decodeChatList : Decoder Model
decodeChatList =
    list <|
        object3 ChatItem
            (at [ "reduction", "sender" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "avatar" ] string)


chatList : Task Error Model
chatList =
    get decodeChatList "http://localhost:3000/get_userlist"


getChatList : Cmd Msg
getChatList =
    perform Fail Succeed chatList


chatItem : ChatItem -> Html Msg
chatItem { sender, avatar, body } =
    div [ onClick <| GetUserChat sender ] [ text sender ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetUserChat user_id ->
            ( model, Cmd.none )

        Succeed chatList' ->
            ( chatList', Cmd.none )

        Fail _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ leftPanel ] <| List.map chatItem model
