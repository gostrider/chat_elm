module Controllers.ChatItem exposing (..)

import Utils.CSS exposing (leftPanel)
import Html exposing (Html, br, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error, Request, get)
import Json.Decode as De exposing (Decoder, at, string)
import Models.ChatList exposing (ChatItem, ChatList, ChatListAction(..))


init : ChatList
init =
    ChatList [] ""


update : ChatListAction -> ChatList -> ( ChatList, Cmd ChatListAction )
update action model =
    case action of
        GetUserChat user_id ->
            ( { model | current = user_id }, Cmd.none )

        Succeed senders ->
            ( { model | chatList = senders }, Cmd.none )

        Fail _ ->
            ( model, Cmd.none )


view : ChatList -> Html ChatListAction
view model =
    div [ leftPanel ] <| List.map chatItem model.chatList


decodeChatList : Decoder (List ChatItem)
decodeChatList =
    De.list <|
        De.map5 ChatItem
            (at [ "reduction", "avatar" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "group" ] string)
            (at [ "reduction", "recipient" ] string)
            (at [ "reduction", "sender" ] string)


chatList : String -> Request (List ChatItem)
chatList id =
    get ("http://localhost:3000/userlist?account=" ++ id) decodeChatList


chatItem : ChatItem -> Html ChatListAction
chatItem { avatar, body, contact } =
    div [ onClick (GetUserChat contact) ]
        [ text contact
        , br [] []
        , text body
        ]
