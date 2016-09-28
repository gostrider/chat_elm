module ChatItem exposing (..)

--import ActionBar

import CSS exposing (leftPanel, rightPanel)


--import MessageItem

import Html.App as App
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, string, list, object3)
import Task exposing (Task, perform)


type alias ChatItem =
    { sender : String
    , avatar : String
    , body : String
    }


type alias Model =
    { chatList : List
    , current :
        String
        --    , messageList : MessageItem.Model
    }


type Msg
    = GetUserChat String
    | Succeed (List ChatItem)
    | Fail Error



--    | UpdateMsgList MessageItem.Msg


init : ( Model, Cmd Msg )
init =
    Model [] ! [ getChatList ]



--    let
--        ( initMsgList, cmdMsgList ) =
--            MessageItem.init
--    in
--        Model [] initMsgList
--            ! [ getChatList
--              , Cmd.map UpdateMsgList cmdMsgList
--              ]


decodeChatList : Decoder (List ChatItem)
decodeChatList =
    list <|
        object3 ChatItem
            (at [ "reduction", "sender" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "avatar" ] string)


chatList : Task Error (List ChatItem)
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
            ( { model | current = user_id }, Cmd.none )

        Succeed chatList' ->
            ( { model | chatList = chatList' }, Cmd.none )

        Fail _ ->
            ( model, Cmd.none )



--        UpdateMsgList msgMsgList ->
--            let
--                ( newMsgList, effectMsgList ) =
--                    MessageItem.update msgMsgList model.messageList
--            in
--                { model | messageList = newMsgList }
--                    ! [ Cmd.map UpdateMsgList effectMsgList ]


view : Model -> Html Msg
view model =
    div []
        [ div [ leftPanel ] <| List.map chatItem model.chatList
          --        , div [ rightPanel ]
          --            [ App.map UpdateMsgList <| MessageItem.view model.messageList
          --            ]
        ]
