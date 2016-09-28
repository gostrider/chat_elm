module RMain exposing (..)

import Login
import ChatItem
import MessageItem
import CSS
import Html.App as App
import Html exposing (Html, div)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { login : Login.Model
    , chatList : ChatItem.Model
    , msgList : MessageItem.Model
    }


type Msg
    = UpdateLogin Login.Msg
    | UpdateChatList ChatItem.Msg
    | UpdateMsgList MessageItem.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( initChatList, cmdChatList ) =
            ChatItem.init
    in
        Model Login.init initChatList
            ! [ Cmd.map UpdateChatList cmdChatList
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin msgLogin ->
            let
                ( login', effectLogin ) =
                    Login.update msgLogin model.login
            in
                ( { model | login = login' }, Cmd.map UpdateLogin effectLogin )

        UpdateChatList msgChatList ->
            let
                ( chatList', effectChatList ) =
                    ChatItem.update msgChatList model.chatList
            in
                ( { model | chatList = chatList' }, Cmd.map UpdateChatList effectChatList )

        UpdateMsgList msgMsgList ->
            let
                ( newMsgList, effectMsgList ) =
                    MessageItem.update msgMsgList model.messageList
            in
                { model | messageList = newMsgList }
                    ! [ Cmd.map UpdateMsgList effectMsgList ]


view : Model -> Html Msg
view model =
    case model.login.status of
        "success" ->
            div []
                [ App.map UpdateChatList <| ChatItem.view model.chatList
                , div [ CSS.rightPanel ]
                    [ App.map UpdateMsgList <| MessageItem.view model.msgList
                    ]
                ]

        _ ->
            App.map UpdateLogin <| Login.view model.login
