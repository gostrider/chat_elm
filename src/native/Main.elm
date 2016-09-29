module RMain exposing (..)

import Login
import ChatItem
import MessageItem
import ActionBar
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
    , messageList : MessageItem.Model
    , action : ActionBar.Model
    }


type Msg
    = UpdateLogin Login.Msg
    | UpdateChatList ChatItem.Msg
    | UpdateMsgList MessageItem.Msg
    | UpdateAction ActionBar.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( initChatList, cmdChatList ) =
            ChatItem.init
    in
        Model Login.init initChatList MessageItem.init ActionBar.init
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

        UpdateChatList (ChatItem.GetUserChat user_id) ->
            let
                ( chatList', effectChatList ) =
                    ChatItem.update (ChatItem.GetUserChat user_id) model.chatList
            in
                { model | chatList = chatList' }
                    ! [ Cmd.map UpdateChatList effectChatList
                      , Cmd.map UpdateMsgList <| MessageItem.getMessageList user_id
                      ]

        UpdateChatList msgChatList ->
            let
                ( chatList', effectChatList ) =
                    ChatItem.update msgChatList model.chatList
            in
                ( { model | chatList = chatList' }, Cmd.map UpdateChatList effectChatList )

        UpdateMsgList (MessageItem.Succeed messages') ->
            let
                context =
                    { send_from = model.login.user.id
                    , send_to = model.chatList.current
                    , session = model.login.user.session
                    }

                ( messageList', effectMsgList ) =
                    MessageItem.update (MessageItem.Succeed messages') model.messageList

                ( newAction, effectAction ) =
                    ActionBar.update context (ActionBar.Visible "visible") model.action
            in
                { model | messageList = messageList', action = newAction }
                    ! [ Cmd.map UpdateMsgList effectMsgList
                      , Cmd.map UpdateAction effectAction
                      ]

        UpdateMsgList msgMsgList ->
            let
                ( newMsgList, effectMsgList ) =
                    MessageItem.update msgMsgList model.messageList
            in
                ( { model | messageList = newMsgList }, Cmd.map UpdateMsgList effectMsgList )

        UpdateAction msgAction ->
            let
                context =
                    { send_from = model.login.user.id
                    , send_to = model.chatList.current
                    , session = model.login.user.session
                    }

                ( newAction, effectAction ) =
                    ActionBar.update context msgAction model.action
            in
                ( { model | action = newAction }, Cmd.map UpdateAction effectAction )


view : Model -> Html Msg
view model =
    case model.login.status of
        "success" ->
            -- TODO Refine CSS ordering
            div []
                [ App.map UpdateChatList <| ChatItem.view model.chatList
                , div [ CSS.rightPanel ]
                    [ App.map UpdateMsgList <| MessageItem.view model.messageList
                    , App.map UpdateAction <| ActionBar.view model.action
                    ]
                ]

        _ ->
            App.map UpdateLogin <| Login.view model.login
