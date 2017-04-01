module Main exposing (..)

import Html exposing (program)
import Models.Application exposing (Application, ApplicationAction(..))
import Models.ChatList exposing (ChatListAction)
import Models.Message exposing (MessageUpdate)
import Views.MainView exposing (applicationView)
import Controllers.Login as Login
import Controllers.ChatItem as ChatItem
import Controllers.MessageItem as Messages


main : Program Never Application ApplicationAction
main =
    program
        { init = init
        , view = applicationView
        , update = update
        , subscriptions = subscriptions
        }


init : ( Application, Cmd ApplicationAction )
init =
    Application Login.init ChatItem.init Messages.init ! []


update : ApplicationAction -> Application -> ( Application, Cmd ApplicationAction )
update action app =
    case action of
        LoginAction_ loginAction ->
            let
                ( login_, effLogin ) =
                    Login.update loginAction app.login
            in
                ( { app | login = login_ }, Cmd.map LoginAction_ effLogin )

        ChatListAction_ chatListAction ->
            let
                ( chatList_, effChatList ) =
                    ChatItem.update chatListAction app.chatList
            in
                ( { app | chatList = chatList_ }, Cmd.map ChatListAction_ effChatList )

        MessageUpdate_ messageUpdate ->
            let
                ( messages_, effMessages ) =
                    Messages.update messageUpdate app.messages
            in
                ( { app | messages = messages_ }, Cmd.map MessageUpdate_ effMessages )


subscriptions : Application -> Sub ApplicationAction
subscriptions app =
    Sub.none
