module Views.MainView exposing (..)

import Html exposing (Html, div)
import Models.Application as App exposing (Application, ApplicationAction(LoginAction_, ChatListAction_, MessageUpdate_))
import Models.Login exposing (Status(LoginSucceed, LoginFailed))
import Models.ChatList exposing (ChatListAction)
import Controllers.Login as Login
import Controllers.ChatItem as ChatItem
import Controllers.MessageItem as Messages
import Utils.CSS as CSS


applicationView : Application -> Html ApplicationAction
applicationView app =
    case app.login.user.status of
        LoginSucceed ->
            div []
                [ Html.map ChatListAction_ (ChatItem.view app.chatList)
                , div [ CSS.rightPanel ]
                    [ Html.map MessageUpdate_ (Messages.view app.messages)
                    ]
                ]

        LoginFailed ->
            Html.map LoginAction_ (Login.view app.login)
