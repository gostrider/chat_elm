module Models.Application exposing (..)

import Controllers.Login exposing (Login, LoginAction)
import Models.ChatList exposing (ChatList, ChatListAction)
import Models.Message exposing (Messages, MessageUpdate)


type alias Application =
    { login : Login
    , chatList : ChatList
    , messages : Messages
    }


type ApplicationAction
    = LoginAction_ LoginAction
    | ChatListAction_ ChatListAction
    | MessageUpdate_ MessageUpdate
