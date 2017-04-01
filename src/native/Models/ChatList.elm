module Models.ChatList exposing (..)

import Http exposing (Error)


type alias ChatItem =
    { avatar : String
    , body : String
    , contact : String
    , recipient : String
    , sender : String
    }


type alias ChatList =
    { chatList : List ChatItem
    , current : String
    }


type ChatListAction
    = GetUserChat String
    | Succeed (List ChatItem)
    | Fail Error
