module EitherTest exposing (..)

import Json.Decode exposing (Decoder, at, list, string, object3)
import Http exposing (Error, get)
import Task exposing (perform)


type alias ChatListItem =
    { sender : String
    , avatar : String
    , body : String
    }


type alias MsgListItem =
    { sender : String
    , recipient : String
    , body : String
    , direction : String
    }


type alias Model =
    Either (List ChatListItem) (List MsgListItem)


type Either a b
    = ChatListItem (List a)
    | MsgListItem (List b)


type Msg
    = Succeed (Either (List ChatListItem) (List MsgListItem))
    | Fail Error


decode : Decoder (List ChatListItem)
decode =
    list <|
        object3 ChatListItem
            (at [ "reduction", "sender" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "avatar" ] string)


getSth : Task Error ChatListItem
getSth =
    get decode "http://localhost:3000/get_userlist"


fetchT : Cmd Msg
fetchT =
    perform Fail Succeed getSth


main =
    fetchT
