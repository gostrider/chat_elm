module Models.Message exposing (..)

import Http exposing (Error)


type alias Message =
    { avatar : String
    , body : String
    , recipient : String
    , sender : String
    , send_direction : String
    }


type alias Messages =
    { messages : List Message
    }


type MessageUpdate
    = Succeed (List Message)
    | Fail Error
    | RethinkChanges String
