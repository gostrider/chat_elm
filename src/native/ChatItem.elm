module ChatItem exposing (..)

import CSS exposing (leftPanel)
import Html exposing (Html, br, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error, get)
import Json.Decode as De exposing (Decoder, at, string)
import Task exposing (Task, perform)


type alias ChatItem =
    { avatar : String
    , body : String
    , contact : String
    , recipient : String
    , sender : String
    }


type alias Model =
    { chatList : List ChatItem
    , current : String
    }


type Msg
    = GetUserChat String
    | Succeed (List ChatItem)
    | Fail Error


init : Model
init =
    Model [] ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetUserChat user_id ->
            ( { model | current = user_id }, Cmd.none )

        Succeed senders ->
            ( { model | chatList = senders }, Cmd.none )

        Fail err ->
            ( model, Cmd.none )


view : Model -> Html Msg
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


chatList : String -> Task Error (List ChatItem)
chatList id =
    get decodeChatList <| "http://localhost:3000/userlist?account=" ++ id


getChatList : String -> Cmd Msg
getChatList id =
    perform Succeed (chatList id)


chatItem : ChatItem -> Html Msg
chatItem { avatar, body, contact } =
    div [ onClick <| GetUserChat contact ]
        [ text contact
        , br [] []
        , text body
        ]
