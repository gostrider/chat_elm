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

        Succeed chatList' ->
            ( { model | chatList = chatList' }, Cmd.none )

        Fail _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ leftPanel ] <| List.map chatItem model.chatList


decodeChatList : Decoder (List ChatItem)
decodeChatList =
    De.list <|
        De.object3 ChatItem
            (at [ "reduction", "avatar" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "sender" ] string)


chatList : String -> Task Error (List ChatItem)
chatList id =
    get decodeChatList <| "http://localhost:3000/get_userlist?account=" ++ id


getChatList : String -> Cmd Msg
getChatList id =
    perform Fail Succeed <| chatList id


chatItem : ChatItem -> Html Msg
chatItem { avatar, body, sender } =
    -- TODO Wrong sender ID when last message sender is self
    -- TODO chatItem title should always sender ID
    div [ onClick <| GetUserChat sender ]
        [ text sender
        , br [] []
        , text body
        ]
