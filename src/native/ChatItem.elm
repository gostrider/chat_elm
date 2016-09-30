module ChatItem exposing (..)

import CSS exposing (leftPanel, rightPanel)
import Html.App as App
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, string, list, object3)
import Task exposing (Task, perform)


type alias ChatItem =
    { sender : String
    , avatar : String
    , body : String
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
    list <|
        object3 ChatItem
            (at [ "reduction", "sender" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "avatar" ] string)


chatList : String -> Task Error (List ChatItem)
chatList id =
    get decodeChatList <| "http://localhost:3000/get_userlist?account=" ++ id


getChatList : String -> Cmd Msg
getChatList id =
    perform Fail Succeed <| chatList id


chatItem : ChatItem -> Html Msg
chatItem { sender, avatar, body } =
    div [ onClick <| GetUserChat sender ] [ text sender ]
