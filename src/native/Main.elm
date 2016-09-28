module Main exposing (..)

import Html.App as App
import Html exposing (Html)
import ChatItem
import MessageItem
import Login


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { userStatus : Login.Model
    , chatList : ChatItem.Model
    }


type Msg
    = UpdateChatList ChatItem.Msg
    | UpdateLogin Login.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( initChatList, cmdChatList ) =
            ChatItem.init
    in
        Model Login.init initChatList
            ! [ Cmd.map UpdateChatList cmdChatList
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateChatList msgChatList ->
            let
                ( newChatList, effectChatList ) =
                    ChatItem.update msgChatList model.chatList
            in
                ( { model | chatList = newChatList }, Cmd.map UpdateChatList effectChatList )

        UpdateLogin msgLogin ->
            let
                ( newLogin, effectLogin ) =
                    Login.update msgLogin model.userStatus
            in
                ( { model | userStatus = newLogin }, Cmd.map UpdateLogin effectLogin )


view : Model -> Html Msg
view model =
    case model.userStatus.status of
        "succeed" ->
            App.map UpdateChatList <| ChatItem.view model.chatList

        _ ->
            App.map UpdateLogin <| Login.view model.userStatus
