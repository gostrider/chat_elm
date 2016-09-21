module Model exposing (..)

import Html.App as App
import Html exposing (Html)
import ChatItem
import MessageItem


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { status : String
    , currentMsg : String
    , chatList : ChatItem.Model
    }


type Msg
    = SendMsg
    | InputMsg String
    | UpdateChatList ChatItem.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( chatList, chatListCmd ) =
            ChatItem.init
    in
        ( Model "" "" chatList, Cmd.map UpdateChatList chatListCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendMsg ->
            ( model, Cmd.none )

        InputMsg newMsg ->
            ( { model | currentMsg = newMsg }, Cmd.none )

        UpdateChatList chatList' ->
            let
                ( newChatList, chatListEffect ) =
                    ChatItem.update chatList' model.chatList
            in
                { model | chatList = newChatList }
                    ! [ Cmd.map UpdateChatList chatListEffect ]


view : Model -> Html Msg
view model =
    App.map UpdateChatList <| ChatItem.view model.chatList
