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
        ( initChatList, cmdChatList ) =
            ChatItem.init
    in
        ( Model "" "" initChatList, Cmd.map UpdateChatList cmdChatList )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendMsg ->
            ( model, Cmd.none )

        InputMsg newMsg ->
            ( { model | currentMsg = newMsg }, Cmd.none )

        UpdateChatList msgChatList ->
            let
                ( newChatList, effectChatList ) =
                    ChatItem.update msgChatList model.chatList
            in
                { model | chatList = newChatList }
                    ! [ Cmd.map UpdateChatList effectChatList ]


view : Model -> Html Msg
view model =
    App.map UpdateChatList <| ChatItem.view model.chatList
