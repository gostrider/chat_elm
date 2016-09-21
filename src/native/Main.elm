module Model exposing (..)

import CSS exposing (floatLeft, floatRight, actionPanel, rightPanel)
import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, string, list, object3, object5)
import Task exposing (Task, perform)
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
    , messageList : MessageItem.Model
    }


type Msg
    = SendMsg
    | InputMsg String
    | UpdateChatList ChatItem.Msg
    | UpdateMsgList MessageItem.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( chatList, chatListCmd ) =
            ChatItem.init

        ( messageList, messageListCmd ) =
            MessageItem.init
    in
        Model "" "" chatList messageList
            ! [ Cmd.map UpdateChatList chatListCmd
              , Cmd.map UpdateMsgList messageListCmd
              ]


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

        UpdateMsgList msgList' ->
            let
                ( newMsgList, msgListEffect ) =
                    MessageItem.update msgList' model.messageList
            in
                { model | messageList = newMsgList }
                    ! [ Cmd.map UpdateMsgList msgListEffect ]


view : Model -> Html Msg
view model =
    div []
        [ App.map UpdateChatList <| ChatItem.view model.chatList
        , div [ rightPanel ]
            [ App.map UpdateMsgList <| MessageItem.view model.messageList
            , div [ actionPanel ]
                [ button [ floatRight, onClick SendMsg ] [ text "send" ]
                , textarea [ floatLeft, style [ ( "width", "90%" ) ] ] []
                ]
            ]
        ]
