module Model exposing (..)

import CSS
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
    , chatList : List ChatItem.Model
    , currentHistory : List MessageItem.Model
    }


type Msg
    = SendMsg
    | InputMsg String
    | UpdateChatList (List ChatItem.Msg)
    | UpdateMsgList (List MessageItem.Msg)


init : ( Model, Cmd Msg )
init =
    ( Model "init" "" [] [], Cmd.map UpdateChatList (ChatItem.getChatList) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendMsg ->
            ( model, Cmd.none )

        InputMsg newMsg ->
            ( { model | currentMsg = newMsg }, Cmd.none )

        UpdateChatList r_chatList ->
            ( { model | chatList = r_chatList }, Cmd.none )

        UpdateMsgList r_MsgList ->
            ( { model | currentHistory = r_MsgList }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [ CSS.leftPanel ] <| List.map ChatItem.chatItem model.chatList
            , div [ CSS.rightPanel ]
                [ div [ CSS.messagePanel ] <| List.map MessageItem.msgItem model.currentHistory
                , div
                    [ CSS.actionPanel ]
                    [ button [ style [ ( "float", "right" ) ], onClick SendMsg ] [ text "send" ]
                    , textarea [ style [ ( "float", "left" ), ( "width", "90%" ) ] ] []
                    ]
                ]
            ]
        ]
