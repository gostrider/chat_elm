module Model exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, string, list, object3, object5)
import Task exposing (Task, perform)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias ChatListItem =
    { sender : String
    , avatar : String
    , body : String
    }


type alias MsgListItem =
    { sender : String
    , recipient : String
    , body : String
    , send_direction : String
    , avatar : String
    }


type alias Model =
    { status : String
    , currentMsg : String
    , chatList : List ChatListItem
    , currentHistory : List MsgListItem
    }


type Msg
    = SendMsg
    | GetUserChat String
    | InputMsg String
    | ChatListSucceed (List ChatListItem)
    | MsgListSucceed (List MsgListItem)
    | Fail Error


decodeChatList : Decoder (List ChatListItem)
decodeChatList =
    list <|
        object3 ChatListItem
            (at [ "reduction", "sender" ] string)
            (at [ "reduction", "body" ] string)
            (at [ "reduction", "avatar" ] string)


chatList : Task Error (List ChatListItem)
chatList =
    get decodeChatList "http://localhost:3000/get_userlist"


getChatList : Cmd Msg
getChatList =
    perform Fail ChatListSucceed chatList


decodeMsgList : Decoder (List MsgListItem)
decodeMsgList =
    list <|
        object5 MsgListItem
            (at [ "sender" ] string)
            (at [ "recipient" ] string)
            (at [ "body" ] string)
            (at [ "send_direction" ] string)
            (at [ "avatar" ] string)


messageList : String -> Task Error (List MsgListItem)
messageList user_id =
    get decodeMsgList <| "http://localhost:3000/get_userchat?user_id=" ++ user_id


getMessageList : String -> Cmd Msg
getMessageList user_id =
    perform Fail MsgListSucceed <| messageList user_id


init : ( Model, Cmd Msg )
init =
    ( Model "init" "" [] [], getChatList )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendMsg ->
            (model, Cmd.none)

        InputMsg newMsg ->
            ({ model | currentMsg = newMsg }, Cmd.none )

        ChatListSucceed r_chatList ->
            ( { model | chatList = r_chatList }, Cmd.none )

        MsgListSucceed r_MsgList ->
            (Debug.log <| toString r_MsgList)
                ( { model | currentHistory = r_MsgList }, Cmd.none )

        Fail err ->
            ( { model | status = toString err }, Cmd.none )

        GetUserChat user_id ->
            ( model, getMessageList user_id )


chatItem : ChatListItem -> Html Msg
chatItem { sender, avatar, body } =
    div [ onClick <| GetUserChat sender ] [ text sender ]


msgItem : MsgListItem -> Html Msg
msgItem { body, send_direction } =
    let
        placeLeft =
            [ ( "float", "left" ) ]

        placeRight =
            [ ( "float", "right" ) ]

        content config =
            div []
                [ div [ style config ] [ text body ]
                , br [] []
                ]
    in
        case send_direction of
            "client" ->
                content placeLeft

            "server" ->
                content placeRight

            _ ->
                content placeLeft


config : String -> String -> List ( String, String )
config align width =
    [ ( "float", align ), ( "width", width ), ( "height", "500px" ), ( "overflow", "auto" ) ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [ style <| config "left" "30%" ] <| List.map chatItem model.chatList
            , div [ style <| config "right" "70%" ] <| List.map msgItem model.currentHistory
            , div [ style [ ( "float", "right" ) ] ] [ input [ onInput InputMsg ] [], button [ onClick SendMsg ] [ text "Send" ] ]
            ]
        ]
