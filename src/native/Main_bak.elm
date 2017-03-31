port module Main exposing (..)

import ActionBar
import ChatItem
import CSS
import Interpol
import Login
import MessageItem
import Navigation
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Router
import WebSocket


{- Sample auth:
   connected=session; Path=/; HttpOnly
   "user1"
   "user2"
-}


main : Program Never
main =
    Navigation.program
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


type alias Model =
    { login : Login.Model
    , chatList : ChatItem.Model
    , messageList : MessageItem.Model
    , action : ActionBar.Model
    , interpol : Interpol.Model
    , route : Router.Route
    }


type Msg
    = UpdateLogin Login.Msg
    | UpdateChatList ChatItem.Msg
    | UpdateMsgList MessageItem.Msg
    | UpdateAction ActionBar.Msg
    | UpdatePort Interpol.Msg


init : Result String Router.Route -> ( Model, Cmd Msg )
init result =
    let
        login =
            Login.init

        chat =
            ChatItem.init

        message =
            MessageItem.init

        action =
            ActionBar.init

        interpol =
            Interpol.init

        route =
            Router.routeFromResult result
    in
        ( Model login chat message action interpol route
        , Interpol.get "user_auth"
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin msgLogin ->
            let
                ( login_, effectLogin ) =
                    Login.update msgLogin model.login

                chatListCmd =
                    if login_.status == "succeed" then
                        ChatItem.getChatList login_.user.id
                    else
                        Cmd.none
            in
                ( { model | login = login_ }
                , Cmd.batch
                    [ Cmd.map UpdateLogin effectLogin
                    , Cmd.map UpdateChatList chatListCmd
                    ]
                )

        UpdateChatList msgChatList ->
            let
                ( chatList_, effectChatList ) =
                    ChatItem.update msgChatList model.chatList

                chatListCmd =
                    case msgChatList of
                        ChatItem.GetUserChat user_id ->
                            Cmd.map UpdateMsgList
                                (MessageItem.getMessageList user_id model.login.user.id)

                        _ ->
                            Cmd.none
            in
                ( { model | chatList = chatList_ }
                , Cmd.batch
                    [ Cmd.map UpdateChatList effectChatList
                    , chatListCmd
                    ]
                )

        UpdateMsgList msgMsgList ->
            let
                ( newMsgList, effectMsgList ) =
                    MessageItem.update
                        msgMsgList
                        model.messageList

                msgListCmd =
                    case msgMsgList of
                        MessageItem.RethinkChanges changes ->
                            Cmd.map UpdateChatList
                                (ChatItem.getChatList model.login.user.id)

                        _ ->
                            Cmd.none

                ( newAction, effectAction ) =
                    case msgMsgList of
                        MessageItem.Succeed messages_ ->
                            ActionBar.update
                                (context model)
                                (ActionBar.Visible "visible")
                                model.action

                        _ ->
                            ( model.action, Cmd.none )
            in
                (Debug.log << toString <| model.action)
                    ( { model | action = newAction, messageList = newMsgList }
                    , Cmd.batch
                        [ Cmd.map UpdateMsgList effectMsgList
                        , Cmd.map UpdateAction effectAction
                        , msgListCmd
                        ]
                    )

        UpdateAction msgAction ->
            let
                ( newAction, effectAction ) =
                    ActionBar.update (context model) msgAction model.action

                sendCmd =
                    case msgAction of
                        ActionBar.Send ->
                            Cmd.map UpdateChatList
                                (ChatItem.getChatList model.login.user.id)

                        _ ->
                            Cmd.none
            in
                ( { model | action = newAction }
                , Cmd.batch
                    [ Cmd.map UpdateAction effectAction
                    , sendCmd
                    ]
                )

        UpdatePort msgPort ->
            -- Handle session expired
            let
                ( newPort, effectPort ) =
                    Interpol.update msgPort model.interpol

                ( status, login_ ) =
                    Interpol.interpolLogin newPort.interpol

                ( newRoute, chatListCmd ) =
                    if status == "succeed" then
                        ( "#main", ChatItem.getChatList login_.id )
                    else
                        ( "#login", Cmd.none )
            in
                ( { model | login = Login.makeLogin status login_, interpol = newPort }
                , Cmd.batch
                    [ Cmd.map UpdatePort effectPort
                    , Cmd.map UpdateChatList chatListCmd
                    , Navigation.newUrl newRoute
                    ]
                )


urlUpdate : Result String Router.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        unwrapRoute =
            Router.routeFromResult result
    in
        ( { model | route = unwrapRoute }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.login.status == "succeed" && model.route == Router.RouteMain of
        True ->
            div []
                [ Html.map UpdateChatList (ChatItem.view model.chatList)
                , div [ CSS.rightPanel ]
                    [ Html.map UpdateMsgList (MessageItem.view model.messageList)
                    , Html.map UpdateAction (ActionBar.view model.action)
                    ]
                ]

        False ->
            Html.map UpdateLogin (Login.view model.login)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subMsgList =
            WebSocket.listen "ws://localhost:4080" MessageItem.RethinkChanges
    in
        Sub.batch
            [ Sub.map UpdateMsgList subMsgList
            , Sub.map UpdatePort (Interpol.reply Interpol.Return)
            ]


context : Model -> { send_from : String, send_to : String, session : String }
context model =
    { send_from = model.login.user.id
    , send_to = model.chatList.current
    , session = model.login.user.session
    }
