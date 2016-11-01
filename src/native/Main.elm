port module Main exposing (..)

import ActionBar
import ChatItem
import CSS
import Interpol
import Login
import MessageItem
import Navigation
import Html.App as App
import Html exposing (Html, div)
import Json.Encode as En exposing (string)
import Router
import WebSocket


{- Sample auth:
   connected=028603668faa9786565cd4c32c7eab47; Path=/; HttpOnly
   "8428456081706861777154255320160518025200"
   "8774365768848866071800687020160905053839"
-}


main : Program Never
main =
    Navigation.program Router.parser
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
        UpdateLogin (Login.Succeed resp) ->
            let
                ( login', effectLogin ) =
                    Login.update (Login.Succeed resp) model.login

                encodeStore =
                    En.object
                        [ ( "id", string login'.user.id )
                        , ( "uuid", string login'.user.uuid )
                        , ( "session", string login'.user.session )
                        ]
            in
                { model | login = login' }
                    ! [ Interpol.store << En.encode 0 <| encodeStore
                      , Cmd.map UpdateLogin effectLogin
                      , Cmd.map UpdateChatList << ChatItem.getChatList <| login'.user.id
                      , Navigation.newUrl "#main"
                      ]

        UpdateLogin msgLogin ->
            let
                ( login', effectLogin ) =
                    Login.update msgLogin model.login
            in
                ( { model | login = login' }, Cmd.map UpdateLogin effectLogin )

        UpdateChatList (ChatItem.GetUserChat user_id) ->
            let
                ( chatList', effectChatList ) =
                    ChatItem.update (ChatItem.GetUserChat user_id) model.chatList
            in
                { model | chatList = chatList' }
                    ! [ Cmd.map UpdateChatList effectChatList
                      , Cmd.map UpdateMsgList << MessageItem.getMessageList user_id <| model.login.user.id
                      ]

        UpdateChatList msgChatList ->
            let
                ( chatList', effectChatList ) =
                    ChatItem.update msgChatList model.chatList
            in
                ( { model | chatList = chatList' }, Cmd.map UpdateChatList effectChatList )

        UpdateMsgList (MessageItem.Succeed messages') ->
            let
                context =
                    { send_from = model.login.user.id
                    , send_to = model.chatList.current
                    , session = model.login.user.session
                    }

                ( messageList', effectMsgList ) =
                    MessageItem.update (MessageItem.Succeed messages') model.messageList

                ( newAction, effectAction ) =
                    ActionBar.update context (ActionBar.Visible "visible") model.action
            in
                { model | messageList = messageList', action = newAction }
                    ! [ Cmd.map UpdateMsgList effectMsgList
                      , Cmd.map UpdateAction effectAction
                      ]

        UpdateMsgList (MessageItem.RethinkChanges changes) ->
            let
                ( messageList', effectMsgList ) =
                    MessageItem.update (MessageItem.RethinkChanges changes) model.messageList
            in
                { model | messageList = messageList' }
                    ! [ Cmd.map UpdateMsgList effectMsgList
                      , Cmd.map UpdateChatList << ChatItem.getChatList model.login.user.id
                      ]

        UpdateMsgList msgMsgList ->
            let
                ( newMsgList, effectMsgList ) =
                    MessageItem.update msgMsgList model.messageList
            in
                ( { model | messageList = newMsgList }, Cmd.map UpdateMsgList effectMsgList )

        UpdateAction msgAction ->
            let
                context =
                    { send_from = model.login.user.id
                    , send_to = model.chatList.current
                    , session = model.login.user.session
                    }

                ( newAction, effectAction ) =
                    ActionBar.update context msgAction model.action
            in
                ( { model | action = newAction }, Cmd.map UpdateAction effectAction )

        UpdatePort msgPort ->
            let
                ( newP, effectP ) =
                    Interpol.update msgPort model.interpol

                newLogin =
                    Interpol.interpolLogin newP.interpol
                        |> Login.Model "" "" "success"
            in
                ( { model | login = newLogin, interpol = newP }, Cmd.map UpdatePort effectP )


urlUpdate : Result String Router.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        unwrapRoute =
            Router.routeFromResult result
    in
        case unwrapRoute of
            unwrapRoute ->
                ( { model | route = unwrapRoute }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.login.status of
        "success" ->
            div []
                [ ChatItem.view model.chatList
                    |> App.map UpdateChatList
                , div [ CSS.rightPanel ]
                    [ MessageItem.view model.messageList
                        |> App.map UpdateMsgList
                    , ActionBar.view model.action
                        |> App.map UpdateAction
                    ]
                ]

        _ ->
            Login.view model.login
                |> App.map UpdateLogin


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
