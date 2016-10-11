module Main exposing (..)

import ActionBar
import ChatItem
import CSS
import Login
import MessageItem
import Navigation
import Html.App as App
import Html exposing (Html, div)
import UrlParse
import WebSocket as WS


main : Program Never
main =
    Navigation.program UrlParse.urlParser
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
    }


type Msg
    = UpdateLogin Login.Msg
    | UpdateChatList ChatItem.Msg
    | UpdateMsgList MessageItem.Msg
    | UpdateAction ActionBar.Msg


init : String -> ( Model, Cmd Msg )
init result =
    urlUpdate result <|
        Model Login.init ChatItem.init MessageItem.init ActionBar.init



{- Sample auth:
   connected=028603668faa9786565cd4c32c7eab47; Path=/; HttpOnly
   "8428456081706861777154255320160518025200"
   "8774365768848866071800687020160905053839"
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin (Login.Succeed resp) ->
            let
                ( login', effectLogin ) =
                    Login.update (Login.Succeed resp) model.login
            in
                { model | login = login' }
                    ! [ Navigation.newUrl (UrlParse.toUrl "main")
                      , Cmd.map UpdateLogin effectLogin
                      , Cmd.map UpdateChatList <|
                            ChatItem.getChatList login'.user.id
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
                      , Cmd.map UpdateMsgList <|
                            MessageItem.getMessageList user_id model.login.user.id
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
                      , Cmd.map UpdateChatList <|
                            ChatItem.getChatList model.login.user.id
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


view : Model -> Html Msg
view model =
    case model.login.status of
        "success" ->
            -- TODO Refine CSS ordering
            div []
                [ App.map UpdateChatList <| ChatItem.view model.chatList
                , div [ CSS.rightPanel ]
                    [ App.map UpdateMsgList <| MessageItem.view model.messageList
                    , App.map UpdateAction <| ActionBar.view model.action
                    ]
                ]

        _ ->
            App.map UpdateLogin <| Login.view model.login


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subMsgList =
            WS.listen "ws://localhost:4080" MessageItem.RethinkChanges
    in
        Sub.map UpdateMsgList subMsgList


urlUpdate : String -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    ( model, Cmd.none )
