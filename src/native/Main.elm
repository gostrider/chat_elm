port module Main exposing (..)

import ActionBar
import ChatItem
import CSS
import Interpol
import Login
import MessageItem
import Navigation
import Html.App as App
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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
    , chatList :
        ChatItem.Model
        --    , messageList : MessageItem.Model
        --    , action : ActionBar.Model
    , interpol : Interpol.Model
    , route : Router.Route
    }


type Msg
    = UpdateLogin Login.Msg
    | UpdateChatList ChatItem.Msg
      --    | UpdateMsgList MessageItem.Msg
      --    | UpdateAction ActionBar.Msg
    | UpdatePort Interpol.Msg



--    | Logout


init : Result String Router.Route -> ( Model, Cmd Msg )
init result =
    let
        login =
            Login.init

        chat =
            ChatItem.init

        --        message =
        --            MessageItem.init
        --        action =
        --            ActionBar.init
        interpol =
            Interpol.init

        route =
            Router.routeFromResult result
    in
        ( Model login chat interpol route
        , Interpol.get "user_auth"
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin msgLogin ->
            let
                ( login', effectLogin ) =
                    Login.update msgLogin model.login
            in
                (Debug.log << toString <| "in login")
                    ( { model | login = login' }
                    , Cmd.batch
                        [ Cmd.map UpdateLogin effectLogin
                        , Cmd.map UpdateChatList (ChatItem.getChatList login'.user.id)
                        ]
                    )

        UpdateChatList msgChatList ->
            let
                ( chatList', effectChatList ) =
                    ChatItem.update msgChatList model.chatList

                --                chatListCmd =
                --                    case msgChatList of
                --                        ChatItem.GetUserChat user_id ->
                --                            [ Cmd.map UpdateMsgList <|
                --                                MessageItem.getMessageList user_id model.login.user.id
                --                            ]
                --
                --                        _ ->
                --                            [ Cmd.none ]
            in
                (Debug.log << toString <| "in chat list")
                    ( { model | chatList = chatList' }
                    , Cmd.map UpdateChatList effectChatList
                    )

        --        UpdateMsgList msgMsgList ->
        --            let
        --                ( newMsgList, effectMsgList ) =
        --                    MessageItem.update msgMsgList model.messageList
        --
        --                msgListCmd =
        --                    case msgMsgList of
        --                        MessageItem.RethinkChanges changes ->
        --                            [ Cmd.map UpdateChatList <|
        --                                ChatItem.getChatList model.login.user.id
        --                            ]
        --
        --                        _ ->
        --                            [ Cmd.none ]
        --
        --                ( newAction, effectAction ) =
        --                    case msgMsgList of
        --                        MessageItem.Succeed messages' ->
        --                            ActionBar.update (context model) (ActionBar.Visible "visible") model.action
        --
        --                        _ ->
        --                            ( model.action, Cmd.none )
        --            in
        --                { model | messageList = newMsgList }
        --                    ! ([ Cmd.map UpdateMsgList effectMsgList
        --                       ]
        --                      )
        --        UpdateAction msgAction ->
        --            model ! []
        --            let
        --                ( newAction, effectAction ) =
        --                    ActionBar.update (context model) msgAction model.action
        --            in
        --                { model | login = Login.init, action = newAction }
        --                    ! [ Cmd.map UpdateAction effectAction
        --                      , Navigation.newUrl "#main"
        --                      ]
        UpdatePort msgPort ->
            let
                ( newPort, effectPort ) =
                    Interpol.update msgPort model.interpol

                ( newLogin, effectLogin ) =
                    let
                        ( status, login' ) =
                            Interpol.interpolLogin newPort.interpol

                        validRoute =
                            if status == "succeed" then
                                "#main"
                            else
                                "#login"
                    in
                        ( Login.makeLogin status login', Navigation.newUrl validRoute )
            in
                (Debug.log << toString <| "in interpol")
                    ( { model | login = newLogin, interpol = newPort }
                    , Cmd.batch
                        [ Cmd.map UpdatePort effectPort
                        , effectLogin
                        ]
                    )



--        Logout ->
--            { model | login = Login.init }
--                ! [ Interpol.remove "user_auth"
--                  , Navigation.newUrl "#login"
--                  ]


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
    case model.login.status == "succeed" && model.route == Router.RouteMain of
        True ->
            div []
                [ App.map UpdateChatList (ChatItem.view model.chatList)
                  --                , div [ CSS.rightPanel ]
                  --                    [ App.map UpdateMsgList <|
                  --                        MessageItem.view model.messageList
                  --                    , App.map UpdateAction <|
                  --                        ActionBar.view model.action
                  --                    ]
                ]

        False ->
            App.map UpdateLogin (Login.view model.login)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subMsgList =
            WebSocket.listen "ws://localhost:4080" MessageItem.RethinkChanges
    in
        Sub.map UpdatePort (Interpol.reply Interpol.Return)



--        Sub.batch
--            [ Sub.map UpdateMsgList subMsgList
--            , Sub.map UpdatePort (Interpol.reply Interpol.Return)
--            ]
--context : Model -> { send_from : String, send_to : String, session : String }
--context model =
--    { send_from = model.login.user.id
--    , send_to = model.chatList.current
--    , session = model.login.user.session
--    }
