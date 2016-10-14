port module Main exposing (..)

import ActionBar
import ChatItem
import CSS
import Login
import Json.Decode as De exposing (Decoder, at, andThen, fail, string)
import MessageItem
import Navigation
import Html.App as App
import Html exposing (Html, div)
import Router as R
import WebSocket as WS


port store : String -> Cmd msg


port reply : (String -> msg) -> Sub msg


port get : String -> Cmd msg


takeEffect : Decoder String
takeEffect =
    at [ "event" ] string
        `andThen` decodePackage


decodePackage : String -> Decoder String
decodePackage event =
    case event of
        "get" ->
            decodeResp

        "post" ->
            decodeResp

        _ ->
            fail "unknown event"


decodeResp : Decoder String
decodeResp =
    at [ "response" ] string


main : Program Never
main =
    Navigation.program R.parser
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
    , route : R.Route
    }


type Msg
    = UpdateLogin Login.Msg
    | UpdateChatList ChatItem.Msg
    | UpdateMsgList MessageItem.Msg
    | UpdateAction ActionBar.Msg
    | ReturnBack String


init : Result String R.Route -> ( Model, Cmd Msg )
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

        route =
            R.routeFromResult result
    in
        ( Model login chat message action route
        , get "key"
        )



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
                    ! [ Navigation.newUrl "main"
                      , Cmd.map UpdateLogin effectLogin
                      , ChatItem.getChatList login'.user.id
                            |> Cmd.map UpdateChatList
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
                      , MessageItem.getMessageList user_id model.login.user.id
                            |> Cmd.map UpdateMsgList
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
                      , ChatItem.getChatList model.login.user.id
                            |> Cmd.map UpdateChatList
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

        ReturnBack future ->
            let
                resp =
                    future |> De.decodeString takeEffect >> Result.withDefault "error"
            in
                (Debug.log resp)
                    ( model, Cmd.none )


urlUpdate : Result String R.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case R.routeFromResult result of
        (R.RouteMain) as unwrapRoute ->
            ( { model | route = unwrapRoute }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.route of
        R.RouteMain ->
            div []
                [ App.map UpdateChatList <| ChatItem.view model.chatList
                , div [ CSS.rightPanel ]
                    [ App.map UpdateMsgList <| MessageItem.view model.messageList
                    , App.map UpdateAction <| ActionBar.view model.action
                    ]
                ]

        _ ->
            App.map UpdateLogin <| Login.view model.login



-- case model.login.status of
--     "success" ->
--         -- TODO Refine CSS ordering
--         div []
--             [ App.map UpdateChatList <| ChatItem.view model.chatList
--             , div [ CSS.rightPanel ]
--                 [ App.map UpdateMsgList <| MessageItem.view model.messageList
--                 , App.map UpdateAction <| ActionBar.view model.action
--                 ]
--             ]
--
--     _ ->
--         App.map UpdateLogin <| Login.view model.login


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subMsgList =
            WS.listen "ws://localhost:4080" MessageItem.RethinkChanges
    in
        Sub.batch
            [ Sub.map UpdateMsgList subMsgList
            , reply ReturnBack
            ]
