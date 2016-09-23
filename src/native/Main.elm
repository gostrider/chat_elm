module Model exposing (..)

import Html.App as App
import Html exposing (Html)
import ChatItem
import MessageItem
import Login


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { userStatus : Login.Model
    , currentMsg : String
    , chatList : ChatItem.Model
    }


type Msg
    = SendMsg
    | InputMsg String
    | UpdateChatList ChatItem.Msg
    | UpdateLogin Login.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( initLogin, cmdLogin ) =
            Login.init

        ( initChatList, cmdChatList ) =
            ChatItem.init
    in
        Model initLogin "" initChatList
            ! [ Cmd.map UpdateChatList cmdLogin
              , Cmd.map UpdateLogin cmdLogin
              ]


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

        UpdateLogin msgLogin ->
            let
                ( newLogin, effectLogin ) =
                    Login.update msgLogin model.userStatus
            in
                { model | userStatus = newLogin }
                    ! [ Cmd.map UpdateLogin effectLogin ]


view : Model -> Html Msg
view model =
    case model.userStatus.status of
        "succeed" ->
            App.map UpdateChatList <| ChatItem.view model.chatList

        "failed" ->
            App.map UpdateLogin <| Login.view model.userStatus

        _ ->
            ()
