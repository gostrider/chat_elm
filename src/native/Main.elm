module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Http
import Json.Decode as De
import Json.Encode as En
import Task exposing (Task, perform)
import ChatItem


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    List ChatItem.ChatInfo


type Msg
    = FetchSucceed (List ChatItem.ChatInfo)
    | FetchFail Http.Error


decoder : De.Decoder (List ChatItem.ChatInfo)
decoder =
    De.list <|
        De.object3 ChatItem.ChatInfo
            (De.at [ "reduction", "sender" ] De.string)
            (De.at [ "reduction", "avatar" ] De.string)
            (De.at [ "reduction", "body" ] De.string)


getChatList : Task Http.Error (List ChatItem.ChatInfo)
getChatList =
    Http.get decoder "http://localhost:3000/get_userlist"


performT : Cmd Msg
performT =
    perform FetchFail FetchSucceed getChatList


init : ( Model, Cmd Msg )
init =
    ( [ ChatItem.ChatInfo "sneder" "url" "body" ], performT )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed chatList ->
            Debug.log (toString chatList)
                ( model, Cmd.none )

        FetchFail error ->
            Debug.log (toString error)
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text <| toString model
