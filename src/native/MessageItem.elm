module MessageItem exposing (..)

import CSS exposing (floatLeft, floatRight, messagePanel)
import Html exposing (Html, br, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, string, list, object5)
import Task exposing (Task, perform)


type alias MessageItem =
    { sender : String
    , recipient : String
    , body : String
    , send_direction : String
    , avatar : String
    }


type alias Model =
    { messages : List MessageItem
    }


type Msg
    = Succeed (List MessageItem)
    | Fail Error


init : Model
init =
    Model []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Succeed message' ->
            ( { model | messages = message' }, Cmd.none )

        Fail _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ messagePanel ] <| List.map msgItem model.messages


decodeMsgList : Decoder (List MessageItem)
decodeMsgList =
    list <|
        object5 MessageItem
            (at [ "sender" ] string)
            (at [ "recipient" ] string)
            (at [ "body" ] string)
            (at [ "send_direction" ] string)
            (at [ "avatar" ] string)


messageList : String -> String -> Task Error (List MessageItem)
messageList send_from send_to =
    -- TODO Failed to retrieve user messages
    get decodeMsgList <|
        "http://localhost:3000/get_userchat?from="
            ++ send_from
            ++ "&to="
            ++ send_to


getMessageList : String -> String -> Cmd Msg
getMessageList send_from send_to =
    perform Fail Succeed <| messageList send_from send_to


msgItem : MessageItem -> Html Msg
msgItem { body, send_direction } =
    let
        content config =
            div []
                [ div [ config ] [ text body ]
                , br [] []
                ]
    in
        case send_direction of
            "client" ->
                content floatLeft

            "server" ->
                content floatRight

            _ ->
                content floatLeft
