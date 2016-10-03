module MessageItem exposing (..)

import CSS exposing (floatLeft, floatRight, messagePanel)
import Html exposing (Html, br, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Http exposing (Error, get)
import Json.Decode as De exposing (Decoder, at, andThen, decodeString, string, list, object5)
import Task exposing (Task, perform)
import WebSocket as WS


--main =
--    App.program
--        { init = init
--        , view = view
--        , update = update
--        , subscriptions = subscriptions
--        }


type alias MessageItem =
    { sender : String
    , recipient : String
    , body : String
    , send_direction : String
    , avatar : String
    }


type alias NewItem =
    { event : String
    , package : MessageItem
    }


type alias Model =
    { messages : List MessageItem
    }


type Msg
    = Succeed (List MessageItem)
    | Fail Error
    | RethinkChanges String



-- App Callbacks


init : Model
init =
    Model []
--     connected=028603668faa9786565cd4c32c7eab47; Path=/; HttpOnly
--    ( Model [], getMessageList "8428456081706861777154255320160518025200" "8774365768848866071800687020160905053839" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Succeed message' ->
            ( { model | messages = message' }, Cmd.none )

        Fail _ ->
            ( model, Cmd.none )

        RethinkChanges content ->
            let
                strContent =
                    toString <| De.decodeString decodeChanges content
            in
                (Debug.log strContent)
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ messagePanel ] <| List.map msgItem model.messages


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen "ws://localhost:4080" RethinkChanges



-- Module Functions


decodeChanges : Decoder MessageItem
decodeChanges =
    at [ "event" ] string
        `andThen` decodePackage


decodePackage : String -> Decoder MessageItem
decodePackage event =
    case event of
        "rethink-changes" ->
            decodeMsg

        _ ->
            De.fail "unknow_event"


decodeMsg : Decoder MessageItem
decodeMsg =
    object5 MessageItem
        (at [ "package", "new_val", "id" ] string)
        (at [ "package", "new_val", "sender" ] string)
        (at [ "package", "new_val", "recipient" ] string)
        (at [ "package", "new_val", "body" ] string)
        (at [ "package", "new_val", "avatar" ] string)


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
    get decodeMsgList <|
        "http://localhost:3000/get_userchat?from="
            ++ send_from
            ++ "&to="
            ++ send_to


getMessageList : String -> String -> Cmd Msg
getMessageList send_from send_to =
    -- TODO Verify client server position
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
