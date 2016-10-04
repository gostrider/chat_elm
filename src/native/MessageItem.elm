module MessageItem exposing (..)

import CSS exposing (floatLeft, floatRight, messagePanel)
import Html exposing (Html, br, div, text)
import Http exposing (Error, get)
import Json.Decode as De exposing (Decoder, at, andThen, string)
import Task exposing (Task, perform)


type alias MessageItem =
    { avatar : String
    , body : String
    , recipient : String
    , sender : String
    , send_direction : String
    }


type alias Model =
    { messages : List MessageItem
    }


type Msg
    = Succeed (List MessageItem)
    | Fail Error
    | RethinkChanges String



{- Application Callbacks -}


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

        RethinkChanges content ->
            let
                content' =
                    Result.withDefault
                        (MessageItem "" "decode failed" "" "" "")
                        (De.decodeString decodeChanges content)
            in
                (Debug.log <| toString content')
                    ( { model | messages = model.messages ++ [ content' ] }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ messagePanel ] <| List.map msgItem model.messages



{- Module Functions -}


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
            De.fail "unknown event"


decodeMsg : Decoder MessageItem
decodeMsg =
    De.object5 MessageItem
        (at [ "package", "new_val", "avatar" ] string)
        (at [ "package", "new_val", "body" ] string)
        (at [ "package", "new_val", "recipient" ] string)
        (at [ "package", "new_val", "sender" ] string)
        (at [ "package", "new_val", "send_direction" ] string)


decodeMsgList : Decoder (List MessageItem)
decodeMsgList =
    De.list <|
        De.object5 MessageItem
            (at [ "avatar" ] string)
            (at [ "body" ] string)
            (at [ "recipient" ] string)
            (at [ "sender" ] string)
            (at [ "send_direction" ] string)


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
