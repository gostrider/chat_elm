module Controllers.MessageItem exposing (..)

import Utils.CSS exposing (floatLeft, floatRight, messagePanel)
import Html exposing (Html, br, div, text)
import Http exposing (Error, Request, get)
import Json.Decode as De exposing (Decoder, at, andThen, string)
import Task exposing (Task, perform)
import Models.Message exposing (Message, Messages, MessageUpdate(..))


init : Messages
init =
    Messages []


update : MessageUpdate -> Messages -> ( Messages, Cmd MessageUpdate )
update action model =
    case action of
        Succeed message_ ->
            ( { model | messages = message_ }, Cmd.none )

        Fail _ ->
            ( model, Cmd.none )

        RethinkChanges content ->
            let
                content_ =
                    Result.withDefault
                        (Message "" "decode failed" "" "" "")
                        (De.decodeString decodeChanges content)
            in
                --                ( { model | messages = model.messages ++ [ content_ ] }, Cmd.none )
                ( model, Cmd.none )


view : Messages -> Html MessageUpdate
view model =
    div [ messagePanel ] <| List.map msgItem model.messages



{- Module Functions -}


decodeChanges : Decoder Message
decodeChanges =
    at [ "event" ] string
        |> andThen decodePackage


decodePackage : String -> Decoder Message
decodePackage event =
    case event of
        "rethink-changes" ->
            decodeMsg

        _ ->
            De.fail "unknown event"


decodeMsg : Decoder Message
decodeMsg =
    De.map5 Message
        (at [ "package", "new_val", "avatar" ] string)
        (at [ "package", "new_val", "body" ] string)
        (at [ "package", "new_val", "recipient" ] string)
        (at [ "package", "new_val", "sender" ] string)
        (at [ "package", "new_val", "send_direction" ] string)


decodeMsgList : Decoder (List Message)
decodeMsgList =
    De.list <|
        De.map5 Message
            (at [ "avatar" ] string)
            (at [ "body" ] string)
            (at [ "recipient" ] string)
            (at [ "sender" ] string)
            (at [ "send_direction" ] string)


messageList : String -> String -> Request (List Message)
messageList send_from send_to =
    get
        ("http://localhost:3000/userchat?from="
            ++ send_from
            ++ "&to="
            ++ send_to
        )
        decodeMsgList


msgItem : Message -> Html MessageUpdate
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
