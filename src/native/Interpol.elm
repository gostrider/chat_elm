port module Interpol exposing (..)

import Json.Decode as De
    exposing
        ( Decoder
        , at
        , andThen
        , decodeString
        , int
        , list
        , string
        )


type alias Model =
    { interpol : Delivery
    }


type alias RespStr =
    String


type alias RespAuth =
    { id : String
    , uuid : String
    , session : String
    }


type Msg
    = Return String


type Delivery
    = ResponseString RespStr
    | ResponseAuthenticate RespAuth


port store : String -> Cmd msg


port reply : (String -> msg) -> Sub msg


port get : String -> Cmd msg


init : Model
init =
    Model << ResponseString <| "init"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Return interpolated ->
            let
                interpolDecode =
                    interpolated
                        |> De.decodeString responseDecoder
                        |> Result.withDefault (ResponseString "err")
            in
                ( { model | interpol = interpolDecode }, Cmd.none )


stringDecoder : Decoder Delivery
stringDecoder =
    De.object1 ResponseString
        (at [ "response" ] string)


authDecoder : Decoder Delivery
authDecoder =
    De.object1 ResponseAuthenticate
        (De.object3 RespAuth
            (at [ "response", "id" ] string)
            (at [ "response", "uuid" ] string)
            (at [ "response", "session" ] string)
        )


packageDecoder : String -> Decoder Delivery
packageDecoder event =
    case event of
        "get" ->
            authDecoder

        "post" ->
            stringDecoder

        "error" ->
            stringDecoder

        _ ->
            De.fail "unknown_event"


responseDecoder : Decoder Delivery
responseDecoder =
    at [ "event" ] string `andThen` packageDecoder


interpolLogin : Delivery -> RespAuth
interpolLogin delivery =
    case delivery of
        ResponseAuthenticate val ->
            val

        ResponseString _ ->
            RespAuth "" "" ""
