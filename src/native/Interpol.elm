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
    { interpol : Result String Delivery
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
    Model << Ok << ResponseString <| "init"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Return interpolated ->
            let
                interpolDecode =
                    De.decodeString responseDecoder interpolated
            in
                (Debug.log << toString <| interpolDecode)
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
            (at [ "response", "user_session" ] string)
        )


packageDecoder : String -> Decoder Delivery
packageDecoder event =
    case event of
        "get" ->
            authDecoder

        "post" ->
            stringDecoder

        _ ->
            De.fail "unknown_event"


responseDecoder : Decoder Delivery
responseDecoder =
    at [ "event" ] string `andThen` packageDecoder
